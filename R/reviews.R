# =============================================================================
# R/reviews.R
# Review & Comment System – Helper Functions
# =============================================================================
# Purpose : CRUD operations for TFL review comments, audit summary, and
#           Excel compilation of all reviews across TFLs.
#
# Storage : One JSON file per TFL in the reviews/ directory.
#           Each file is an array of comment objects.
#           Threading is achieved via parent_id linkage.
# =============================================================================


# -----------------------------------------------------------------------------
# is_top_level_comment()
# Robustly check whether a parent_id value means "no parent" (top-level).
# Handles NULL, list(), "", NA — all edge cases from JSON round-tripping.
# -----------------------------------------------------------------------------
is_top_level_comment <- function(parent_id) {
  if (is.null(parent_id)) return(TRUE)
  if (length(parent_id) == 0) return(TRUE)
  if (is.list(parent_id)) return(TRUE)
  if (is.na(parent_id)) return(TRUE)
  if (identical(parent_id, "")) return(TRUE)
  FALSE
}


# -----------------------------------------------------------------------------
# build_review_id()
# Generate a unique review/comment ID from timestamp + random suffix.
# -----------------------------------------------------------------------------
build_review_id <- function() {
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  rand <- paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  paste0("rev-", ts, "-", rand)
}


# -----------------------------------------------------------------------------
# load_reviews()
# Read all comments for a given TFL from its JSON file.
# Returns a list of comment objects (empty list if no file exists).
# -----------------------------------------------------------------------------
load_reviews <- function(tfl_key, reviews_dir = "reviews") {
  path <- file.path(reviews_dir, paste0(tfl_key, ".json"))
  if (!file.exists(path)) return(list())
  tryCatch(
    jsonlite::fromJSON(path, simplifyDataFrame = FALSE),
    error = function(e) {
      warning("Could not read reviews for ", tfl_key, ": ", e$message)
      list()
    }
  )
}


# -----------------------------------------------------------------------------
# save_review()
# Append a single comment object to the TFL's JSON file.
# Creates the file if it doesn't exist.
#
# @param tfl_key     Character. The TFL key (e.g. "tfl_t14_1_1").
# @param comment     List with fields: id, tfl_key, reviewer, role,
#                    timestamp, text, parent_id.
# @param reviews_dir Character. Path to the reviews directory.
# -----------------------------------------------------------------------------
save_review <- function(tfl_key, comment, reviews_dir = "reviews") {
  dir.create(reviews_dir, showWarnings = FALSE, recursive = TRUE)
  existing <- load_reviews(tfl_key, reviews_dir)
  existing <- c(existing, list(comment))
  path <- file.path(reviews_dir, paste0(tfl_key, ".json"))
  jsonlite::write_json(existing, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(comment)
}


# -----------------------------------------------------------------------------
# get_audit_summary()
# Build a data frame with one row per TFL and columns for each reviewer role,
# showing whether that role has at least one comment.
#
# @param tfl_registry Data frame with columns tfl_key, name.
# @param reviewer_roles Character vector of role names.
# @param reviews_dir Character. Path to the reviews directory.
#
# @return Data frame: TFL_ID, TFL_Name, then one column per role with values
#         like "Jane S. (2026-03-18)" or "" (empty = not reviewed).
# -----------------------------------------------------------------------------
get_audit_summary <- function(tfl_registry, reviewer_roles, reviews_dir = "reviews") {
  rows <- lapply(seq_len(nrow(tfl_registry)), function(i) {
    key  <- tfl_registry$tfl_key[i]
    name <- tfl_registry$name[i]
    revs <- load_reviews(key, reviews_dir)

    role_status <- setNames(
      rep("", length(reviewer_roles)),
      reviewer_roles
    )

    for (rv in revs) {
      r <- rv$role
      if (!is.null(r) && r %in% reviewer_roles && role_status[[r]] == "") {
        ts <- if (!is.null(rv$timestamp))
          substr(rv$timestamp, 1, 10) else ""
        role_status[[r]] <- paste0(rv$reviewer, " (", ts, ")")
      }
    }

    c(TFL_ID = key, TFL_Name = name, role_status)
  })

  do.call(rbind, lapply(rows, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)))
}


# -----------------------------------------------------------------------------
# get_tfl_review_status()
# For a single TFL, return which roles have reviewed.
# Returns a named logical vector (role -> TRUE/FALSE).
# -----------------------------------------------------------------------------
get_tfl_review_status <- function(tfl_key, reviewer_roles, reviews_dir = "reviews") {
  revs <- load_reviews(tfl_key, reviews_dir)
  roles_present <- unique(vapply(revs, function(r) r$role %||% "", character(1)))
  setNames(reviewer_roles %in% roles_present, reviewer_roles)
}


# -----------------------------------------------------------------------------
# compile_reviews_excel()
# Build a two-sheet Excel workbook:
#   Sheet 1 "Review Log"    – one row per comment/response across all TFLs
#   Sheet 2 "Audit Summary" – one row per TFL, one column per role
#
# @param tfl_registry   Data frame with tfl_key and name columns.
# @param reviewer_roles Character vector of role names.
# @param reviews_dir    Character. Path to reviews directory.
# @param output_file    Character. Path for the output .xlsx file.
# -----------------------------------------------------------------------------
compile_reviews_excel <- function(tfl_registry, reviewer_roles,
                                  reviews_dir = "reviews",
                                  output_file = "review_audit.xlsx") {

  # --- Sheet 1: Review Log ---
  all_comments <- list()

  for (i in seq_len(nrow(tfl_registry))) {
    key  <- tfl_registry$tfl_key[i]
    name <- tfl_registry$name[i]
    revs <- load_reviews(key, reviews_dir)

    # Build lookup: comment id -> reviewer name (for Response_To column)
    id_to_reviewer <- list()
    for (rv in revs) {
      if (!is.null(rv$id)) id_to_reviewer[[rv$id]] <- rv$reviewer %||% ""
    }

    for (rv in revs) {
      # Determine thread_id: for top-level comments it's their own id,
      # for replies it's the parent_id
      pid <- rv$parent_id
      is_response <- !is_top_level_comment(pid)
      thread_id <- if (is_response) pid else rv$id

      # Resolve who this response is directed to
      response_to <- if (is_response) (id_to_reviewer[[pid]] %||% "") else ""

      all_comments[[length(all_comments) + 1L]] <- data.frame(
        TFL_ID      = key,
        TFL_Name    = name,
        Thread_ID   = thread_id,
        Comment_ID  = rv$id %||% "",
        Parent_ID   = if (is_response) pid else "",
        Type        = if (is_response) "Response" else "Comment",
        Reviewer    = rv$reviewer %||% "",
        Role        = rv$role %||% "",
        Timestamp   = rv$timestamp %||% "",
        Comment     = rv$text %||% "",
        Response_To = response_to,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(all_comments) > 0) {
    log_df <- do.call(rbind, all_comments)
    # Sort by TFL, thread, then timestamp so threads are grouped
    log_df <- log_df[order(log_df$TFL_ID, log_df$Thread_ID, log_df$Timestamp), ]
  } else {
    log_df <- data.frame(
      TFL_ID = character(), TFL_Name = character(), Thread_ID = character(),
      Comment_ID = character(), Parent_ID = character(), Type = character(),
      Reviewer = character(), Role = character(), Timestamp = character(),
      Comment = character(), Response_To = character(), stringsAsFactors = FALSE
    )
  }

  # --- Sheet 2: Audit Summary ---
  audit_df <- get_audit_summary(tfl_registry, reviewer_roles, reviews_dir)

  # --- Write ---
  writexl::write_xlsx(
    list("Review Log" = log_df, "Audit Summary" = audit_df),
    path = output_file
  )

  invisible(output_file)
}
