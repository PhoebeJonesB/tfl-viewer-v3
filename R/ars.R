# =============================================================================
# R/ars.R
# ARS (Analysis Results Standard) JSON Builder
# =============================================================================
# Purpose : Build ARS-compliant R list objects from TFL results + user session
#           state, and serialise them to JSON.
#
# References:
#   CDISC ARS Standard : https://www.cdisc.org/standards/foundational/ars
#   ARS GitHub Spec    : https://github.com/cdisc-org/analysis-results-standard
#
# Key functions:
#   build_ars_json()    – assemble the ARS object from a TFL result + filters
#   ars_to_json_string()– serialise to pretty-printed JSON string
#   write_ars_json()    – write JSON to a file in ars_outputs/
# =============================================================================


# -----------------------------------------------------------------------------
# build_ars_json()
# Assemble an ARS-compliant list object for a given TFL result and session.
# -----------------------------------------------------------------------------
#' Build an ARS reporting event list from a TFL result
#'
#' @param tfl_result    A single element from tfl_list (output of run_tfl_script).
#' @param active_filters Named list of active population flag selections from
#'        the sidebar, e.g. list(SAFFL = "Y", ITTFL = "All").
#'        Values equal to "All" or NULL are treated as no filter applied.
#' @param study_id      Study identifier string (e.g. "STUDY001").
#' @param timestamp     POSIXct timestamp (defaults to Sys.time()).
#'
#' @return A named list representing a CDISC ARS ReportingEvent, suitable for
#'         serialisation with ars_to_json_string().
build_ars_json <- function(tfl_result,
                           active_filters = list(),
                           study_id       = "STUDY001",
                           timestamp      = Sys.time()) {

  m <- tfl_result$metadata

  # ── Helper: normalise a where-clause condition to a list ─────────────────
  make_condition <- function(dataset, variable, comparator, value) {
    list(
      dataset    = dataset,
      variable   = variable,
      comparator = comparator,
      value      = if (length(value) == 1) value else as.list(value)
    )
  }

  # ── 1. Analysis sets from script metadata ────────────────────────────────
  script_sets <- if (!is.null(m$analysis_sets) && length(m$analysis_sets) > 0) {
    lapply(m$analysis_sets, function(as_item) {
      list(
        id        = as_item$id,
        label     = as_item$label,
        condition = make_condition(
          as_item$dataset,
          as_item$variable,
          as_item$comparator,
          as_item$value
        )
      )
    })
  } else {
    list()
  }

  # ── 2. User-applied filters → additional analysis sets ───────────────────
  user_sets <- list()
  user_filter_log <- list()

  active_filters_clean <- Filter(
    function(v) !is.null(v) && v != "All",
    active_filters
  )

  for (fl in names(active_filters_clean)) {
    val <- active_filters_clean[[fl]]

    # Only add if not already declared in script metadata
    already_declared <- any(sapply(script_sets, function(s) {
      !is.null(s$condition) &&
        s$condition$variable == fl &&
        s$condition$value    == val
    }))

    if (!already_declared) {
      set_id <- paste0("AS-USER-", fl)
      user_sets <- c(user_sets, list(list(
        id        = set_id,
        label     = paste0("User-applied: ", fl, " = ", val),
        condition = make_condition("adsl", fl, "EQ", val)
      )))
    }

    user_filter_log <- c(user_filter_log, list(list(
      variable   = fl,
      comparator = "EQ",
      value      = val
    )))
  }

  all_analysis_sets <- c(script_sets, user_sets)

  # ── 3. Data subsets from metadata ─────────────────────────────────────────
  data_subsets <- if (!is.null(m$data_subsets) && length(m$data_subsets) > 0) {
    lapply(m$data_subsets, function(ds_item) {
      list(
        id        = ds_item$id,
        label     = ds_item$label,
        condition = make_condition(
          ds_item$dataset,
          ds_item$variable,
          ds_item$comparator,
          ds_item$value
        )
      )
    })
  } else {
    list()
  }

  # ── 4. Display (titles / footnotes) ──────────────────────────────────────
  display_title    <- m$display$title    %||% m$name
  display_subtitle <- m$display$subtitle %||% ""
  footnotes        <- m$display$footnotes %||% character(0)

  display_obj <- list(
    id             = paste0("DISP-", m$id),
    displayTitle   = display_title,
    displaySubtitle = display_subtitle,
    footnotes      = as.list(footnotes)
  )

  # ── 5. Output object ──────────────────────────────────────────────────────
  output_obj <- list(
    id               = paste0("OUT-", m$id),
    name             = m$name,
    outputType       = m$type,
    fileSpecifications = list(list(
      name     = paste0("tfl_", m$id, ".R"),
      fileType = "R Script"
    )),
    displays         = list(display_obj),
    analyses         = if (!is.null(m$analyses) && length(m$analyses) > 0)
      lapply(m$analyses, function(a) a$id)
    else
      list()
  )

  # ── 6. Analyses from metadata ─────────────────────────────────────────────
  analyses <- if (!is.null(m$analyses) && length(m$analyses) > 0) {
    lapply(m$analyses, function(a) {
      list(
        id             = a$id,
        name           = a$label,
        analysisSetId  = a$analysis_set_id,
        dataset        = a$dataset,
        variable       = a$variable,
        groupingVariable = a$grouping_var,
        methodId       = a$method_id
      )
    })
  } else {
    list()
  }

  # ── 7. Methods from metadata ──────────────────────────────────────────────
  methods <- if (!is.null(m$methods) && length(m$methods) > 0) {
    lapply(m$methods, function(mth) {
      list(
        id         = mth$id,
        name       = mth$label,
        operations = lapply(mth$operations, function(op) list(name = op))
      )
    })
  } else {
    list()
  }

  # ── 8. User session provenance block ──────────────────────────────────────
  user_session <- list(
    baseTflId      = m$id,
    variantOf      = m$variant_of %||% NULL,
    generatedAt    = format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    appliedFilters = user_filter_log
  )

  # ── 9. Assemble reporting event ───────────────────────────────────────────
  list(
    reportingEvent = list(
      id           = paste0("RE-", study_id),
      name         = paste(study_id, "Analysis Results"),
      version      = "3.0.0",
      generatedAt  = format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      outputs      = list(output_obj),
      analyses     = analyses,
      analysisSets = all_analysis_sets,
      dataSubsets  = data_subsets,
      methods      = methods,
      userSession  = user_session
    )
  )
}


# -----------------------------------------------------------------------------
# ars_to_json_string()
# Serialise an ARS list object to a pretty-printed JSON string.
# -----------------------------------------------------------------------------
#' Serialise an ARS object to JSON
#'
#' @param ars_obj Named list returned by build_ars_json().
#' @return Character string containing formatted JSON.
ars_to_json_string <- function(ars_obj) {
  jsonlite::toJSON(ars_obj, pretty = TRUE, auto_unbox = TRUE, null = "null")
}


# -----------------------------------------------------------------------------
# write_ars_json()
# Write an ARS JSON file to the ars_outputs/ directory.
# -----------------------------------------------------------------------------
#' Write an ARS JSON object to disk
#'
#' @param ars_obj   Named list returned by build_ars_json().
#' @param output_dir Path to the output directory (default: "ars_outputs").
#' @param tfl_id    TFL identifier string used to name the file.
#' @param suffix    Optional suffix appended before .json (e.g. timestamp).
#'
#' @return Invisibly returns the full file path written.
write_ars_json <- function(ars_obj,
                           output_dir = "ars_outputs",
                           tfl_id     = "unknown",
                           suffix     = NULL) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  fname <- if (!is.null(suffix))
    paste0(tfl_id, "_", suffix, ".json")
  else
    paste0(tfl_id, ".json")

  fpath <- file.path(output_dir, fname)
  writeLines(ars_to_json_string(ars_obj), fpath)

  message(sprintf("[ars] Wrote ARS JSON: %s", fpath))
  invisible(fpath)
}


# -----------------------------------------------------------------------------
# next_variant_suffix()
# Suggest the next alphabetic variant label for a given base TFL id.
# -----------------------------------------------------------------------------
#' Suggest the next available variant suffix (a, b, c, ...)
#'
#' @param base_id  Base TFL id string, e.g. "t14_1_1".
#' @param tfl_dir  Path to the tfls/ directory.
#'
#' @return Single character string (e.g. "a", "b", "c").
next_variant_suffix <- function(base_id, tfl_dir = "tfls") {
  pattern  <- paste0("^tfl_", base_id, "_[a-z]\\.R$")
  existing <- list.files(tfl_dir, pattern = pattern)
  if (length(existing) == 0L) return("a")
  used   <- sub(paste0(".*", base_id, "_([a-z])\\.R$"), "\\1", existing)
  used_i <- match(used, letters)
  letters[max(used_i, na.rm = TRUE) + 1L]
}


# -----------------------------------------------------------------------------
# generate_variant_script()
# Create a new TFL .R script that is a variant of a base TFL, with baked-in
# analysis_sets derived from the user's active filter selections.
# -----------------------------------------------------------------------------
#' Generate and write a variant TFL script
#'
#' @param base_result     A tfl_list element (output of run_tfl_script).
#' @param variant_id      New TFL id string, e.g. "t14_1_1_a".
#' @param variant_label   Human-readable label for the variant.
#' @param variant_desc    Description text.
#' @param baked_filters   Named list of filters to bake in, e.g. list(SAFFL = "Y").
#' @param tfl_dir         Path to the tfls/ directory.
#'
#' @return Invisibly returns the path to the written script.
generate_variant_script <- function(base_result,
                                    variant_id,
                                    variant_label,
                                    variant_desc,
                                    baked_filters,
                                    tfl_dir = "tfls") {

  m       <- base_result$metadata
  lines   <- base_result$script_lines

  # Build the new analysis_sets by merging base sets with baked_filters
  base_sets <- m$analysis_sets %||% list()

  new_sets <- base_sets
  for (fl in names(baked_filters)) {
    val <- baked_filters[[fl]]
    if (is.null(val) || val == "All") next
    # Only add if not already in the base sets
    already <- any(sapply(base_sets, function(s) {
      !is.null(s) && isTRUE(s$variable == fl) && isTRUE(s$value == val)
    }))
    if (!already) {
      new_sets <- c(new_sets, list(list(
        id         = paste0("AS-", toupper(fl)),
        label      = paste0(fl, " = ", val),
        dataset    = "adsl",
        variable   = fl,
        comparator = "EQ",
        value      = val
      )))
    }
  }

  # Serialise analysis_sets to R code for embedding in the script
  sets_code <- if (length(new_sets) > 0) {
    sets_str <- lapply(new_sets, function(s) {
      sprintf(
        '    list(\n      id         = "%s",\n      label      = "%s",\n      dataset    = "%s",\n      variable   = "%s",\n      comparator = "%s",\n      value      = "%s"\n    )',
        s$id, s$label, s$dataset, s$variable, s$comparator, s$value
      )
    })
    paste0("  analysis_sets = list(\n", paste(sets_str, collapse = ",\n"), "\n  ),")
  } else {
    "  analysis_sets = NULL,"
  }

  # Build the display block, inheriting from base if present
  base_display <- m$display %||% list()
  disp_title    <- base_display$title    %||% m$name
  disp_subtitle <- variant_label
  disp_footnotes <- base_display$footnotes %||% character(0)
  fn_str <- if (length(disp_footnotes) > 0)
    paste0('    c(\n', paste0('      "', disp_footnotes, '"', collapse = ',\n'), '\n    )')
  else
    'character(0)'

  # Format variant_of as a list element
  variant_of_str <- sprintf('  variant_of     = "%s",', m$id)

  # Construct the new metadata block
  new_meta <- sprintf(
'# -- SECTION 1: METADATA (auto-generated variant) ---------------------------

tfl_metadata <- list(
  id          = "%s",
  name        = "%s \u2013 %s",
  type        = "%s",
  datasets    = c(%s),
  description = "%s",
  variant_of  = "%s",

  display = list(
    title     = "%s",
    subtitle  = "%s",
    footnotes = %s
  ),

%s

  analyses = %s,
  methods  = %s
)',
    variant_id,
    m$id, variant_label,
    m$type,
    paste0('"', m$datasets, '"', collapse = ", "),
    variant_desc,
    m$id,
    disp_title, disp_subtitle, fn_str,
    sets_code,
    if (!is.null(m$analyses)) deparse(m$analyses, control = "keepInteger") else "NULL",
    if (!is.null(m$methods))  deparse(m$methods,  control = "keepInteger") else "NULL"
  )

  # Find the bounds of the original metadata block in the script
  # Look for the line starting the metadata list and the line of the closing )
  meta_start <- which(grepl("^tfl_metadata\\s*<-\\s*list\\(", lines))
  if (length(meta_start) == 0L) meta_start <- 1L else meta_start <- meta_start[1L]

  # Find the matching closing ) by counting parens from meta_start
  depth    <- 0L
  meta_end <- meta_start
  for (i in seq(meta_start, length(lines))) {
    depth <- depth +
      stringr::str_count(lines[i], "\\(") -
      stringr::str_count(lines[i], "\\)")
    if (depth <= 0L) { meta_end <- i; break }
  }

  # Replace the metadata block lines with the new block
  new_lines <- c(
    paste0(
      "# =============================================================================\n",
      "# tfl_", variant_id, ".R\n",
      "# Auto-generated variant of tfl_", m$id, ".R\n",
      "# Base TFL  : ", m$name, "\n",
      "# Variant   : ", variant_label, "\n",
      "# Generated : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
      "# Baked filters: ",
      paste(
        paste0(names(baked_filters), " = ", unlist(baked_filters)),
        collapse = ", "
      ), "\n",
      "# ============================================================================="
    ),
    "",
    new_meta,
    "",
    lines[(meta_end + 1L):length(lines)]
  )

  # Write the new script
  out_path <- file.path(tfl_dir, paste0("tfl_", variant_id, ".R"))
  writeLines(new_lines, out_path)
  message(sprintf("[ars] Wrote variant script: %s", out_path))
  invisible(out_path)
}
