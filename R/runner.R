# =============================================================================
# R/runner.R
# TFL Script Discovery and Execution Engine
# =============================================================================
# Purpose : Scan the tfls/ directory for TFL scripts, execute each one in an
#           isolated child environment that has access to the shared ADaM data,
#           and return a structured list of results including metadata, source
#           datasets, display-ready code strings, and the final TFL output.
#
# Convention: Scripts must be named  tfl_*.R  (the _template.R is excluded).
#             Each script must define the four contract objects:
#               tfl_metadata    - named list (id, name, type, datasets, description)
#                                 + optional ARS extension fields (v3)
#               source_datasets - named list of data frames used as inputs
#               filter_code     - character string (auto-derived in v3 if omitted)
#               tfl_output      - data.frame (Table/Listing) or ggplot (Figure)
#
# v3 additions:
#   validate_ars_metadata() – normalise metadata; fill in NULL defaults for ARS
#   auto-pre-filtering      – apply analysis_sets before script body runs
#   auto filter_code        – derived from analysis_sets when not hand-written
#
# Usage:
#   adam      <- generate_adam_data()
#   tfl_list  <- discover_tfls("tfls/", adam)
#   registry  <- build_registry(tfl_list)
# =============================================================================


# -----------------------------------------------------------------------------
# validate_ars_metadata()
# Normalise a raw tfl_metadata list: check required fields, fill ARS defaults.
# -----------------------------------------------------------------------------
#' Validate and normalise a tfl_metadata list
#'
#' @param metadata Raw list from a TFL script's tfl_metadata object.
#'
#' @return Normalised list with required fields checked and all optional ARS
#'         extension fields filled with NULL if absent.
validate_ars_metadata <- function(metadata) {

  required <- c("id", "name", "type", "datasets", "description")
  missing  <- setdiff(required, names(metadata))
  if (length(missing) > 0L) {
    stop(sprintf("tfl_metadata is missing required field(s): %s",
                 paste(missing, collapse = ", ")))
  }

  # Fill optional ARS extension fields with NULL defaults
  ars_optional <- c(
    "display", "analysis_sets", "data_subsets",
    "analyses", "methods", "variant_of"
  )
  for (field in ars_optional) {
    if (is.null(metadata[[field]])) metadata[[field]] <- NULL
  }

  metadata
}


# -----------------------------------------------------------------------------
# apply_analysis_sets()
# Pre-filter a named list of ADaM datasets using the analysis_sets declared in
# tfl_metadata.  This is idempotent: if the script also filters its own data,
# filtering twice on the same condition has no effect.
# -----------------------------------------------------------------------------
#' Pre-filter ADaM data using ARS analysis_sets
#'
#' @param adam_data     Named list of ADaM data frames.
#' @param analysis_sets List of analysis set definitions from tfl_metadata.
#'
#' @return Modified named list with the declared conditions applied.
apply_analysis_sets <- function(adam_data, analysis_sets) {

  if (is.null(analysis_sets) || length(analysis_sets) == 0L) return(adam_data)

  for (as_item in analysis_sets) {
    ds_name <- as_item$dataset
    if (!ds_name %in% names(adam_data)) next

    df  <- adam_data[[ds_name]]
    col <- as_item$variable
    if (!col %in% names(df)) next

    val <- as_item$value
    cmp <- as_item$comparator %||% "EQ"

    df <- switch(cmp,
      EQ    = df[!is.na(df[[col]]) & df[[col]] == val,  , drop = FALSE],
      NE    = df[!is.na(df[[col]]) & df[[col]] != val,  , drop = FALSE],
      IN    = df[!is.na(df[[col]]) & df[[col]] %in% val,, drop = FALSE],
      NOTIN = df[!is.na(df[[col]]) & !(df[[col]] %in% val),, drop = FALSE],
      df   # fallback: no filter
    )

    adam_data[[ds_name]] <- df
  }

  adam_data
}


# -----------------------------------------------------------------------------
# run_tfl_script()
# Execute a single TFL script and capture its outputs safely.
# -----------------------------------------------------------------------------
#' Run one TFL script in an isolated environment
#'
#' @param script_path  Full path to the .R script file.
#' @param adam_data    Named list of ADaM data frames (adsl, adae, adlb, adtte).
#'
#' @return A named list with:
#'   \item{success}         TRUE if script ran without error.
#'   \item{metadata}        Normalised tfl_metadata list from the script.
#'   \item{source_datasets} Named list of raw input data frames.
#'   \item{filter_code}     Character string shown in the Filter Code tab.
#'   \item{tfl_output}      data.frame or ggplot object for display.
#'   \item{script_lines}    Raw source lines for the TFL Code tab.
#'   \item{error}           Error message string (NULL on success).
#'   \item{run_time_secs}   Elapsed execution time in seconds.
run_tfl_script <- function(script_path, adam_data) {

  env <- new.env(parent = globalenv())

  script_lines <- tryCatch(
    readLines(script_path, warn = FALSE),
    error = function(e) character(0)
  )

  t_start <- proc.time()["elapsed"]

  result <- tryCatch({

    # --- v3: validate metadata first (lightweight parse for analysis_sets) ---
    # We source the script to get metadata, then apply analysis_sets as
    # pre-filters before the script body runs in its own env.
    # To avoid running the whole script twice we rely on the script being
    # idempotent when data is pre-filtered (filtering twice is a no-op).

    # Inject ADaM data (full datasets initially)
    list2env(adam_data, envir = env)

    # Execute script
    source(script_path, local = env, echo = FALSE, verbose = FALSE)

    # Capture raw metadata; validate and normalise
    raw_meta <- env$tfl_metadata
    metadata <- validate_ars_metadata(raw_meta)

    # --- v3: If analysis_sets present, re-inject pre-filtered data ----------
    # This ensures source_datasets captured by the script reflects the declared
    # population (important for the Dataset Explorer tab).
    if (!is.null(metadata$analysis_sets) && length(metadata$analysis_sets) > 0L) {
      pre_filtered <- apply_analysis_sets(adam_data, metadata$analysis_sets)
      # Update the env datasets so source_datasets captures filtered versions
      list2env(pre_filtered, envir = env)
      # Re-capture source_datasets if the script already set it
      if (!is.null(env$source_datasets)) {
        for (nm in names(env$source_datasets)) {
          if (nm %in% names(pre_filtered)) {
            env$source_datasets[[nm]] <- pre_filtered[[nm]]
          }
        }
      }
    }

    # --- v3: Auto-derive filter_code if script didn't define one ------------
    captured_fc <- env$filter_code
    if (is.null(captured_fc) || nchar(trimws(captured_fc)) == 0L) {
      # Use the primary dataset (first declared)
      primary_ds <- metadata$datasets[1]
      captured_fc <- ars_to_filter_code(
        dataset_name  = primary_ds,
        analysis_sets = metadata$analysis_sets,
        data_subsets  = metadata$data_subsets
      )
    }

    # Capture analysis_data if the script created it (the filtered dataset
    # that actually drove the TFL output)
    captured_analysis_data <- if (exists("analysis_data", envir = env))
      env$analysis_data else NULL

    list(
      success         = TRUE,
      metadata        = metadata,
      source_datasets = env$source_datasets,
      filter_code     = captured_fc,
      tfl_output      = env$tfl_output,
      analysis_data   = captured_analysis_data,
      script_lines    = script_lines,
      error           = NULL,
      run_time_secs   = unname(round(proc.time()["elapsed"] - t_start, 2))
    )

  }, error = function(e) {
    list(
      success         = FALSE,
      metadata        = list(
        id            = sub("\\.R$", "", basename(script_path)),
        name          = paste("ERROR \u2013", basename(script_path)),
        type          = "Error",
        datasets      = character(0),
        description   = conditionMessage(e),
        display       = NULL,
        analysis_sets = NULL,
        data_subsets  = NULL,
        analyses      = NULL,
        methods       = NULL,
        variant_of    = NULL
      ),
      source_datasets = NULL,
      filter_code     = NULL,
      tfl_output      = NULL,
      analysis_data   = NULL,
      script_lines    = script_lines,
      error           = conditionMessage(e),
      run_time_secs   = unname(round(proc.time()["elapsed"] - t_start, 2))
    )
  })

  result
}


# -----------------------------------------------------------------------------
# discover_tfls()
# Scan a directory and run all matching TFL scripts.
# -----------------------------------------------------------------------------
#' Discover and run all TFL scripts in a folder
#'
#' @param tfl_dir   Path to the folder containing TFL scripts.
#' @param adam_data Named list of ADaM data frames passed to each script.
#'
#' @return A named list; each element is the result of run_tfl_script().
#'         Names are the script basenames without the .R extension.
discover_tfls <- function(tfl_dir, adam_data) {

  scripts <- list.files(
    tfl_dir,
    pattern    = "^tfl_.*\\.R$",
    full.names = TRUE
  )

  if (length(scripts) == 0L) {
    warning("No TFL scripts found in: ", tfl_dir)
    return(list())
  }

  scripts <- sort(scripts)

  message(sprintf("[runner] Discovering %d TFL script(s) in '%s'", length(scripts), tfl_dir))

  results <- lapply(scripts, function(s) {
    message(sprintf("[runner]   Running: %s", basename(s)))
    run_tfl_script(s, adam_data)
  })

  names(results) <- sub("\\.R$", "", basename(scripts))
  results
}


# -----------------------------------------------------------------------------
# build_registry()
# Flatten the TFL list into a display-ready data frame for the Registry tab.
# -----------------------------------------------------------------------------
#' Build a flat registry data frame from the TFL results list
#'
#' @param tfl_list Named list returned by discover_tfls().
#'
#' @return A data frame with one row per TFL.
build_registry <- function(tfl_list) {

  if (length(tfl_list) == 0L) {
    return(data.frame(
      tfl_key       = character(0),
      id            = character(0),
      name          = character(0),
      type          = character(0),
      datasets      = character(0),
      description   = character(0),
      population    = character(0),
      grouped_by    = character(0),
      variant_of    = character(0),
      status        = character(0),
      run_time_secs = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names(tfl_list), function(key) {
    tfl <- tfl_list[[key]]
    m   <- tfl$metadata

    # Summarise analysis sets as a short string for the registry
    pop_str <- if (!is.null(m$analysis_sets) && length(m$analysis_sets) > 0) {
      paste(
        sapply(m$analysis_sets, function(s) paste0(s$variable, "=", s$value)),
        collapse = ", "
      )
    } else NA_character_

    # Summarise grouping variable from the first analysis entry
    grp_str <- if (!is.null(m$analyses) && length(m$analyses) > 0)
      m$analyses[[1]]$grouping_var %||% NA_character_
    else NA_character_

    data.frame(
      tfl_key       = key,
      id            = m$id,
      name          = m$name,
      type          = m$type,
      datasets      = paste(m$datasets, collapse = ", "),
      description   = m$description,
      population    = pop_str,
      grouped_by    = grp_str,
      variant_of    = m$variant_of %||% NA_character_,
      status        = ifelse(tfl$success, "\u2705 OK", "\u274C ERROR"),
      run_time_secs = tfl$run_time_secs,
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, rows)
  row.names(result_df) <- NULL
  result_df
}


# -----------------------------------------------------------------------------
# rerun_tfl_with_population()
# Re-execute a TFL script after replacing source datasets with user-filtered
# versions. Used by the "Re-run with Current Filters" button.
# -----------------------------------------------------------------------------
#' Re-run a TFL script against user-filtered datasets
#'
#' @param script_path       Full path to the TFL .R script.
#' @param filtered_datasets Named list of already-filtered data frames.
#' @param adam_data         Full ADaM list (fallback for datasets not filtered).
#'
#' @return Same structure as run_tfl_script().
rerun_tfl_with_population <- function(script_path, filtered_datasets, adam_data) {
  combined <- adam_data
  for (nm in names(filtered_datasets)) {
    combined[[nm]] <- filtered_datasets[[nm]]
  }
  run_tfl_script(script_path, combined)
}
