# =============================================================================
# R/utils.R
# Shared Helper Functions
# =============================================================================
# Purpose : Small, reusable utilities used across global.R, server.R, and the
#           TFL scripts themselves. Keep functions focused and well-documented.
#
# v3 additions:
#   ars_to_filter_code() – generate dplyr filter code from ARS analysis_sets
# =============================================================================


# ---------------------------------------------------------------------------
# Utility: null-coalescing operator
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


# -----------------------------------------------------------------------------
# build_filter_code()
# Reconstruct a readable dplyr::filter() snippet from active filter values.
# Used by the "Live Filter Code" panel in the Filter Code tab.
# -----------------------------------------------------------------------------
#' Generate a dplyr filter code string from active selections
#'
#' @param dataset_name  Character. Name of the dataset (e.g., "adae").
#' @param pop_filters   Named character vector of population flag selections,
#'                      e.g. c(SAFFL = "Y", ITTFL = "All").
#' @param col_filters   Named list of column filter values (character vectors).
#'
#' @return A single character string showing the equivalent dplyr pipeline.
build_filter_code <- function(dataset_name, pop_filters = list(),
                               col_filters = list()) {

  # Collect active conditions (skip "All" selections)
  conditions <- character(0)

  for (fl in names(pop_filters)) {
    val <- pop_filters[[fl]]
    if (!is.null(val) && val != "All") {
      conditions <- c(conditions, sprintf('%s == "%s"', fl, val))
    }
  }

  for (col in names(col_filters)) {
    vals <- col_filters[[col]]
    if (!is.null(vals) && length(vals) > 0 && !("All" %in% vals)) {
      if (length(vals) == 1L) {
        conditions <- c(conditions, sprintf('%s == "%s"', col, vals))
      } else {
        quoted <- paste0('"', vals, '"', collapse = ", ")
        conditions <- c(conditions, sprintf('%s %%in%% c(%s)', col, quoted))
      }
    }
  }

  if (length(conditions) == 0L) {
    return(sprintf("%s  # no filters applied", dataset_name))
  }

  cond_lines <- paste0("    ", conditions, collapse = ",\n")
  sprintf("%s |>\n  filter(\n%s\n  )", dataset_name, cond_lines)
}


# -----------------------------------------------------------------------------
# fmt_n_pct()
# Format a count and denominator as "n (xx.x%)".  Handles zero denominators.
# -----------------------------------------------------------------------------
#' Format count and percentage
#'
#' @param n   Numerator (count).
#' @param N   Denominator (total).
#' @param digits Integer. Decimal places for the percentage. Default 1.
#'
#' @return Character string e.g. "42 (21.0%)" or "0" if n == 0.
fmt_n_pct <- function(n, N, digits = 1) {
  if (is.na(n) || n == 0L) return("0")
  if (is.na(N) || N == 0L) return(as.character(n))
  pct <- round(100 * n / N, digits)
  sprintf("%d (%.*f%%)", as.integer(n), digits, pct)
}


# -----------------------------------------------------------------------------
# detect_pop_flags()
# Identify population flag columns (names ending in "FL") in a data frame.
# -----------------------------------------------------------------------------
#' Return names of population flag columns present in a data frame
#'
#' @param df A data.frame.
#'
#' @return Character vector of column names ending in "FL", e.g. c("SAFFL", "ITTFL").
detect_pop_flags <- function(df) {
  grep("FL$", names(df), value = TRUE)
}


# -----------------------------------------------------------------------------
# tfl_type_icon()
# Map a TFL type string to a Bootstrap / Font Awesome icon name.
# -----------------------------------------------------------------------------
#' Return a Font Awesome icon name for a TFL type
#'
#' @param type Character. One of "Table", "Figure", "Listing", "Error".
#'
#' @return Character icon name suitable for shiny::icon().
tfl_type_icon <- function(type) {
  switch(type,
    "Table"   = "table",
    "Figure"  = "chart-bar",
    "Listing" = "list-ul",
    "Error"   = "triangle-exclamation",
    "file"
  )
}


# -----------------------------------------------------------------------------
# safe_unique_vals()
# Get sorted unique values of a column, handling edge cases.
# -----------------------------------------------------------------------------
#' Get sorted unique non-NA values for filter dropdowns
#'
#' @param df     A data.frame.
#' @param column Character. Column name.
#' @param max_n  Integer. Maximum number of values to return (avoids huge dropdowns).
#'
#' @return Character vector of unique values, prepended with "All".
safe_unique_vals <- function(df, column, max_n = 200L) {
  if (!column %in% names(df)) return("All")
  vals <- sort(unique(na.omit(df[[column]])))
  if (length(vals) > max_n) vals <- vals[seq_len(max_n)]
  c("All", as.character(vals))
}


# -----------------------------------------------------------------------------
# km_to_df()
# Convert a survfit object to a ggplot-friendly data frame.
# Avoids the broom dependency while still supporting confidence ribbons.
# -----------------------------------------------------------------------------
#' Convert survfit to a plottable data frame
#'
#' @param fit  A survfit object (from survival::survfit()).
#' @param strata_prefix  Character prefix to strip from strata labels,
#'                       e.g. "TRT01A=" to give clean treatment names.
#'
#' @return A data.frame with columns: time, surv, lower, upper, n_risk, strata.
km_to_df <- function(fit, strata_prefix = "") {

  if (is.null(fit$strata)) {
    # Single-stratum fit
    return(data.frame(
      time   = c(0, fit$time),
      surv   = c(1, fit$surv),
      lower  = c(1, fit$lower),
      upper  = c(1, fit$upper),
      strata = "All",
      stringsAsFactors = FALSE
    ))
  }

  # Multi-stratum fit: rebuild index bounds for each stratum
  strata_labels <- sub(strata_prefix, "", names(fit$strata), fixed = TRUE)
  cum_idx       <- cumsum(fit$strata)

  rows <- lapply(seq_along(fit$strata), function(i) {
    start <- if (i == 1L) 1L else cum_idx[i - 1L] + 1L
    end   <- cum_idx[i]
    data.frame(
      time   = c(0, fit$time[start:end]),
      surv   = c(1, fit$surv[start:end]),
      lower  = c(1, fit$lower[start:end]),
      upper  = c(1, fit$upper[start:end]),
      strata = strata_labels[i],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


# -----------------------------------------------------------------------------
# ars_to_filter_code()
# Convert structured ARS analysis_sets + data_subsets to a dplyr filter code
# string.  Used by runner.R to auto-derive filter_code when the script omits
# it, and by the Filter Code tab to display the ARS-derived recipe.
# -----------------------------------------------------------------------------
#' Generate dplyr filter code from ARS analysis_sets and data_subsets
#'
#' @param dataset_name   Character. The primary dataset name (e.g. "adsl").
#' @param analysis_sets  List of analysis set definitions from tfl_metadata.
#' @param data_subsets   List of data subset definitions from tfl_metadata.
#'
#' @return A character string showing the equivalent dplyr pipeline.
ars_to_filter_code <- function(dataset_name,
                                analysis_sets = NULL,
                                data_subsets  = NULL) {

  comparator_to_r <- function(cmp) {
    switch(cmp,
      EQ    = "==",
      NE    = "!=",
      IN    = "%in%",
      NOTIN = "%in%",   # caller wraps in ! for NOTIN
      "=="
    )
  }

  conditions <- character(0)

  # Analysis sets → population filter conditions
  if (!is.null(analysis_sets) && length(analysis_sets) > 0) {
    for (as_item in analysis_sets) {
      cmp <- comparator_to_r(as_item$comparator %||% "EQ")
      val <- as_item$value
      neg <- identical(as_item$comparator, "NOTIN")

      if (length(val) == 1L) {
        expr <- sprintf('%s %s "%s"', as_item$variable, cmp, val)
      } else {
        quoted <- paste0('"', val, '"', collapse = ", ")
        expr   <- sprintf('%s %s c(%s)', as_item$variable, cmp, quoted)
      }
      if (neg) expr <- paste0("!", expr)
      conditions <- c(conditions, expr)
    }
  }

  # Data subsets → record-level filter conditions
  if (!is.null(data_subsets) && length(data_subsets) > 0) {
    for (ds_item in data_subsets) {
      cmp <- comparator_to_r(ds_item$comparator %||% "EQ")
      val <- ds_item$value
      neg <- identical(ds_item$comparator, "NOTIN")

      if (length(val) == 1L) {
        # Numeric check: don't quote numbers
        if (suppressWarnings(!is.na(as.numeric(val)))) {
          expr <- sprintf('%s %s %s', ds_item$variable, cmp, val)
        } else {
          expr <- sprintf('%s %s "%s"', ds_item$variable, cmp, val)
        }
      } else {
        quoted <- paste0('"', val, '"', collapse = ", ")
        expr   <- sprintf('%s %s c(%s)', ds_item$variable, cmp, quoted)
      }
      if (neg) expr <- paste0("!", expr)
      conditions <- c(conditions, expr)
    }
  }

  if (length(conditions) == 0L) {
    return(sprintf("# No ARS-defined filters\n%s", dataset_name))
  }

  cond_lines <- paste0("    ", conditions, collapse = ",\n")
  sprintf(
    "# ARS-derived filter code\nanalysis_data <- %s |>\n  filter(\n%s\n  )",
    dataset_name, cond_lines
  )
}
