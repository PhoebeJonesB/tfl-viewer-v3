# =============================================================================
# TFL SCRIPT TEMPLATE (v3 – ARS-compliant)
# =============================================================================
#
# HOW TO USE THIS TEMPLATE
# ------------------------
# 1. Copy this file to a new file named  tfl_<id>.R  in the tfls/ folder.
#    Naming convention:
#      t  = Table     e.g.  tfl_t14_1_1.R
#      f  = Figure    e.g.  tfl_f14_2_1.R
#      l  = Listing   e.g.  tfl_l16_1_1.R
#
# 2. Fill in the sections below:
#      [1] METADATA        – base fields + optional ARS extension fields (v3)
#      [2] SOURCE DATASETS – declare which ADaM datasets are used
#      [3] FILTER CODE     – optional; auto-derived from analysis_sets if omitted
#      [4] APPLY FILTERS   – apply your analysis population logic
#      [5] TFL OUTPUT      – produce the final table or figure
#
# 3. The app (runner.R) executes this script in a child environment that
#    already contains the ADaM datasets (adsl, adae, adlb, adtte).
#    You do NOT need to load data; just reference it by name.
#
# ARS EXTENSION FIELDS (optional but recommended)
# ------------------------------------------------
#   display       – title, subtitle, footnotes for the Output tab
#   analysis_sets – machine-readable population definitions (WhereClause)
#   data_subsets  – record-level filter conditions beyond the population
#   analyses      – links variable, population, grouping, and method
#   methods       – statistical method and operations
#
# If analysis_sets is provided, runner.R will:
#   • Pre-filter the injected data before the script body runs
#   • Auto-derive filter_code if the script omits Section 3
#
# CONTRACT OBJECTS (must be defined before the end of the script)
# ---------------------------------------------------------------
#   tfl_metadata    - named list (required base + optional ARS fields)
#   source_datasets - named list of data frames used as analysis inputs
#   filter_code     - character string shown in "Filter Code" tab (optional in v3)
#   tfl_output      - data.frame (Table/Listing) OR ggplot object (Figure)
#
# REFERENCES
# ----------
#   CDISC ARS : https://www.cdisc.org/standards/foundational/ars
#   ARS GitHub : https://github.com/cdisc-org/analysis-results-standard
#
# =============================================================================

# Study:       STUDY001
# TFL ID:      t00_0_0          ← change me
# TFL Name:    Table 00.0.0 – [Descriptive Title]
# Type:        Table            ← "Table" | "Figure" | "Listing"
# Datasets:    adsl             ← comma-separated dataset names
# Description: One sentence describing what this TFL shows.
# Author:      [Your Name]
# Date:        YYYY-MM-DD


# =============================================================================
# SECTION 1 – METADATA
# =============================================================================

tfl_metadata <- list(

  # ── Required base fields (same as v2) ──────────────────────────────────────
  id          = "t00_0_0",
  name        = "Table 00.0.0 \u2013 [Descriptive Title]",
  type        = "Table",          # "Table" | "Figure" | "Listing"
  datasets    = c("adsl"),        # must match names in generate_adam_data()
  description = "Brief plain-English description of this TFL.",

  # ── Optional: Display (v3) ─────────────────────────────────────────────────
  # Used to render formatted title, subtitle, and footnotes in the Output tab.
  display = list(
    title     = "Table 00.0.0",
    subtitle  = "[Descriptive Title]",
    footnotes = c(
      "Footnote 1.",
      "Footnote 2."
    )
  ),

  # ── Optional: Analysis sets / population (v3) ──────────────────────────────
  # Machine-readable population definition. Corresponds to ARS AnalysisSet.
  # If provided, runner.R will auto-apply these as pre-filters and auto-derive
  # filter_code so you can omit Section 3.
  #
  # comparator: "EQ" | "NE" | "IN" | "NOTIN"
  analysis_sets = list(
    list(
      id         = "AS-ITT",
      label      = "Intent-to-Treat",
      dataset    = "adsl",
      variable   = "ITTFL",
      comparator = "EQ",
      value      = "Y"
    )
    # Add more sets if the analysis uses multiple populations
  ),

  # ── Optional: Data subsets (v3) ────────────────────────────────────────────
  # Record-level filters applied within a dataset beyond the population flag.
  # Corresponds to ARS DataSubset. NULL if no additional record-level filtering.
  data_subsets = NULL,
  # data_subsets = list(
  #   list(
  #     id         = "DS-ALT",
  #     label      = "ALT parameter only",
  #     dataset    = "adlb",
  #     variable   = "PARAMCD",
  #     comparator = "EQ",
  #     value      = "ALT"
  #   )
  # ),

  # ── Optional: Analyses (v3) ────────────────────────────────────────────────
  # Ties together population, outcome variable, grouping, and method.
  # Corresponds to ARS Analysis.
  analyses = list(
    list(
      id              = "AN-t00_0_0-01",
      label           = "Description of what is being analysed",
      analysis_set_id = "AS-ITT",
      dataset         = "adsl",
      variable        = "DCSREAS",     # the outcome variable
      grouping_var    = "TRT01A",      # what defines result columns
      method_id       = "METH-freq-pct"
    )
  ),

  # ── Optional: Methods (v3) ─────────────────────────────────────────────────
  # Corresponds to ARS AnalysisMethod + Operation.
  methods = list(
    list(
      id         = "METH-freq-pct",
      label      = "Frequency and Percentage",
      operations = c("count", "percent")
    )
  )
)


# =============================================================================
# SECTION 2 – SOURCE DATASETS
# =============================================================================
# List every ADaM dataset this TFL reads from.

source_datasets <- list(
  adsl = adsl
  # adae  = adae,    # uncomment if used
  # adlb  = adlb,    # uncomment if used
  # adtte = adtte    # uncomment if used
)


# =============================================================================
# SECTION 3 – FILTER / ANALYSIS POPULATION CODE  (optional in v3)
# =============================================================================
# In v3, if tfl_metadata$analysis_sets is defined above, this section can be
# omitted — runner.R will auto-derive filter_code from the structured fields.
#
# Include this section if:
#   • You want to customise the display string shown in the Filter Code tab
#   • Your filter logic is more complex than a simple WHERE clause
#
# filter_code <- "
# # Analysis population: Intent-to-Treat (ITT)
# analysis_data <- adsl |>
#   filter(ITTFL == 'Y')
# "


# =============================================================================
# SECTION 4 – APPLY FILTERS  (must produce object `analysis_data`)
# =============================================================================
# If analysis_sets was declared above, the data injected here is already
# pre-filtered by runner.R.  You may still re-apply the filter explicitly
# (filtering a filtered dataset is idempotent).

analysis_data <- source_datasets$adsl |>
  dplyr::filter(ITTFL == "Y")


# =============================================================================
# SECTION 5 – CREATE TFL OUTPUT  (must produce object `tfl_output`)
# =============================================================================
# For Tables / Listings: produce a data.frame or tibble.
# For Figures:           produce a ggplot object (do NOT call print() or ggsave()).

tfl_output <- analysis_data   # ← replace with your summary / visualisation code

# Example Table skeleton:
# tfl_output <- analysis_data |>
#   dplyr::group_by(TRT01A, DCSREAS) |>
#   dplyr::summarise(n = dplyr::n(), .groups = "drop")

# Example Figure skeleton:
# tfl_output <- ggplot2::ggplot(analysis_data, ggplot2::aes(x = TRT01A)) +
#   ggplot2::geom_bar() +
#   ggplot2::theme_bw()
