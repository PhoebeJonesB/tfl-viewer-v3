# =============================================================================
# tfl_t14_1_1.R
# Table 14.1.1 – Subject Disposition
# =============================================================================
# Study:       STUDY001
# TFL ID:      t14_1_1
# Type:        Table
# Datasets:    adsl
# Description: Counts and percentages of subjects by disposition reason and
#              treatment group. ITT population.
# Author:      TFL Viewer Demo
# Date:        2026-03-13
# =============================================================================


# -- SECTION 1: METADATA ------------------------------------------------------

tfl_metadata <- list(
  id          = "t14_1_1",
  name        = "Table 14.1.1 \u2013 Subject Disposition",
  type        = "Table",
  datasets    = c("adsl"),
  description = "Number and percentage of subjects by disposition reason and treatment arm (ITT population).",

  # -- ARS extension fields (v3) -----------------------------------------------
  display = list(
    title     = "Table 14.1.1",
    subtitle  = "Subject Disposition",
    footnotes = c(
      "ITT = Intent-to-Treat.",
      "Percentages are based on the number of subjects in the analysis set."
    )
  ),

  analysis_sets = list(
    list(
      id         = "AS-ITT",
      label      = "Intent-to-Treat",
      dataset    = "adsl",
      variable   = "ITTFL",
      comparator = "EQ",
      value      = "Y"
    )
  ),

  data_subsets = NULL,

  analyses = list(
    list(
      id              = "AN-t14_1_1-01",
      label           = "Disposition reason frequency by treatment arm",
      analysis_set_id = "AS-ITT",
      dataset         = "adsl",
      variable        = "DCSREAS",
      grouping_var    = "TRT01A",
      method_id       = "METH-freq-pct"
    )
  ),

  methods = list(
    list(
      id         = "METH-freq-pct",
      label      = "Frequency and Percentage",
      operations = c("count", "percent")
    )
  )
)


# -- SECTION 2: SOURCE DATASETS -----------------------------------------------

source_datasets <- list(
  adsl = adsl
)


# -- SECTION 3: FILTER CODE (display string) ----------------------------------

filter_code <- "
# Analysis population: Intent-to-Treat (ITT)
analysis_data <- adsl |>
  filter(ITTFL == 'Y')
"


# -- SECTION 4: APPLY FILTERS -------------------------------------------------

analysis_data <- source_datasets$adsl |>
  dplyr::filter(ITTFL == "Y")


# -- SECTION 5: CREATE TFL OUTPUT ---------------------------------------------

# Treatment arms in a consistent order
trts   <- sort(unique(analysis_data$TRT01A))
n_trt  <- setNames(
  as.integer(table(analysis_data$TRT01A)[trts]),
  trts
)
n_all  <- nrow(analysis_data)

# Disposition categories in a logical display order
disp_cats <- c(
  "COMPLETED",
  "ADVERSE EVENT",
  "WITHDRAWAL BY SUBJECT",
  "LOST TO FOLLOW-UP",
  "PROTOCOL DEVIATION"
)

# Build one row per disposition category
rows <- lapply(disp_cats, function(cat) {
  row <- data.frame(
    `Disposition Reason` = cat,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  # Add n (%) column for each treatment arm
  for (trt in trts) {
    sub <- analysis_data[analysis_data$TRT01A == trt &
                           analysis_data$DCSREAS  == cat, ]
    row[[trt]] <- fmt_n_pct(nrow(sub), n_trt[[trt]])
  }
  # Total column
  sub_all <- analysis_data[analysis_data$DCSREAS == cat, ]
  row[["Total"]] <- fmt_n_pct(nrow(sub_all), n_all)
  row
})

disp_table <- do.call(rbind, rows)

# Header row showing N per arm
hdr <- data.frame(
  `Disposition Reason` = "Subjects Randomised, n",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
for (trt in trts) hdr[[trt]] <- as.character(n_trt[[trt]])
hdr[["Total"]] <- as.character(n_all)

tfl_output <- rbind(hdr, disp_table)
