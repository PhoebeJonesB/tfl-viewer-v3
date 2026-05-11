# =============================================================================
# tfl_t14_3_1.R
# Table 14.3.1 – Adverse Events by System Organ Class and Preferred Term
# =============================================================================
# Study:       CDISCPILOT01 (pharmaverseadam)
# TFL ID:      t14_3_1
# Type:        Table
# Datasets:    adsl, adae
# Description: Number and percentage of subjects with any AE, serious AE, and
#              all AEs organised by MedDRA System Organ Class and Preferred
#              Term.  Safety population.
# Author:      TFL Viewer Demo
# Date:        2026-03-13
# =============================================================================


# -- SECTION 1: METADATA ------------------------------------------------------

tfl_metadata <- list(
  id          = "t14_3_1",
  name        = "Table 14.3.1 \u2013 Adverse Events by SOC and PT",
  type        = "Table",
  datasets    = c("adsl", "adae"),
  description = "Subjects with AEs by MedDRA SOC and Preferred Term (Safety population, sorted by descending frequency).",

  # -- ARS extension fields (v3) -----------------------------------------------
  display = list(
    title     = "Table 14.3.1",
    subtitle  = "Adverse Events by System Organ Class and Preferred Term",
    footnotes = c(
      "MedDRA = Medical Dictionary for Regulatory Activities; SOC = System Organ Class; PT = Preferred Term.",
      "A subject is counted once per PT per treatment arm.",
      "Sorted by descending frequency of SOC, then PT, based on total column."
    )
  ),

  analysis_sets = list(
    list(
      id         = "AS-SAF",
      label      = "Safety Set",
      dataset    = "adsl",
      variable   = "SAFFL",
      comparator = "EQ",
      value      = "Y"
    )
  ),

  data_subsets = list(
    list(
      id         = "DS-AE-SAFFL",
      label      = "Safety Set AEs (ADAE)",
      dataset    = "adae",
      variable   = "SAFFL",
      comparator = "EQ",
      value      = "Y"
    )
  ),

  analyses = list(
    list(
      id              = "AN-t14_3_1-01",
      label           = "AE frequency by SOC and PT, by treatment arm",
      analysis_set_id = "AS-SAF",
      dataset         = "adae",
      variable        = "AEDECOD",
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
  adsl = adsl,
  adae = adae
)


# -- SECTION 3: FILTER CODE (display string) ----------------------------------

filter_code <- "
# Analysis population: Safety Set (SAFFL == 'Y')
safe_subj <- adsl |>
  filter(SAFFL == 'Y') |>
  select(USUBJID, TRT01A)

analysis_data <- adae |>
  filter(SAFFL == 'Y') |>
  # One record per subject per preferred term (distinct subjects for %)
  distinct(USUBJID, AEBODSYS, AEDECOD, .keep_all = TRUE)
"


# -- SECTION 4: APPLY FILTERS -------------------------------------------------

# Safety-population subject counts per treatment arm
safe_subj <- source_datasets$adsl[source_datasets$adsl$SAFFL == "Y",
                                    c("USUBJID", "TRT01A")]

trts  <- sort(unique(safe_subj$TRT01A))
n_trt <- setNames(
  as.integer(table(safe_subj$TRT01A)[trts]),
  trts
)
n_all <- nrow(safe_subj)

# Distinct subjects per AE term (a subject is counted once per PT per arm)
ae_safe <- source_datasets$adae[source_datasets$adae$SAFFL == "Y", ]
ae_distinct <- unique(ae_safe[, c("USUBJID", "TRT01A", "AEBODSYS", "AEDECOD")])

analysis_data <- ae_distinct


# -- SECTION 5: CREATE TFL OUTPUT ---------------------------------------------

# Helper: subjects with the event / total subjects in that arm → "n (%)"
count_row <- function(subj_ids, label, indent = "") {
  row <- data.frame(
    `System Organ Class / Preferred Term` = paste0(indent, label),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (trt in trts) {
    n <- sum(subj_ids %in% safe_subj$USUBJID[safe_subj$TRT01A == trt])
    row[[trt]] <- fmt_n_pct(n, n_trt[[trt]])
  }
  n_any <- sum(subj_ids %in% safe_subj$USUBJID)
  row[["Total"]] <- fmt_n_pct(n_any, n_all)
  row
}

# --- Summary rows (Any AE / Any Serious AE) --------------------------
any_ae  <- unique(ae_safe$USUBJID)
any_ser <- unique(ae_safe$USUBJID[ae_safe$AESER == "Y"])

summary_rows <- rbind(
  count_row(any_ae,  "Any Adverse Event"),
  count_row(any_ser, "Any Serious Adverse Event")
)

# --- SOC / PT rows ---------------------------------------------------
soc_list <- sort(unique(analysis_data$AEBODSYS))

detail_rows <- lapply(soc_list, function(soc) {
  soc_subj <- unique(analysis_data$USUBJID[analysis_data$AEBODSYS == soc])
  soc_row  <- count_row(soc_subj, soc)

  # Preferred terms within this SOC, sorted by descending overall frequency
  pts <- names(sort(
    table(analysis_data$AEDECOD[analysis_data$AEBODSYS == soc]),
    decreasing = TRUE
  ))

  pt_rows <- lapply(pts, function(pt) {
    pt_subj <- unique(analysis_data$USUBJID[
      analysis_data$AEBODSYS == soc & analysis_data$AEDECOD == pt
    ])
    count_row(pt_subj, pt, indent = "  ")
  })

  rbind(soc_row, do.call(rbind, pt_rows))
})

# Header row with N denominators
hdr <- data.frame(
  `System Organ Class / Preferred Term` = "Subjects in Safety Population, n",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
for (trt in trts) hdr[[trt]] <- as.character(n_trt[[trt]])
hdr[["Total"]] <- as.character(n_all)

tfl_output <- do.call(rbind, c(list(hdr, summary_rows), detail_rows))
row.names(tfl_output) <- NULL
