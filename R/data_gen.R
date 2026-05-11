# =============================================================================
# R/data_gen.R
# ADaM Dataset Loader (pharmaverseadam)
# =============================================================================
# Purpose : Load CDISC ADaM datasets from the {pharmaverseadam} package and
#           derive columns expected by the TFL Viewer that are not present in
#           the source data.  Replaces the previous synthetic data generator.
#
# Datasets produced:
#   adsl  - Subject Level Analysis Dataset        (1 row per subject)
#   adae  - Adverse Events Analysis Dataset       (1+ rows per subject)
#   adlb  - Laboratory Data Analysis Dataset      (1 row per subject/param/visit)
#   adtte - Time-to-Event Analysis Dataset        (1 row per subject/parameter)
#
# Notes:
#   - Screen Failure subjects (TRT01A == "Screen Failure") are excluded.
#   - pharmaverseadam::adtte_onco is used for time-to-event data (the package
#     does not include a general-purpose ADTTE).
#   - DCSREAS is derived from EOSSTT (COMPLETED / DISCONTINUED) and DTHFL.
#     pharmaverseadam does not provide granular disposition reasons.
#   - ITTFL, RANDFL, PPROTFL, SEXN, and TRTDUR are derived.
#
# Usage:
#   adam  <- load_pharmaverse_data()
#   adsl  <- adam$adsl
#   adae  <- adam$adae
#   adlb  <- adam$adlb
#   adtte <- adam$adtte
# =============================================================================

#' Load ADaM datasets from the pharmaverseadam package
#'
#' @return A named list with elements: adsl, adae, adlb, adtte.
load_pharmaverse_data <- function() {

  # ---------------------------------------------------------------------------
  # 1. ADSL – Subject Level Analysis Dataset
  # ---------------------------------------------------------------------------

  adsl <- pharmaverseadam::adsl |>
    # Exclude Screen Failure subjects (no treatment, no analysis)
    dplyr::filter(TRT01A != "Screen Failure") |>
    dplyr::mutate(
      # Derive columns the TFL scripts expect
      SEXN    = dplyr::case_when(SEX == "M" ~ 1L, SEX == "F" ~ 2L,
                                 TRUE ~ NA_integer_),
      RANDFL  = "Y",
      ITTFL   = "Y",
      PPROTFL = dplyr::if_else(EOSSTT == "COMPLETED", "Y", "N",
                               missing = "N"),
      TRTDUR  = TRTDURD,

      # Disposition reason – pharmaverseadam only carries EOSSTT
      # (COMPLETED / DISCONTINUED).  We add "DEATH" for subjects with
      # DTHFL == "Y" and keep the rest as "DISCONTINUED".
      DCSREAS = dplyr::case_when(
        EOSSTT == "COMPLETED"         ~ "COMPLETED",
        !is.na(DTHFL) & DTHFL == "Y"  ~ "DEATH",
        EOSSTT == "DISCONTINUED"       ~ "DISCONTINUED",
        TRUE                           ~ "UNKNOWN"
      )
    )

  # Subject IDs that survived the filter (used to subset other datasets)
  keep_ids <- adsl$USUBJID

  # ---------------------------------------------------------------------------
  # 2. ADAE – Adverse Events Analysis Dataset
  # ---------------------------------------------------------------------------

  adae <- pharmaverseadam::adae |>
    dplyr::filter(USUBJID %in% keep_ids) |>
    # Derive ITTFL (not present in pharmaverseadam adae)
    dplyr::mutate(ITTFL = "Y")

  # ---------------------------------------------------------------------------
  # 3. ADLB – Laboratory Data Analysis Dataset
  # ---------------------------------------------------------------------------

  adlb <- pharmaverseadam::adlb |>
    dplyr::filter(USUBJID %in% keep_ids) |>
    dplyr::mutate(ITTFL = "Y")

  # ---------------------------------------------------------------------------
  # 4. ADTTE – Time-to-Event Analysis Dataset
  # ---------------------------------------------------------------------------
  # pharmaverseadam provides adtte_onco (oncology endpoints: OS, PFS, RSD).
  # It does not include a TTFAE (time to first AE) endpoint.
  # We join ADSL flags (TRT01A, ARMCD, SAFFL, ITTFL) onto adtte_onco.

  adtte <- pharmaverseadam::adtte_onco |>
    dplyr::filter(USUBJID %in% keep_ids) |>
    dplyr::left_join(
      adsl |> dplyr::select(USUBJID, TRT01A, ARMCD, SAFFL, ITTFL),
      by = "USUBJID"
    )

  list(adsl = adsl, adae = adae, adlb = adlb, adtte = adtte)
}
