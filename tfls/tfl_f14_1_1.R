# =============================================================================
# tfl_f14_1_1.R
# Figure 14.1.1 – Mean Change from Baseline in ALT by Visit and Treatment
# =============================================================================
# Study:       CDISCPILOT01 (pharmaverseadam)
# TFL ID:      f14_1_1
# Type:        Figure
# Datasets:    adsl, adlb
# Description: Grouped bar chart showing mean (± SE) change from baseline in
#              Alanine Aminotransferase (ALT, U/L) at each scheduled visit,
#              stratified by treatment arm. Safety population.
# Author:      TFL Viewer Demo
# Date:        2026-03-13
# =============================================================================


# -- SECTION 1: METADATA ------------------------------------------------------

tfl_metadata <- list(
  id          = "f14_1_1",
  name        = "Figure 14.1.1 \u2013 Mean Change from Baseline in ALT by Visit",
  type        = "Figure",
  datasets    = c("adsl", "adlb"),
  description = "Mean (\u00b1 SE) change from baseline in ALT (U/L) by scheduled visit and treatment arm (Safety population).",

  # -- ARS extension fields (v3) -----------------------------------------------
  display = list(
    title     = "Figure 14.1.1",
    subtitle  = "Mean Change from Baseline in ALT by Scheduled Visit and Treatment",
    footnotes = c(
      "ALT = Alanine Aminotransferase (U/L). Error bars represent \u00b1 1 Standard Error.",
      "Based on subjects with both a baseline and at least one post-baseline ALT value.",
      "Safety population; ANL01FL = Y; PARAMCD = ALT."
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
    ),
    list(
      id         = "AS-ANL01",
      label      = "Analysis Flag 01",
      dataset    = "adlb",
      variable   = "ANL01FL",
      comparator = "EQ",
      value      = "Y"
    )
  ),

  data_subsets = list(
    list(
      id         = "DS-ALT",
      label      = "ALT parameter only",
      dataset    = "adlb",
      variable   = "PARAMCD",
      comparator = "EQ",
      value      = "ALT"
    ),
    list(
      id         = "DS-POSTBASE",
      label      = "Post-baseline visits only",
      dataset    = "adlb",
      variable   = "AVISITN",
      comparator = "GT",
      value      = "0"
    )
  ),

  analyses = list(
    list(
      id              = "AN-f14_1_1-01",
      label           = "Mean change from baseline in ALT by visit and treatment",
      analysis_set_id = "AS-SAF",
      dataset         = "adlb",
      variable        = "CHG",
      grouping_var    = "TRT01A",
      method_id       = "METH-mean-se"
    )
  ),

  methods = list(
    list(
      id         = "METH-mean-se",
      label      = "Mean and Standard Error",
      operations = c("mean", "standard_error")
    )
  )
)


# -- SECTION 2: SOURCE DATASETS -----------------------------------------------

source_datasets <- list(
  adsl = adsl,
  adlb = adlb
)


# -- SECTION 3: FILTER CODE (display string) ----------------------------------

filter_code <- "
# Analysis population: Safety Set, ALT parameter, post-baseline visits only
analysis_data <- adlb |>
  filter(
    SAFFL   == 'Y',
    ANL01FL == 'Y',
    PARAMCD == 'ALT',
    AVISITN > 0       # exclude baseline from the change-from-baseline plot
  )
"


# -- SECTION 4: APPLY FILTERS -------------------------------------------------

analysis_data <- source_datasets$adlb[
  source_datasets$adlb$SAFFL   == "Y" &
  source_datasets$adlb$ANL01FL == "Y" &
  source_datasets$adlb$PARAMCD == "ALT" &
  source_datasets$adlb$AVISITN >  0,
]


# -- SECTION 5: CREATE TFL OUTPUT (ggplot) ------------------------------------

# Summarise: mean and SE of CHG per visit × treatment, preserving visit order
library(dplyr)

lab_summary <- analysis_data |>
  group_by(TRT01A, AVISIT, AVISITN) |>
  summarise(
    mean_chg = mean(CHG, na.rm = TRUE),
    se_chg   = sd(CHG,   na.rm = TRUE) / sqrt(dplyr::n()),
    n_obs    = dplyr::n(),
    .groups  = "drop"
  ) |>
  # Re-order visits chronologically for the x-axis
  dplyr::mutate(
    AVISIT = factor(AVISIT, levels = unique(AVISIT[order(AVISITN)]))
  )

tfl_output <- ggplot2::ggplot(
  lab_summary,
  ggplot2::aes(x = AVISIT, y = mean_chg, fill = TRT01A)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(0.8),
    width    = 0.7,
    colour   = "white"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mean_chg - se_chg, ymax = mean_chg + se_chg),
    position = ggplot2::position_dodge(0.8),
    width    = 0.25,
    linewidth = 0.6
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::labs(
    title    = "Figure 14.1.1 \u2013 Mean Change from Baseline in ALT",
    subtitle = "Safety Population  |  Error bars: \u00b1 1 SE",
    x        = "Scheduled Visit",
    y        = "Mean Change from Baseline (U/L)",
    fill     = "Treatment",
    caption  = paste0("ALT = Alanine Aminotransferase. ",
                      "Based on subjects with both baseline and post-baseline values.")
  ) +
  ggplot2::theme_bw(base_size = 13) +
  ggplot2::theme(
    legend.position  = "bottom",
    axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
    plot.caption     = ggplot2::element_text(size = 9, colour = "grey40"),
    panel.grid.minor = ggplot2::element_blank()
  )
