# =============================================================================
# tfl_f14_2_1.R
# Figure 14.2.1 – Kaplan-Meier Plot: Time to First Adverse Event
# =============================================================================
# Study:       STUDY001
# TFL ID:      f14_2_1
# Type:        Figure
# Datasets:    adtte
# Description: Kaplan-Meier survival curves for time to first adverse event
#              (TTFAE) by treatment arm, with 95% confidence bands. Safety
#              population.  Censored observations shown as tick marks.
# Author:      TFL Viewer Demo
# Date:        2026-03-13
# =============================================================================


# -- SECTION 1: METADATA ------------------------------------------------------

tfl_metadata <- list(
  id          = "f14_2_1",
  name        = "Figure 14.2.1 \u2013 KM: Time to First Adverse Event",
  type        = "Figure",
  datasets    = c("adtte"),
  description = "Kaplan-Meier curves for time to first AE by treatment arm with 95% CI (Safety population, PARAMCD = TTFAE).",

  # -- ARS extension fields (v3) -----------------------------------------------
  display = list(
    title     = "Figure 14.2.1",
    subtitle  = "Kaplan-Meier Plot: Time to First Adverse Event by Treatment Arm",
    footnotes = c(
      "Kaplan-Meier method. Shaded area: 95% pointwise confidence interval.",
      "+ = Censored observation. Subjects censored at last known event-free date or Day 180.",
      "Safety population; PARAMCD = TTFAE."
    )
  ),

  analysis_sets = list(
    list(
      id         = "AS-SAF",
      label      = "Safety Set",
      dataset    = "adtte",
      variable   = "SAFFL",
      comparator = "EQ",
      value      = "Y"
    )
  ),

  data_subsets = list(
    list(
      id         = "DS-TTFAE",
      label      = "Time to First AE endpoint",
      dataset    = "adtte",
      variable   = "PARAMCD",
      comparator = "EQ",
      value      = "TTFAE"
    )
  ),

  analyses = list(
    list(
      id              = "AN-f14_2_1-01",
      label           = "Kaplan-Meier survival analysis: time to first AE",
      analysis_set_id = "AS-SAF",
      dataset         = "adtte",
      variable        = "AVAL",
      grouping_var    = "TRT01A",
      method_id       = "METH-kaplan-meier"
    )
  ),

  methods = list(
    list(
      id         = "METH-kaplan-meier",
      label      = "Kaplan-Meier Survival Estimation",
      operations = c("survival_probability", "confidence_interval_95", "median_survival")
    )
  )
)


# -- SECTION 2: SOURCE DATASETS -----------------------------------------------

source_datasets <- list(
  adtte = adtte
)


# -- SECTION 3: FILTER CODE (display string) ----------------------------------

filter_code <- "
# Analysis population: Safety Set, Time to First AE endpoint
library(survival)

analysis_data <- adtte |>
  filter(
    SAFFL   == 'Y',
    PARAMCD == 'TTFAE'
  )

fit <- survfit(Surv(AVAL, 1 - CNSR) ~ TRT01A, data = analysis_data)
"


# -- SECTION 4: APPLY FILTERS -------------------------------------------------

library(survival)

analysis_data <- source_datasets$adtte[
  source_datasets$adtte$SAFFL   == "Y" &
  source_datasets$adtte$PARAMCD == "TTFAE",
]


# -- SECTION 5: CREATE TFL OUTPUT (ggplot KM plot) ----------------------------

# Fit KM curves by treatment arm
km_fit <- survfit(Surv(AVAL, 1 - CNSR) ~ TRT01A, data = analysis_data)

# Convert survfit to a ggplot-friendly data frame using km_to_df() from utils.R
km_df <- km_to_df(km_fit, strata_prefix = "TRT01A=")

# Censoring tick marks: extract censored observations for each stratum
cens_df <- analysis_data[analysis_data$CNSR == 1, c("AVAL", "TRT01A")]
# Estimate the survival probability at each censoring time via approxfun per arm
cens_df$surv <- mapply(
  function(t, trt) {
    arm_data <- km_df[km_df$strata == trt, ]
    if (nrow(arm_data) == 0) return(NA_real_)
    # Step function: last surv value at or before time t
    idx <- max(which(arm_data$time <= t), na.rm = TRUE)
    if (is.infinite(idx)) 1 else arm_data$surv[idx]
  },
  cens_df$AVAL, cens_df$TRT01A
)

# Median survival per arm for annotation
med_tbl <- summary(km_fit)$table
if (!is.matrix(med_tbl)) med_tbl <- t(as.matrix(med_tbl))
med_labels <- data.frame(
  strata  = sub("TRT01A=", "", rownames(med_tbl)),
  med     = round(med_tbl[, "median"], 1),
  stringsAsFactors = FALSE
)

tfl_output <- ggplot2::ggplot(
  km_df,
  ggplot2::aes(x = time, y = surv, colour = strata)
) +
  # Confidence ribbon (subtle)
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper, fill = strata),
    alpha = 0.12, colour = NA
  ) +
  # Step function survival curve
  ggplot2::geom_step(linewidth = 0.9) +
  # Censoring tick marks
  ggplot2::geom_point(
    data   = cens_df,
    ggplot2::aes(x = AVAL, y = surv, colour = TRT01A),
    shape  = 3,   # "+" tick
    size   = 2.5,
    inherit.aes = FALSE
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 180, by = 30),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::labs(
    title    = "Figure 14.2.1 \u2013 Kaplan-Meier: Time to First Adverse Event",
    subtitle = "Safety Population  |  Shaded area: 95% pointwise CI  |  + = Censored",
    x        = "Time to First Adverse Event (Days)",
    y        = "Event-Free Probability",
    colour   = "Treatment",
    fill     = "Treatment",
    caption  = paste0(
      "Kaplan-Meier method. Censored at last known event-free date or Day 180 (max follow-up).\n",
      paste(
        paste0(med_labels$strata, ": median = ", med_labels$med, " days"),
        collapse = "   |   "
      )
    )
  ) +
  ggplot2::theme_bw(base_size = 13) +
  ggplot2::theme(
    legend.position  = "bottom",
    plot.caption     = ggplot2::element_text(size = 9, colour = "grey40"),
    panel.grid.minor = ggplot2::element_blank()
  )
