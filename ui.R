# =============================================================================
# ui.R
# Clinical TFL Viewer v2 – User Interface
# =============================================================================
# Layout: page_navbar() with three top-level panels:
#   [1] TFL Registry  – browse and select all available TFLs
#   [2] TFL Viewer    – view output, explore datasets, inspect code, investigate
#   [3] About         – study info and app version
#
# The TFL Viewer panel is the main workspace. Its sidebar holds controls that
# apply to the currently selected TFL (dataset selector, population flags,
# download). The main area is a tabset:
#   Output | Dataset Explorer (Data / Investigate) | TFL Code | Filter Code
# =============================================================================

ui <- page_navbar(

  # ---------------------------------------------------------------------------
  # App chrome
  # ---------------------------------------------------------------------------
  title = tagList(
    tags$span(
      style = "font-weight:700; letter-spacing:0.03em;",
      icon("flask-vial"), " Clinical TFL Viewer"
    ),
    tags$small(
      class = "text-muted ms-2",
      style = "font-size:0.75rem; font-weight:400;",
      "v3.1"
    )
  ),

  id    = "main_nav",
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    # Slightly tighter headings
    heading_font = font_google("Inter", wght = "300..700")
  ),

  # ============================================================================
  # PANEL 1 – TFL REGISTRY
  # ============================================================================
  nav_panel(
    title = tagList(icon("table-list"), " TFL Registry"),
    value = "tab_registry",

    layout_column_wrap(
      width  = 1,
      height = "100%",

      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span(icon("table-list"), " All TFLs"),
          actionButton(
            "btn_refresh_tfls",
            label     = tagList(icon("arrows-rotate"), " Refresh"),
            class     = "btn btn-sm btn-outline-secondary",
            title     = "Re-run all TFL scripts from disk"
          )
        ),

        # Informational sub-header row
        card_body(
          class = "pb-0 pt-2",
          uiOutput("registry_summary_ui")
        ),

        # Registry table – clicking a row loads that TFL in the Viewer tab
        DTOutput("registry_table"),

        card_footer(
          class = "text-muted small",
          "Click any row, then press \u201cView Selected TFL\u201d to open it in the TFL Viewer."
        )
      )
    )
  ),


  # ============================================================================
  # PANEL 2 – TFL VIEWER  (main workspace)
  # ============================================================================
  nav_panel(
    title = tagList(icon("eye"), " TFL Viewer"),
    value = "tab_viewer",

    layout_sidebar(

      # -----------------------------------------------------------------------
      # SIDEBAR: TFL selector + dataset controls + population flags
      # -----------------------------------------------------------------------
      sidebar = sidebar(
        width = 310,
        open  = TRUE,

        # --- TFL selector ---
        tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                   "Selected TFL"),
        selectInput(
          "sel_tfl",
          label   = NULL,
          choices = NULL,   # populated server-side once tfl_list is ready
          width   = "100%"
        ),

        # --- Selected TFL metadata card ---
        uiOutput("tfl_info_card_ui"),

        hr(class = "my-2"),

        # --- Dataset selector (shown when TFL uses multiple datasets) ---
        uiOutput("dataset_selector_ui"),

        # --- Population flags (auto-detected from the selected dataset) ---
        uiOutput("pop_flags_ui"),

        hr(class = "my-2"),

        # --- Quick stats (reactive) ---
        uiOutput("quick_stats_ui"),

        hr(class = "my-2"),

        # --- Save as Variant (v3) ---
        uiOutput("save_variant_ui"),

        hr(class = "my-2"),

        # --- Downloads ---
        tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                   "Export"),
        downloadButton(
          "dl_filtered_data",
          label = tagList(icon("file-excel"), " Download Filtered Data"),
          class = "btn btn-success btn-sm w-100"
        ),

        tags$div(
          class = "text-muted small mt-1",
          "Exports the currently filtered dataset (not the formatted TFL)."
        )
      ),

      # -----------------------------------------------------------------------
      # MAIN AREA: tabset
      # -----------------------------------------------------------------------
      navset_card_tab(
        id = "viewer_tabs",

        # --- Tab 1: TFL Output -----------------------------------------------
        nav_panel(
          title = tagList(icon("table"), " Output"),
          value = "vtab_output",

          card_body(
            class = "p-3",
            # Shows TFL title and description above the output
            uiOutput("tfl_header_ui"),
            tags$hr(class = "my-2"),
            # Dynamically renders either a DT table or a plot
            uiOutput("tfl_output_ui")
          )
        ),

        # --- Tab 2: Source Data (filtered dataset behind the TFL) ---------------
        nav_panel(
          title = tagList(icon("table-cells"), " Source Data"),
          value = "vtab_source_data",

          card_body(
            class = "p-3",
            tags$h6(
              class = "text-uppercase text-muted small fw-semibold",
              icon("filter"), " Filtered Dataset Used to Generate This TFL"
            ),
            tags$p(
              class = "text-muted small",
              "This table shows the analysis-ready dataset (",
              tags$code("analysis_data"),
              ") that was filtered and used by the TFL script to produce the output."
            ),
            uiOutput("source_data_info_ui"),
            tags$div(class = "mt-2", DTOutput("source_data_table"))
          )
        ),

        # --- Tab 3: Dataset Explorer -----------------------------------------
        nav_panel(
          title = tagList(icon("database"), " Dataset Explorer"),
          value = "vtab_explorer",

          navset_tab(
            id = "explorer_sub_tabs",

            # Sub-tab A: Raw filtered data table
            nav_panel(
              title = tagList(icon("table"), " Data"),
              value = "etab_data",
              card_body(
                class = "p-2",
                uiOutput("explorer_dataset_info_ui"),
                tags$div(class = "mt-2", DTOutput("explorer_table"))
              )
            ),

            # Sub-tab B: Investigation tools
            nav_panel(
              title = tagList(icon("magnifying-glass-chart"), " Investigate"),
              value = "etab_investigate",
              card_body(
                class = "p-3",

                # Row 1: Unique values + Patient listing (side by side)
                layout_column_wrap(
                  width = 1 / 2,

                  # Unique Values panel
                  card(
                    card_header(tagList(icon("list-ol"), " Unique Values")),
                    card_body(
                      selectInput(
                        "inv_col",
                        label   = "Select column",
                        choices = NULL,
                        width   = "100%"
                      ),
                      DTOutput("inv_unique_table", height = "220px"),
                      tags$div(
                        class = "mt-2",
                        plotOutput("inv_unique_plot", height = "180px")
                      )
                    )
                  ),

                  # Patient Listing panel
                  card(
                    card_header(tagList(icon("user"), " Patient Listing")),
                    card_body(
                      selectInput(
                        "inv_usubjid",
                        label   = "Select subject (USUBJID)",
                        choices = NULL,
                        width   = "100%"
                      ),
                      uiOutput("inv_patient_info_ui"),
                      tags$div(
                        class = "mt-2",
                        DTOutput("inv_patient_table", height = "260px")
                      )
                    )
                  )
                ),

                tags$hr(class = "my-3"),

                # Row 2: Exploratory plot builder
                card(
                  card_header(tagList(icon("chart-simple"), " Exploratory Plot")),
                  card_body(
                    layout_column_wrap(
                      width = 1 / 4,
                      selectInput("inv_plot_type",  "Plot type",
                                  c("Bar", "Histogram", "Box", "Scatter"),
                                  width = "100%"),
                      selectInput("inv_plot_x",     "X-axis",
                                  choices = NULL, width = "100%"),
                      selectInput("inv_plot_y",     "Y-axis (numeric)",
                                  choices = NULL, width = "100%"),
                      selectInput("inv_plot_color", "Colour by",
                                  choices = NULL, width = "100%")
                    ),
                    plotOutput("inv_custom_plot", height = "350px")
                  )
                )
              )
            )
          )
        ),

        # --- Tab 3: TFL Code -------------------------------------------------
        nav_panel(
          title = tagList(icon("code"), " TFL Code"),
          value = "vtab_tfl_code",

          card_body(
            class = "p-3",
            tags$p(
              class = "text-muted small mb-2",
              "Full source of the TFL script as it exists on disk.",
              "This is the code that produced the Output shown in the Output tab."
            ),
            tags$pre(
              style = paste(
                "background:#f8f9fa; border:1px solid #dee2e6;",
                "border-radius:4px; padding:12px;",
                "font-size:0.8rem; max-height:560px; overflow-y:auto;"
              ),
              verbatimTextOutput("tfl_script_code", placeholder = TRUE)
            )
          )
        ),

        # --- Tab 4: Filter Code ----------------------------------------------
        nav_panel(
          title = tagList(icon("filter"), " Filter Code"),
          value = "vtab_filter_code",

          card_body(
            class = "p-3",

            # Static filter code from the script (the declared recipe)
            tags$h6(class = "text-uppercase text-muted small fw-semibold",
                    icon("file-code"), " Script Filter Recipe"),
            tags$p(
              class = "text-muted small",
              "The dataset preparation logic as written in the TFL script."
            ),
            tags$pre(
              style = paste(
                "background:#f8f9fa; border:1px solid #dee2e6;",
                "border-radius:4px; padding:12px;",
                "font-size:0.8rem; max-height:220px; overflow-y:auto;"
              ),
              verbatimTextOutput("filter_code_static", placeholder = TRUE)
            ),

            tags$hr(class = "my-3"),

            # Live filter code: updated in real time as the user changes flags
            tags$h6(class = "text-uppercase text-muted small fw-semibold",
                    icon("bolt"), " Live Filter Code"),
            tags$p(
              class = "text-muted small",
              "Equivalent dplyr code reflecting your current population flag and column filter selections."
            ),
            tags$pre(
              style = paste(
                "background:#fff8e1; border:1px solid #ffe082;",
                "border-radius:4px; padding:12px;",
                "font-size:0.8rem; max-height:220px; overflow-y:auto;"
              ),
              verbatimTextOutput("filter_code_live", placeholder = TRUE)
            ),

            tags$hr(class = "my-3"),

            # Re-run button
            actionButton(
              "btn_rerun_tfl",
              label = tagList(icon("play"), " Re-run TFL with Current Filters"),
              class = "btn btn-warning btn-sm"
            ),
            tags$span(
              class = "text-muted small ms-2",
              "Rebuilds the Output tab using only subjects that pass your current filters."
            ),
            tags$div(class = "mt-2", uiOutput("rerun_status_ui")),

            tags$hr(class = "my-3"),

            # ARS JSON Export (v3)
            tags$h6(
              class = "text-uppercase text-muted small fw-semibold",
              icon("file-code"), " ARS JSON Export"
            ),
            tags$p(
              class = "text-muted small",
              "Downloads a CDISC ARS-compliant JSON file capturing this TFL and all active filter selections."
            ),
            downloadButton(
              "btn_export_ars",
              label = tagList(icon("download"), " Export ARS JSON"),
              class = "btn btn-outline-primary btn-sm"
            ),
            tags$div(
              class = "text-muted small mt-1",
              tags$a(
                href   = "https://www.cdisc.org/standards/foundational/ars",
                target = "_blank",
                "About CDISC ARS \u2197"
              )
            )
          )
        ),

        # --- Tab 7: Reviews & Audit Trail ------------------------------------
        nav_panel(
          title = tagList(icon("comments"), " Reviews"),
          value = "vtab_reviews",

          card_body(
            class = "p-3",

            # -- Reviewer identity --
            tags$h6(
              class = "text-uppercase text-muted small fw-semibold",
              icon("user-shield"), " Reviewer Identity"
            ),
            tags$p(
              class = "text-muted small",
              "Set your name and role once per session. All comments you submit will be tagged with this identity."
            ),
            layout_column_wrap(
              width = 1 / 2,
              textInput("reviewer_name", "Your Name", value = "", width = "100%",
                        placeholder = "e.g. Jane Smith"),
              selectInput("reviewer_role", "Your Role", width = "100%",
                          choices = c("Select role..." = "", REVIEWER_ROLES))
            ),

            tags$hr(class = "my-3"),

            # -- Audit status bar --
            tags$h6(
              class = "text-uppercase text-muted small fw-semibold",
              icon("clipboard-check"), " Audit Status"
            ),
            tags$p(
              class = "text-muted small",
              "Shows which reviewer roles have commented on this TFL."
            ),
            uiOutput("audit_status_ui"),

            tags$hr(class = "my-3"),

            # -- Existing comments --
            tags$div(
              class = "d-flex justify-content-between align-items-center",
              tags$h6(
                class = "text-uppercase text-muted small fw-semibold mb-0",
                icon("comments"), " Comments"
              ),
              # Toggle between thread view and reviewer summary
              tags$div(
                class = "btn-group btn-group-sm",
                actionButton("btn_view_threads", "Threads",
                             class = "btn btn-outline-secondary btn-sm active",
                             style = "font-size: 0.75rem; padding: 2px 8px;"),
                actionButton("btn_view_reviewers", "By Reviewer",
                             class = "btn btn-outline-secondary btn-sm",
                             style = "font-size: 0.75rem; padding: 2px 8px;")
              )
            ),
            tags$div(class = "mt-2", uiOutput("reviews_list_ui")),
            uiOutput("reviewer_summary_ui"),

            tags$hr(class = "my-3"),

            # -- New comment form --
            tags$h6(
              class = "text-uppercase text-muted small fw-semibold",
              icon("pen"), " Add Comment"
            ),
            textAreaInput("review_text", label = NULL,
                          placeholder = "Write your review comment here...",
                          width = "100%", rows = 3),
            actionButton(
              "btn_submit_review",
              label = tagList(icon("paper-plane"), " Submit Comment"),
              class = "btn btn-primary btn-sm"
            ),

            tags$hr(class = "my-3"),

            # -- Excel export --
            tags$h6(
              class = "text-uppercase text-muted small fw-semibold",
              icon("file-excel"), " Review Audit Export"
            ),
            tags$p(
              class = "text-muted small",
              "Download an Excel workbook with all review comments across every TFL plus an audit summary."
            ),
            downloadButton(
              "dl_review_audit",
              label = tagList(icon("download"), " Download Review Audit (Excel)"),
              class = "btn btn-outline-success btn-sm"
            )
          )
        )
      )
    )
  ),


  # ============================================================================
  # PANEL 3 – ABOUT
  # ============================================================================
  nav_panel(
    title = tagList(icon("circle-info"), " About"),
    value = "tab_about",

    layout_column_wrap(
      width = 1 / 2,

      card(
        card_header(tagList(icon("flask-vial"), " About This App")),
        card_body(
          tags$p(
            tags$strong("Clinical TFL Viewer v2"), " is an interactive R Shiny application",
            "for exploring Tables, Figures, and Listings (TFLs) produced from",
            "CDISC ADaM analysis datasets."
          ),
          tags$ul(
            tags$li("Auto-discovers TFL scripts from a folder and runs them at startup."),
            tags$li("Tracks which datasets went into each TFL."),
            tags$li("Lets you explore and filter source datasets interactively."),
            tags$li("Displays the code that produced each TFL alongside a live version."),
            tags$li("Investigation mode: unique values, patient listings, exploratory plots."),
            tags$li(tags$strong("v3: "), "ARS-compliant metadata schema for all TFLs."),
            tags$li(tags$strong("v3: "), "Export any TFL + active filters as a CDISC ARS JSON file."),
            tags$li(tags$strong("v3: "), "Save filtered views as new TFL variants from the UI.")
          ),
          tags$hr(),
          tags$p(tags$strong("Study ID:"), textOutput("about_study_id", inline = TRUE)),
          tags$p(tags$strong("App Version:"), APP_VERSION),
          tags$p(tags$strong("R Version:"), paste(R.version$major, R.version$minor, sep = ".")),
          tags$hr(),
          tags$p(
            tags$strong("CDISC ARS Standard: "),
            tags$a("cdisc.org/standards/foundational/ars",
                   href = "https://www.cdisc.org/standards/foundational/ars",
                   target = "_blank")
          ),
          tags$p(
            tags$strong("ARS GitHub Spec: "),
            tags$a("github.com/cdisc-org/analysis-results-standard",
                   href = "https://github.com/cdisc-org/analysis-results-standard",
                   target = "_blank")
          )
        )
      ),

      card(
        card_header(tagList(icon("database"), " Dataset Summary")),
        card_body(
          tableOutput("about_dataset_summary")
        )
      )
    )
  ),

  # Spacer pushes the study label to the far right
  nav_spacer(),
  nav_item(
    tags$span(
      class = "text-muted small",
      style = "line-height:3rem;",
      icon("hospital"), " ", STUDY_ID
    )
  )
)
