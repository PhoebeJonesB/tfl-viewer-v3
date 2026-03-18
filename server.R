# =============================================================================
# server.R
# Clinical TFL Viewer v2 – Server Logic
# =============================================================================
# Reactive graph summary:
#
#  [btn_refresh_tfls] ──► rv$tfl_list, rv$registry
#  [sel_tfl]          ──► current_tfl()
#  [sel_dataset]      ──► raw_dataset()
#  [pop_flag inputs]  ──► pop_filtered()
#  [explorer DT col]  ──► col_filtered()  (via DT column search)
#  pop_filtered()     ──► explorer_table, inv_* outputs, live filter code
#  col_filtered()     ──► quick stats, download
#  [btn_rerun_tfl]    ──► rv$rerun_output
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 0. Reactive values – mutable app state
  # ---------------------------------------------------------------------------

  rv <- reactiveValues(
    tfl_list       = tfl_list,      # from global.R (initial load)
    registry       = tfl_registry,  # from global.R (initial load)
    rerun_output   = NULL,          # result of manual "Re-run TFL" button
    rerun_msg      = NULL,          # status message for the re-run button
    variant_msg    = NULL,          # status message for variant creation (v3)
    review_trigger = 0L             # incremented to refresh review display
  )

  # Populate the TFL selector dropdown once registry is ready
  observe({
    req(rv$registry)
    choices <- setNames(rv$registry$tfl_key, rv$registry$name)
    # Pre-select the first TFL
    updateSelectInput(session, "sel_tfl",
                      choices  = choices,
                      selected = choices[1])
  })


  # ---------------------------------------------------------------------------
  # 1. REFRESH – re-run all TFL scripts from disk
  # ---------------------------------------------------------------------------

  observeEvent(input$btn_refresh_tfls, {
    showModal(modalDialog(
      title  = "Refreshing TFLs...",
      "Re-running all TFL scripts. This may take a few seconds.",
      footer = NULL,
      easyClose = FALSE
    ))

    withCallingHandlers(
      {
        new_list <- discover_tfls(TFL_DIR, adam)
        rv$tfl_list <- new_list
        rv$registry <- build_registry(new_list)
        # Rebuild dropdown
        choices <- setNames(rv$registry$tfl_key, rv$registry$name)
        updateSelectInput(session, "sel_tfl",
                          choices  = choices,
                          selected = isolate(input$sel_tfl))
      },
      message = function(m) invokeRestart("muffleMessage")
    )

    removeModal()
  })


  # ---------------------------------------------------------------------------
  # 2. REGISTRY TAB
  # ---------------------------------------------------------------------------

  # Summary banner above the registry table
  output$registry_summary_ui <- renderUI({
    req(rv$registry)
    r  <- rv$registry
    ok <- sum(r$status == "\u2705 OK")
    er <- sum(r$status == "\u274C ERROR")

    layout_column_wrap(
      width = 1 / 4,
      value_box(title = "Total TFLs",  value = nrow(r),
                theme = "secondary",   showcase = icon("table-list")),
      value_box(title = "Tables",      value = sum(r$type == "Table"),
                theme = "info",        showcase = icon("table")),
      value_box(title = "Figures",     value = sum(r$type == "Figure"),
                theme = "success",     showcase = icon("chart-bar")),
      value_box(title = "Loaded OK",   value = ok,
                theme = ifelse(er > 0, "danger", "primary"),
                showcase = icon("circle-check"))
    )
  })

  # Registry DT table (single-row selection)
  output$registry_table <- renderDT({
    req(rv$registry)
    rv$review_trigger   # re-render when reviews change

    reg <- rv$registry

    # Add review status column
    review_status <- vapply(reg$tfl_key, function(key) {
      status <- get_tfl_review_status(key, REVIEWER_ROLES, REVIEWS_DIR)
      n_reviewed <- sum(status)
      n_total    <- length(status)
      if (n_reviewed == 0) return("\u2014")
      paste0(n_reviewed, "/", n_total, " reviewed")
    }, character(1))

    disp <- data.frame(
      `TFL Name`      = reg$name,
      Type             = reg$type,
      `Datasets Used`  = reg$datasets,
      Description      = reg$description,
      Status           = reg$status,
      `Run (s)`        = reg$run_time_secs,
      `Review Status`  = review_status,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )

    datatable(
      disp,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength = 20,
        dom        = "frtip",
        columnDefs = list(list(width = "30%", targets = 0))
      )
    )
  })

  # When a row is selected in the registry, switch to viewer and load that TFL
  observeEvent(input$registry_table_rows_selected, {
    row <- input$registry_table_rows_selected
    req(row, rv$registry)
    key <- rv$registry$tfl_key[row]
    updateSelectInput(session, "sel_tfl", selected = key)
    nav_select("main_nav", "tab_viewer", session = session)
  })


  # ---------------------------------------------------------------------------
  # 3. CURRENT TFL  (reactive – changes with sel_tfl dropdown)
  # ---------------------------------------------------------------------------

  current_tfl <- reactive({
    req(input$sel_tfl, rv$tfl_list)
    rv$tfl_list[[input$sel_tfl]]
  })


  # ---------------------------------------------------------------------------
  # 4. SIDEBAR: TFL INFO CARD
  # ---------------------------------------------------------------------------

  output$tfl_info_card_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)
    m   <- tfl$metadata
    status_class <- if (tfl$success) "text-success" else "text-danger"

    card(
      class = "mt-2 mb-0",
      card_body(
        class = "py-2 px-3",
        tags$p(class = "fw-semibold mb-1 small", m$name),
        tags$p(class = "text-muted small mb-1", m$description),
        tags$div(
          class = "d-flex gap-2 flex-wrap",
          tags$span(class = "badge bg-secondary",
                    icon(tfl_type_icon(m$type)), " ", m$type),
          lapply(m$datasets, function(ds)
            tags$span(class = "badge bg-light text-dark border", ds)
          )
        ),
        if (!tfl$success)
          tags$p(class = "text-danger small mt-1 mb-0",
                 icon("triangle-exclamation"), " Error: ", tfl$error)
      )
    )
  })


  # ---------------------------------------------------------------------------
  # 5. SIDEBAR: DATASET SELECTOR
  # ---------------------------------------------------------------------------
  # When a TFL uses multiple datasets, let the user pick which one to explore.

  # Which dataset is currently selected for exploration
  sel_dataset_name <- reactiveVal(NULL)

  output$dataset_selector_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl, tfl$source_datasets)
    ds_names <- names(tfl$source_datasets)

    # Initialise (or reset) the selected dataset reactiveVal
    if (is.null(sel_dataset_name()) || !sel_dataset_name() %in% ds_names) {
      sel_dataset_name(ds_names[1])
    }

    if (length(ds_names) == 1L) return(NULL)   # No selector needed for 1 dataset

    tagList(
      tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                 "Explore Dataset"),
      div(
        class = "btn-group w-100",
        lapply(ds_names, function(nm) {
          active <- if (!is.null(sel_dataset_name()) && sel_dataset_name() == nm)
            " active" else ""
          actionButton(
            inputId = paste0("btn_ds_", nm),
            label   = toupper(nm),
            class   = paste0("btn btn-sm btn-outline-primary", active),
            width   = "100%"
          )
        })
      )
    )
  })

  # Update sel_dataset_name when a dataset button is clicked
  observe({
    tfl <- current_tfl()
    req(tfl, tfl$source_datasets)
    ds_names <- names(tfl$source_datasets)
    lapply(ds_names, function(nm) {
      observeEvent(input[[paste0("btn_ds_", nm)]], {
        sel_dataset_name(nm)
      }, ignoreInit = TRUE)
    })
  })

  # Reset when TFL changes
  observeEvent(input$sel_tfl, {
    sel_dataset_name(NULL)
    rv$rerun_output <- NULL
    rv$rerun_msg    <- NULL
  })

  # The raw (unfiltered) dataset currently selected for exploration
  raw_dataset <- reactive({
    tfl <- current_tfl()
    req(tfl, tfl$source_datasets)
    nm <- sel_dataset_name()
    req(nm, nm %in% names(tfl$source_datasets))
    tfl$source_datasets[[nm]]
  })


  # ---------------------------------------------------------------------------
  # 6. SIDEBAR: POPULATION FLAGS
  # ---------------------------------------------------------------------------
  # Detect any column ending in "FL" in the selected dataset and render
  # a selectInput per flag.

  pop_flags <- reactive({
    detect_pop_flags(raw_dataset())
  })

  output$pop_flags_ui <- renderUI({
    flags <- pop_flags()
    if (length(flags) == 0L) return(NULL)
    df    <- raw_dataset()

    tagList(
      tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                 icon("flag"), " Population Flags"),
      lapply(flags, function(fl) {
        selectInput(
          inputId  = paste0("popfl_", fl),
          label    = fl,
          choices  = safe_unique_vals(df, fl),
          selected = "All",
          width    = "100%"
        )
      })
    )
  })

  # Collect active flag selections (excluding "All")
  active_pop_flags <- reactive({
    flags <- pop_flags()
    if (length(flags) == 0L) return(list())
    vals <- lapply(flags, function(fl) input[[paste0("popfl_", fl)]])
    setNames(vals, flags)
  })

  # Apply population flag filters to the raw dataset
  pop_filtered <- reactive({
    df    <- raw_dataset()
    flags <- active_pop_flags()
    for (fl in names(flags)) {
      val <- flags[[fl]]
      if (!is.null(val) && val != "All") {
        df <- df[!is.na(df[[fl]]) & df[[fl]] == val, ]
      }
    }
    df
  })


  # ---------------------------------------------------------------------------
  # 7. SIDEBAR: QUICK STATS
  # ---------------------------------------------------------------------------

  output$quick_stats_ui <- renderUI({
    df   <- pop_filtered()
    nrow_df <- nrow(df)
    n_flags <- sum(sapply(active_pop_flags(), function(v) !is.null(v) && v != "All"))
    n_subj  <- if ("USUBJID" %in% names(df)) dplyr::n_distinct(df$USUBJID) else NA_integer_

    tagList(
      tags$div(
        class = "d-flex justify-content-between small",
        tags$span(class = "text-muted", "Rows shown"),
        tags$strong(format(nrow_df, big.mark = ","))
      ),
      if (!is.na(n_subj))
        tags$div(
          class = "d-flex justify-content-between small",
          tags$span(class = "text-muted", "Unique subjects"),
          tags$strong(format(n_subj, big.mark = ","))
        ),
      tags$div(
        class = "d-flex justify-content-between small",
        tags$span(class = "text-muted", "Active flag filters"),
        tags$strong(n_flags)
      )
    )
  })


  # ---------------------------------------------------------------------------
  # 8. TFL VIEWER: OUTPUT TAB
  # ---------------------------------------------------------------------------

  output$tfl_header_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)
    m   <- tfl$metadata
    # If a re-run result is available, note that in the header
    rerun_badge <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      tags$span(class = "badge bg-warning text-dark ms-2",
                icon("arrows-rotate"), " Re-run output")
    else NULL

    tagList(
      tags$h5(class = "mb-0", m$name, rerun_badge),
      tags$p(class = "text-muted small mb-0", m$description)
    )
  })

  # Decide what to render (DT table vs plot) and build the appropriate output
  output$tfl_output_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)

    if (!tfl$success) {
      return(div(
        class = "alert alert-danger mt-2",
        icon("triangle-exclamation"), " ",
        strong("TFL script failed: "), tfl$error
      ))
    }

    # Prefer re-run output if available and successful
    out <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      rv$rerun_output$tfl_output
    else
      tfl$tfl_output

    if (inherits(out, "ggplot")) {
      plotOutput("tfl_plot_output", height = "520px")
    } else if (is.data.frame(out)) {
      DTOutput("tfl_datatable_output")
    } else {
      div(class = "alert alert-warning",
          "Unrecognised output type: ", class(out)[1])
    }
  })

  # Render the plot (only evaluated when tfl_output_ui requests it)
  output$tfl_plot_output <- renderPlot({
    tfl <- current_tfl()
    req(tfl, tfl$success)
    out <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      rv$rerun_output$tfl_output else tfl$tfl_output
    req(inherits(out, "ggplot"))
    print(out)
  })

  # Render the DT table
  output$tfl_datatable_output <- renderDT({
    tfl <- current_tfl()
    req(tfl, tfl$success)
    out <- if (!is.null(rv$rerun_output) && rv$rerun_output$success)
      rv$rerun_output$tfl_output else tfl$tfl_output
    req(is.data.frame(out))
    datatable(
      out,
      rownames  = FALSE,
      extensions = c("Buttons", "Scroller"),
      options   = list(
        dom        = "Bfrtip",
        buttons    = list("excel", "csv", "pdf"),
        scrollX    = TRUE,
        scrollY    = "420px",
        scroller   = TRUE,
        pageLength = 25
      ),
      filter = "top"
    )
  })


  # ---------------------------------------------------------------------------
  # 9. DATASET EXPLORER: DATA SUB-TAB
  # ---------------------------------------------------------------------------

  output$explorer_dataset_info_ui <- renderUI({
    df <- pop_filtered()
    nm <- sel_dataset_name()
    tags$div(
      class = "d-flex gap-3 align-items-center small text-muted mb-1",
      tags$strong(toupper(nm %||% "—")),
      tags$span(format(nrow(df), big.mark = ","), " rows"),
      tags$span(ncol(df), " columns"),
      if ("USUBJID" %in% names(df))
        tags$span(dplyr::n_distinct(df$USUBJID), " subjects")
    )
  })

  output$explorer_table <- renderDT({
    df <- pop_filtered()
    datatable(
      df,
      rownames   = FALSE,
      extensions = c("Buttons", "Scroller"),
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("excel", "csv"),
        scrollX    = TRUE,
        scrollY    = "450px",
        scroller   = TRUE,
        pageLength = 25
      ),
      filter = "top"
    )
  })


  # ---------------------------------------------------------------------------
  # 10. INVESTIGATE: UNIQUE VALUES
  # ---------------------------------------------------------------------------

  # Populate column selector with all columns from the filtered dataset
  observe({
    df <- pop_filtered()
    updateSelectInput(session, "inv_col",
                      choices  = names(df),
                      selected = names(df)[1])
  })

  # Count table
  output$inv_unique_table <- renderDT({
    req(input$inv_col)
    df  <- pop_filtered()
    col <- input$inv_col
    req(col %in% names(df))

    cnt <- as.data.frame(table(df[[col]], useNA = "ifany"),
                         stringsAsFactors = FALSE)
    colnames(cnt) <- c("Value", "Count")
    cnt$`%` <- round(100 * cnt$Count / sum(cnt$Count), 1)
    cnt      <- cnt[order(-cnt$Count), ]

    datatable(
      cnt,
      rownames  = FALSE,
      options   = list(dom = "tip", pageLength = 10, scrollY = "200px",
                       scroller = TRUE)
    )
  })

  # Bar chart for the selected column
  output$inv_unique_plot <- renderPlot({
    req(input$inv_col)
    df  <- pop_filtered()
    col <- input$inv_col
    req(col %in% names(df), nrow(df) > 0)

    # Only plot if <= 30 unique values (avoids illegible charts)
    uniq <- unique(na.omit(df[[col]]))
    if (length(uniq) > 30) {
      plot.new()
      text(0.5, 0.5, paste("Too many unique values\nto display (", length(uniq), ")"),
           cex = 1.1, col = "grey50")
      return(invisible(NULL))
    }

    ggplot(df, aes(y = forcats::fct_rev(forcats::fct_infreq(.data[[col]])))) +
      geom_bar(fill = "#4E79A7") +
      labs(x = "Count", y = col) +
      theme_bw(base_size = 11) +
      theme(panel.grid.major.y = element_blank())
  })


  # ---------------------------------------------------------------------------
  # 11. INVESTIGATE: PATIENT LISTING
  # ---------------------------------------------------------------------------

  # Populate USUBJID selector
  observe({
    df <- pop_filtered()
    choices <- if ("USUBJID" %in% names(df))
      sort(unique(df$USUBJID))
    else
      character(0)
    updateSelectInput(session, "inv_usubjid",
                      choices  = choices,
                      selected = choices[1])
  })

  output$inv_patient_info_ui <- renderUI({
    req(input$inv_usubjid)
    # Show how many rows this subject has in the current dataset
    df   <- pop_filtered()
    subj <- input$inv_usubjid
    n    <- sum(df$USUBJID == subj, na.rm = TRUE)
    tags$p(class = "text-muted small mb-1",
           n, " row(s) for subject ", tags$strong(subj))
  })

  output$inv_patient_table <- renderDT({
    req(input$inv_usubjid)
    df   <- pop_filtered()
    subj <- input$inv_usubjid
    req("USUBJID" %in% names(df))
    patient_df <- df[df$USUBJID == subj, ]
    datatable(
      patient_df,
      rownames = FALSE,
      options  = list(dom = "tip", pageLength = 15,
                       scrollX = TRUE, scrollY = "220px", scroller = TRUE)
    )
  })


  # ---------------------------------------------------------------------------
  # 12. INVESTIGATE: EXPLORATORY PLOT BUILDER
  # ---------------------------------------------------------------------------

  # Update axis selectors when the dataset changes
  observe({
    df       <- pop_filtered()
    all_cols <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]

    updateSelectInput(session, "inv_plot_x",     choices = all_cols, selected = all_cols[1])
    updateSelectInput(session, "inv_plot_y",     choices = c("—" = "", num_cols), selected = "")
    updateSelectInput(session, "inv_plot_color", choices = c("None" = "", all_cols), selected = "")
  })

  output$inv_custom_plot <- renderPlot({
    req(input$inv_plot_x)
    df    <- pop_filtered()
    req(nrow(df) > 0)
    x     <- input$inv_plot_x
    y     <- input$inv_plot_y
    col   <- input$inv_plot_color
    ptype <- input$inv_plot_type

    req(x %in% names(df))

    # Limit categorical axis to ≤50 levels to keep plots legible
    if (!is.numeric(df[[x]]) && dplyr::n_distinct(df[[x]]) > 50) {
      plot.new()
      text(0.5, 0.5, "X-axis has > 50 unique values.\nChoose a column with fewer categories.",
           cex = 1.1, col = "grey50")
      return(invisible(NULL))
    }

    aes_base <- if (!is.null(col) && col != "" && col %in% names(df)) {
      ggplot2::aes(x = .data[[x]], colour = .data[[col]], fill = .data[[col]])
    } else {
      ggplot2::aes(x = .data[[x]])
    }

    p <- ggplot2::ggplot(df, aes_base)

    p <- switch(ptype,
      "Bar" = {
        p + ggplot2::geom_bar(position = "dodge")
      },
      "Histogram" = {
        if (!is.numeric(df[[x]])) {
          return(plot.new())
        }
        p + ggplot2::geom_histogram(bins = 25, colour = "white", alpha = 0.8)
      },
      "Box" = {
        req(!is.null(y) && y != "" && y %in% names(df) && is.numeric(df[[y]]))
        ggplot2::ggplot(
          df,
          if (!is.null(col) && col != "" && col %in% names(df))
            ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[col]])
          else
            ggplot2::aes(x = .data[[x]], y = .data[[y]])
        ) +
          ggplot2::geom_boxplot(outlier.size = 1, alpha = 0.8) +
          ggplot2::labs(y = y)
      },
      "Scatter" = {
        req(!is.null(y) && y != "" && y %in% names(df) && is.numeric(df[[y]]))
        ggplot2::ggplot(
          df,
          if (!is.null(col) && col != "" && col %in% names(df))
            ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[col]])
          else
            ggplot2::aes(x = .data[[x]], y = .data[[y]])
        ) +
          ggplot2::geom_point(alpha = 0.5, size = 1.5) +
          ggplot2::labs(y = y)
      },
      p   # fallback
    )

    p + ggplot2::labs(x = x) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position  = "bottom",
        axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
        panel.grid.minor = ggplot2::element_blank()
      )
  })


  # ---------------------------------------------------------------------------
  # 13. CODE TABS
  # ---------------------------------------------------------------------------

  output$tfl_script_code <- renderText({
    tfl <- current_tfl()
    req(tfl)
    paste(tfl$script_lines, collapse = "\n")
  })

  output$filter_code_static <- renderText({
    tfl <- current_tfl()
    req(tfl, tfl$filter_code)
    tfl$filter_code
  })

  output$filter_code_live <- renderText({
    nm     <- sel_dataset_name()
    flags  <- active_pop_flags()
    build_filter_code(
      dataset_name = nm %||% "dataset",
      pop_filters  = flags
    )
  })


  # ---------------------------------------------------------------------------
  # 14. RE-RUN TFL WITH CURRENT FILTERS
  # ---------------------------------------------------------------------------

  observeEvent(input$btn_rerun_tfl, {
    tfl <- isolate(current_tfl())
    req(tfl, tfl$success)

    rv$rerun_msg <- "running"

    # Build filtered versions of every source dataset for this TFL
    filtered_ds <- lapply(names(tfl$source_datasets), function(nm) {
      ds    <- tfl$source_datasets[[nm]]
      flags <- isolate(active_pop_flags())
      for (fl in names(flags)) {
        val <- flags[[fl]]
        if (!is.null(val) && val != "All" && fl %in% names(ds)) {
          ds <- ds[!is.na(ds[[fl]]) & ds[[fl]] == val, ]
        }
      }
      ds
    })
    names(filtered_ds) <- names(tfl$source_datasets)

    # Find the script path on disk
    script_path <- file.path(TFL_DIR, paste0(isolate(input$sel_tfl), ".R"))

    if (!file.exists(script_path)) {
      rv$rerun_msg <- paste("Script not found:", script_path)
      return()
    }

    result <- tryCatch(
      rerun_tfl_with_population(script_path, filtered_ds, adam),
      error = function(e) list(success = FALSE, error = conditionMessage(e))
    )

    rv$rerun_output <- result
    rv$rerun_msg    <- if (result$success) "success" else result$error

    # Switch to Output tab so user can see the result
    nav_select("viewer_tabs", "vtab_output", session = session)
  })

  output$rerun_status_ui <- renderUI({
    msg <- rv$rerun_msg
    if (is.null(msg)) return(NULL)
    if (msg == "running")
      tags$span(class = "text-warning small",
                icon("spinner"), " Running...")
    else if (msg == "success")
      tags$span(class = "text-success small",
                icon("circle-check"), " Re-run successful. Output tab updated.")
    else
      tags$span(class = "text-danger small",
                icon("triangle-exclamation"), " Error: ", msg)
  })


  # ---------------------------------------------------------------------------
  # 15. DOWNLOAD: FILTERED DATA
  # ---------------------------------------------------------------------------

  output$dl_filtered_data <- downloadHandler(
    filename = function() {
      nm   <- sel_dataset_name() %||% "data"
      date <- format(Sys.Date(), "%Y-%m-%d")
      paste0(nm, "_filtered_", date, ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(pop_filtered(), file)
    }
  )


  # ---------------------------------------------------------------------------
  # 16-A. SAVE AS VARIANT BUTTON (v3)
  # ---------------------------------------------------------------------------
  # Show only when a TFL is selected and at least one flag is active

  output$save_variant_ui <- renderUI({
    tfl    <- current_tfl()
    flags  <- active_pop_flags()
    n_active <- sum(sapply(flags, function(v) !is.null(v) && v != "All"))
    if (is.null(tfl) || !tfl$success || n_active == 0L) return(NULL)

    tagList(
      tags$label(class = "fw-semibold text-muted small text-uppercase mb-1",
                 icon("code-fork"), " Variants"),
      actionButton(
        "btn_save_variant",
        label = tagList(icon("code-fork"), " Save as Variant"),
        class = "btn btn-outline-secondary btn-sm w-100"
      ),
      tags$div(class = "text-muted small mt-1",
               "Bake current filters into a new TFL variant.")
    )
  })

  # Open the Variant Creator modal
  observeEvent(input$btn_save_variant, {
    tfl    <- isolate(current_tfl())
    flags  <- isolate(active_pop_flags())
    req(tfl, tfl$success)

    base_id <- tfl$metadata$id
    suf     <- next_variant_suffix(base_id, TFL_DIR)

    # Build filter checkbox choices from active flags
    active_flags <- Filter(function(v) !is.null(v) && v != "All", flags)
    filter_choices <- setNames(
      names(active_flags),
      paste0(names(active_flags), " = ", unlist(active_flags))
    )

    showModal(modalDialog(
      title = tagList(icon("code-fork"), " Save as Variant"),
      size  = "m",
      easyClose = TRUE,

      tags$p(
        class = "text-muted small mb-3",
        tags$strong("Base TFL:"), tfl$metadata$name
      ),

      fluidRow(
        column(6,
          textInput("variant_suffix", "Variant suffix",
                    value = suf, width = "100%",
                    placeholder = "a, b, c…")
        ),
        column(6,
          textInput("variant_label", "Variant label",
                    value = "", width = "100%",
                    placeholder = "e.g. Elderly Subgroup")
        )
      ),

      textAreaInput("variant_desc", "Description",
                    value = tfl$metadata$description,
                    width = "100%", rows = 2),

      checkboxGroupInput(
        "variant_baked_filters",
        "Filters to bake into the variant:",
        choices  = filter_choices,
        selected = names(filter_choices)
      ),

      tags$div(
        class = "alert alert-warning small mt-2 mb-0",
        icon("triangle-exclamation"), " ",
        "This writes a new .R file to the ", tags$code("tfls/"), " directory.",
        " Intended for development environments only."
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_confirm_variant", "Create Variant",
                     class = "btn btn-primary", icon = icon("check"))
      )
    ))
  })

  # Confirm: generate the variant script + ARS JSON
  observeEvent(input$btn_confirm_variant, {
    tfl    <- isolate(current_tfl())
    flags  <- isolate(active_pop_flags())
    req(tfl, tfl$success)

    suffix <- trimws(isolate(input$variant_suffix))
    label  <- trimws(isolate(input$variant_label))
    desc   <- trimws(isolate(input$variant_desc))
    baked  <- isolate(input$variant_baked_filters)

    if (nchar(suffix) == 0L) {
      showNotification("Please enter a variant suffix.", type = "error")
      return()
    }
    if (nchar(label) == 0L) {
      showNotification("Please enter a variant label.", type = "error")
      return()
    }

    base_id    <- tfl$metadata$id
    variant_id <- paste0(base_id, "_", suffix)

    # Build the baked filter list (only selected flags)
    active_flags  <- Filter(function(v) !is.null(v) && v != "All", flags)
    baked_filters <- active_flags[names(active_flags) %in% baked]

    # Guard: don't overwrite existing variant
    variant_path <- file.path(TFL_DIR, paste0("tfl_", variant_id, ".R"))
    if (file.exists(variant_path)) {
      showNotification(
        paste0("Variant already exists: tfl_", variant_id, ".R — choose a different suffix."),
        type = "error", duration = 8
      )
      return()
    }

    tryCatch({
      # 1. Write the variant .R script
      generate_variant_script(
        base_result   = tfl,
        variant_id    = variant_id,
        variant_label = label,
        variant_desc  = desc,
        baked_filters = baked_filters,
        tfl_dir       = TFL_DIR
      )

      # 2. Write the ARS JSON for the variant
      ars_obj <- build_ars_json(
        tfl_result     = tfl,
        active_filters = baked_filters,
        study_id       = STUDY_ID
      )
      # Patch the id in the ARS JSON to reflect the variant
      ars_obj$reportingEvent$outputs[[1]]$id   <- paste0("OUT-", variant_id)
      ars_obj$reportingEvent$userSession$baseTflId <- variant_id
      write_ars_json(ars_obj, output_dir = ARS_OUTPUT_DIR, tfl_id = variant_id)

      removeModal()
      showNotification(
        tagList(
          icon("circle-check"), " Variant created: ",
          tags$strong(paste0("tfl_", variant_id, ".R")),
          " — click Refresh to load it."
        ),
        type = "message", duration = 8
      )

    }, error = function(e) {
      showNotification(paste("Error creating variant:", e$message),
                       type = "error", duration = 10)
    })
  })


  # ---------------------------------------------------------------------------
  # 16-B. ARS JSON EXPORT (v3)
  # ---------------------------------------------------------------------------

  output$btn_export_ars <- downloadHandler(
    filename = function() {
      tfl_id <- current_tfl()$metadata$id %||% "tfl"
      paste0("ars_", tfl_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      tfl    <- isolate(current_tfl())
      flags  <- isolate(active_pop_flags())
      req(tfl)

      ars_obj <- build_ars_json(
        tfl_result     = tfl,
        active_filters = flags,
        study_id       = STUDY_ID
      )
      writeLines(ars_to_json_string(ars_obj), file)
    }
  )


  # ---------------------------------------------------------------------------
  # 17. SOURCE DATA TAB (filtered dataset behind the TFL)
  # ---------------------------------------------------------------------------

  output$source_data_info_ui <- renderUI({
    tfl <- current_tfl()
    req(tfl)
    ad <- tfl$analysis_data

    if (is.null(ad) || !is.data.frame(ad)) {
      return(tags$div(
        class = "alert alert-info small",
        icon("circle-info"), " ",
        "This TFL script does not expose an ", tags$code("analysis_data"),
        " object. To enable this view, ensure the TFL script assigns the ",
        "filtered dataset to a variable named ", tags$code("analysis_data"), "."
      ))
    }

    n_subj <- if ("USUBJID" %in% names(ad)) dplyr::n_distinct(ad$USUBJID) else NA
    tags$div(
      class = "d-flex gap-3 align-items-center small text-muted mb-1",
      tags$strong("analysis_data"),
      tags$span(format(nrow(ad), big.mark = ","), " rows"),
      tags$span(ncol(ad), " columns"),
      if (!is.na(n_subj)) tags$span(n_subj, " subjects")
    )
  })

  output$source_data_table <- renderDT({
    tfl <- current_tfl()
    req(tfl, is.data.frame(tfl$analysis_data))
    datatable(
      tfl$analysis_data,
      rownames   = FALSE,
      extensions = c("Buttons", "Scroller"),
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("excel", "csv"),
        scrollX    = TRUE,
        scrollY    = "450px",
        scroller   = TRUE,
        pageLength = 25
      ),
      filter = "top"
    )
  })


  # ---------------------------------------------------------------------------
  # 18. REVIEWS & AUDIT TRAIL
  # ---------------------------------------------------------------------------

  # Load reviews for the currently selected TFL
  tfl_reviews <- reactive({
    rv$review_trigger   # dependency: re-read on submit
    key <- input$sel_tfl
    req(key)
    load_reviews(key, REVIEWS_DIR)
  })

  # --- 18a. Audit status bar ---
  output$audit_status_ui <- renderUI({
    revs  <- tfl_reviews()
    roles_seen <- unique(vapply(revs, function(r) r$role %||% "", character(1)))

    badges <- lapply(REVIEWER_ROLES, function(role) {
      reviewed <- role %in% roles_seen
      # Find reviewer name for tooltip
      reviewer_name <- ""
      if (reviewed) {
        for (r in revs) {
          if (!is.null(r$role) && r$role == role) {
            reviewer_name <- r$reviewer %||% ""
            break
          }
        }
      }

      if (reviewed) {
        tags$span(
          class = "badge bg-success me-1 mb-1",
          title = paste0("Reviewed by: ", reviewer_name),
          icon("circle-check"), " ", role
        )
      } else {
        tags$span(
          class = "badge bg-light text-muted border me-1 mb-1",
          title = "Not yet reviewed",
          icon("circle-xmark"), " ", role
        )
      }
    })

    tags$div(class = "d-flex flex-wrap", badges)
  })

  # --- 18b. Comment views: threaded + per-reviewer ---

  # Track which view is active: "threads" (default) or "reviewers"
  review_view_mode <- reactiveVal("threads")

  observeEvent(input$btn_view_threads, {
    review_view_mode("threads")
  })
  observeEvent(input$btn_view_reviewers, {
    review_view_mode("reviewers")
  })

  # Role badge colour mapping (used in comment display)
  role_colour <- function(role) {
    switch(role,
      "Statistics Lead"          = "bg-primary",
      "Safety Lead"              = "bg-danger",
      "Medical Lead"             = "bg-info",
      "Programming Lead"         = "bg-warning text-dark",
      "Clinical Operations Lead" = "bg-secondary",
      "Trial Programmer"         = "bg-success",
      "bg-dark"
    )
  }

  output$reviews_list_ui <- renderUI({
    if (review_view_mode() != "threads") return(NULL)

    revs <- tfl_reviews()

    if (length(revs) == 0) {
      return(tags$div(
        class = "text-muted small fst-italic py-3",
        icon("circle-info"),
        " No comments yet. Be the first to leave a review."
      ))
    }

    # Separate top-level comments and responses
    top_level <- Filter(function(r) is_top_level_comment(r$parent_id), revs)
    responses <- Filter(function(r) !is_top_level_comment(r$parent_id), revs)

    # Build a lookup of parent_id -> list of responses
    resp_by_parent <- list()
    for (r in responses) {
      pid <- r$parent_id
      resp_by_parent[[pid]] <- c(resp_by_parent[[pid]], list(r))
    }

    # Render a single comment card
    render_comment <- function(cmt, is_reply = FALSE) {
      ts_display <- if (!is.null(cmt$timestamp))
        format(as.POSIXct(cmt$timestamp, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d %H:%M")
      else ""

      card_class <- if (is_reply)
        "ms-4 mb-2 border-start border-3 border-primary"
      else
        "mb-2"

      # Reply button uses Shiny.setInputValue with event priority so it
      # always fires, even if the same button is clicked twice in a row
      reply_btn <- if (!is_reply) {
        cmt_id_js <- gsub("'", "\\\\'", cmt$id)
        tags$button(
          type    = "button",
          class   = "btn btn-outline-secondary btn-sm py-0 px-2",
          style   = "font-size: 0.7rem;",
          onclick = sprintf(
            "Shiny.setInputValue('reply_to', '%s', {priority: 'event'})",
            cmt_id_js
          ),
          icon("reply"), " Reply"
        )
      }

      tags$div(
        class = paste("card", card_class),
        tags$div(
          class = "card-body py-2 px-3",
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-1",
            tags$div(
              tags$span(class = paste("badge", role_colour(cmt$role %||% "")),
                        cmt$role),
              tags$strong(class = "ms-2 small", cmt$reviewer)
            ),
            tags$small(class = "text-muted", ts_display)
          ),
          tags$p(class = "mb-1 small", cmt$text),
          reply_btn
        )
      )
    }

    # Build the threaded UI
    comment_blocks <- lapply(top_level, function(cmt) {
      child_replies <- resp_by_parent[[cmt$id]] %||% list()
      tagList(
        render_comment(cmt, is_reply = FALSE),
        lapply(child_replies, function(r) render_comment(r, is_reply = TRUE))
      )
    })

    tags$div(
      style = "max-height: 400px; overflow-y: auto;",
      comment_blocks
    )
  })

  # --- 18b-2. Per-reviewer summary view ---
  output$reviewer_summary_ui <- renderUI({
    if (review_view_mode() != "reviewers") return(NULL)

    revs <- tfl_reviews()
    if (length(revs) == 0) return(NULL)

    # Group comments by reviewer (role + name)
    reviewer_map <- list()
    for (r in revs) {
      key <- paste0(r$role %||% "Unknown", " | ", r$reviewer %||% "Unknown")
      reviewer_map[[key]] <- c(reviewer_map[[key]], list(r))
    }

    reviewer_cards <- lapply(names(reviewer_map), function(rkey) {
      cmts <- reviewer_map[[rkey]]
      first <- cmts[[1]]
      n_comments  <- sum(vapply(cmts, function(c)
        is_top_level_comment(c$parent_id), logical(1)))
      n_responses <- length(cmts) - n_comments

      # Most recent timestamp
      timestamps <- vapply(cmts, function(c) c$timestamp %||% "", character(1))
      latest <- max(timestamps)
      latest_display <- format(
        as.POSIXct(latest, format = "%Y-%m-%dT%H:%M:%S"),
        "%Y-%m-%d %H:%M"
      )

      tags$div(
        class = "card mb-2",
        tags$div(
          class = "card-body py-2 px-3",
          tags$div(
            class = "d-flex justify-content-between align-items-center",
            tags$div(
              tags$span(class = paste("badge", role_colour(first$role %||% "")),
                        first$role),
              tags$strong(class = "ms-2 small", first$reviewer)
            ),
            tags$small(class = "text-muted", "Last activity: ", latest_display)
          ),
          tags$div(
            class = "mt-1 small text-muted",
            tags$span(class = "me-3",
                      icon("comment"), " ", n_comments, " comment(s)"),
            tags$span(icon("reply"), " ", n_responses, " response(s)")
          )
        )
      )
    })

    tags$div(reviewer_cards)
  })

  # --- 18c. Reply modal (triggered via Shiny.setInputValue from JS) ---
  # reactiveVal to hold the parent comment being replied to
  reply_parent <- reactiveVal(NULL)

  observeEvent(input$reply_to, {
    parent_id <- input$reply_to
    req(parent_id)

    # Find the parent comment object
    revs <- isolate(tfl_reviews())
    parent_cmt <- NULL
    for (r in revs) {
      if (!is.null(r$id) && r$id == parent_id) {
        parent_cmt <- r
        break
      }
    }
    req(parent_cmt)

    reply_parent(parent_id)

    # Pre-fill from main reviewer identity if available
    prefill_name <- isolate(input$reviewer_name %||% "")
    prefill_role <- isolate(input$reviewer_role %||% "")

    showModal(modalDialog(
      title = tagList(icon("reply"), " Reply to ", parent_cmt$reviewer),
      size  = "m",
      easyClose = TRUE,

      tags$div(
        class = "card bg-light mb-3",
        tags$div(
          class = "card-body py-2 px-3 small",
          tags$div(
            tags$span(class = paste("badge", role_colour(parent_cmt$role %||% "")),
                      parent_cmt$role),
            tags$strong(class = "ms-2", parent_cmt$reviewer)
          ),
          tags$p(class = "mb-0 mt-1", parent_cmt$text)
        )
      ),

      # Responder identity fields
      tags$h6(class = "text-muted small fw-semibold mt-2",
              icon("user"), " Responder Identity"),
      layout_column_wrap(
        width = 1 / 2,
        textInput("reply_name", "Your Name", value = prefill_name,
                  placeholder = "e.g. Jane Smith", width = "100%"),
        selectInput("reply_role", "Your Role", width = "100%",
                    selected = prefill_role,
                    choices = c("Select role..." = "", REVIEWER_ROLES))
      ),

      textAreaInput("reply_text", "Your Response",
                    value = "", placeholder = "Write your response...",
                    width = "100%", rows = 3),

      # JS to ensure the text area gets focus after modal animation
      tags$script(HTML(
        "setTimeout(function() { $('#reply_text').focus(); }, 500);"
      )),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_submit_reply", "Submit Reply",
                     class = "btn btn-primary", icon = icon("paper-plane"))
      )
    ))
  })

  # --- 18d. Submit new top-level comment ---
  observeEvent(input$btn_submit_review, {
    reviewer <- trimws(input$reviewer_name %||% "")
    role     <- input$reviewer_role %||% ""
    text     <- trimws(input$review_text %||% "")
    key      <- input$sel_tfl

    if (nchar(reviewer) == 0) {
      showNotification("Please enter your name in the Reviewer Identity section.", type = "error")
      return()
    }
    if (nchar(role) == 0 || role == "Select role...") {
      showNotification("Please select your role.", type = "error")
      return()
    }
    if (nchar(text) == 0) {
      showNotification("Please write a comment before submitting.", type = "error")
      return()
    }

    comment <- list(
      id        = build_review_id(),
      tfl_key   = key,
      reviewer  = reviewer,
      role      = role,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      text      = text,
      parent_id = ""
    )

    save_review(key, comment, REVIEWS_DIR)

    # Clear the text input and refresh
    updateTextAreaInput(session, "review_text", value = "")
    rv$review_trigger <- isolate(rv$review_trigger) + 1L

    showNotification(
      tagList(icon("circle-check"), " Comment saved."),
      type = "message", duration = 3
    )
  })

  # --- 18e. Submit reply (reads from modal's own identity fields) ---
  observeEvent(input$btn_submit_reply, {
    reviewer  <- trimws(input$reply_name %||% "")
    role      <- input$reply_role %||% ""
    text      <- trimws(input$reply_text %||% "")
    parent_id <- reply_parent() %||% ""
    key       <- input$sel_tfl

    if (nchar(reviewer) == 0) {
      showNotification("Please enter your name in the Responder Identity fields.", type = "error")
      return()
    }
    if (nchar(role) == 0 || role == "Select role...") {
      showNotification("Please select your role.", type = "error")
      return()
    }
    if (nchar(text) == 0) {
      showNotification("Please write a response.", type = "error")
      return()
    }

    comment <- list(
      id        = build_review_id(),
      tfl_key   = key,
      reviewer  = reviewer,
      role      = role,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      text      = text,
      parent_id = parent_id
    )

    save_review(key, comment, REVIEWS_DIR)

    removeModal()
    rv$review_trigger <- isolate(rv$review_trigger) + 1L

    showNotification(
      tagList(icon("circle-check"), " Reply saved."),
      type = "message", duration = 3
    )
  })

  # --- 18f. Excel audit export ---
  output$dl_review_audit <- downloadHandler(
    filename = function() {
      paste0("review_audit_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      compile_reviews_excel(
        tfl_registry  = rv$registry,
        reviewer_roles = REVIEWER_ROLES,
        reviews_dir    = REVIEWS_DIR,
        output_file    = file
      )
    }
  )


  # ---------------------------------------------------------------------------
  # 19. ABOUT TAB
  # ---------------------------------------------------------------------------

  output$about_study_id <- renderText(STUDY_ID)

  output$about_dataset_summary <- renderTable({
    data.frame(
      Dataset     = c("ADSL", "ADAE", "ADLB", "ADTTE"),
      Description = c(
        "Subject Level Analysis",
        "Adverse Events Analysis",
        "Laboratory Data Analysis",
        "Time-to-Event Analysis"
      ),
      Rows        = c(nrow(adsl), nrow(adae), nrow(adlb), nrow(adtte)),
      Columns     = c(ncol(adsl), ncol(adae), ncol(adlb), ncol(adtte)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

}
