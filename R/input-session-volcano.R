# =============================================================================
# FLAME Volcano Input Session
# =============================================================================
#
# Manages the Volcano input tab and plot panel for creating gene lists
# from volcano plot point selection.
#
# UI Components:
# - Input tab: file upload, example button, data table preview
# - Plot panel: column selectors, sliders, plot, selection, add to list
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist-registry.R (for AnalyteListRegistry)
#
# =============================================================================

# =============================================================================
# VOLCANO INPUT UI FUNCTIONS
# =============================================================================

#' Generate the Volcano input tab panel UI
#' @param id Module namespace ID
#' @return A Shiny tabPanel
volcanoInputUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Volcano",
    icon = shiny::icon("volcano"),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::fileInput(ns("volcanoUpload"), "Upload gene fold change file:",
                        multiple = FALSE,
                        accept = c(".tsv", ".csv", ".txt")) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Upload file with at least: 1 column for gene identifiers + 2 numeric columns (logFC, p-value). You'll select which columns to use after upload."
              )
          ),
        shiny::actionButton(ns("addExample"), "Example", shiny::icon("bookmark"))
      ),
      shiny::column(
        8,
        DT::dataTableOutput(ns("volcanoViewer"))
      )
    )
  )
}

#' Generate the Volcano plot panel UI
#' @param id Module namespace ID
#' @return A Shiny tabPanel
volcanoPlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Volcano Plot",
    icon = shiny::icon("volcano"),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::textOutput(ns("volcanoSelectionInfo")),
        plotly::plotlyOutput(ns("volcanoPlot"), height = "750px")
      ),
      shiny::column(
        3,
        shiny::tags$div(
          id = ns("volcanoPanel"),
          shiny::tags$h5("Plot Controls", class = "section-header"),
            shiny::selectInput(
              inputId = ns("volcano_gene_col"),
              label = "Gene column *:",
              choices = c(""),
              selected = "",
              width = "100%"
            ),
            shiny::selectInput(
              inputId = ns("volcano_logfc_col"),
              label = "LogFC column *:",
              choices = c(""),
              selected = "",
              width = "100%"
            ),
            shiny::selectInput(
              inputId = ns("volcano_pvalue_col"),
              label = "P-value column *:",
              choices = c(""),
              selected = "",
              width = "100%"
            ),
            shiny::sliderInput(
              inputId = ns("volcano_pvalue_slider"),
              label = "Choose -log10Pvalue threshold:",
              min = 0, max = 5,
              value = 1.30103,   # -log10(0.05) standard significance threshold
              step = 0.00001,
              width = "100%"
            ) %>%
              bsplus::shinyInput_label_embed(
                bsplus::shiny_iconlink("circle-info") %>%
                  bsplus::bs_embed_popover(
                    title = "0.05 pvalue == 1.30103 -log10pvalue\n0.01 pvalue == 2 -log10pvalue"
                  )
              ),
            shiny::sliderInput(
              inputId = ns("volcano_fc_slider"),
              label = "Choose |log2FC| threshold:",
              min = 0, max = 5,
              value = 1,         # Standard 2-fold change threshold
              step = 0.001,
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::actionButton(ns("volcanoGenerate"), "Generate Plot",
                               shiny::icon("palette"), class = "submit_button"),
            shiny::actionButton(ns("volcanoClear"), "Clear",
                               shiny::icon("broom"), class = "submit_button"),
            shiny::actionButton(ns("volcano_submit"), "Add to lists",
                               shiny::icon("paper-plane")),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$h5("Plot Information", class = "section-header"),
            shiny::tags$div(
              class = "plot-info-content",
              shiny::textOutput(ns("volcanoPlotStatus")),
              shiny::textOutput(ns("volcanoMetricConversions"))
            ),
            shiny::tags$h5("Selected Genes", class = "section-header"),
            shiny::verbatimTextOutput(ns("volcanoSelected"))
        )
      )
    )
  )
}

# =============================================================================
# VOLCANO INPUT SESSION
# =============================================================================

#' Volcano Input Session Class
#'
#' Manages the Volcano input tab and plot panel for creating gene lists
#' from volcano plot point selection.
#'
#' @section UI Components:
#' - Input tab: file upload, example button, data table preview
#' - Plot panel: column selectors, sliders, plot, selection, add to list
#'
#' @section Responsibilities:
#' - Handle file upload and example data loading
#' - Display data table preview
#' - Manage column selection dropdowns
#' - Handle plot generation with threshold controls
#' - Handle point selection and adding to registry
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' volcanoSession <- VolcanoInputSession$new(ModuleIds$INPUT_VOLCANO, registry)
#' volcanoSession$server(input, session)
#' }
#'
VolcanoInputSession <- R6::R6Class(
  "VolcanoInputSession",

  inherit = InputSession,

  public = list(
    #' Initialize a VolcanoInputSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      super$initialize(id, registry)
      private$.currentVolcano <- NULL
      private$.volcanoPlotData <- NULL
      private$.selectedItems <- character(0)
      private$.tabInserted <- FALSE
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      # Remove dynamically inserted tab if present
      if (private$.tabInserted) {
        tryCatch({
          shiny::removeTab(inputId = "inputPlots", target = "Volcano Plot",
                          session = private$.parentSession)
        }, error = function(e) {
          # Tab may already be removed, ignore
        })
        private$.tabInserted <- FALSE
      }
      private$.currentVolcano <- NULL
      private$.volcanoPlotData <- NULL
      private$.selectedItems <- character(0)
      super$cleanup()
    },

    #' Initialize server logic
    #' @param parentInput Parent session's input object
    #' @param parentSession Parent session object
    server = function(parentInput, parentSession) {
      private$.parentSession <- parentSession

      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session

        # -------------------------------------------------------------------
        # OBSERVERS - Input Tab
        # -------------------------------------------------------------------

        # File upload
        private$.observers$fileUpload <- shiny::observeEvent(input$volcanoUpload, {
          private$handleVolcanoUpload(input, output, session)
        }, ignoreInit = TRUE)

        # Example button
        private$.observers$example <- shiny::observeEvent(input$addExample, {
          private$handleVolcanoExample(input, output, session)
        }, ignoreInit = TRUE)

        # -------------------------------------------------------------------
        # OBSERVERS - Plot Panel
        # -------------------------------------------------------------------

        # Column selection changes (update available choices)
        private$.observers$columnChange <- shiny::observeEvent(
          c(input$volcano_gene_col, input$volcano_logfc_col, input$volcano_pvalue_col),
          {
            private$updateDropdownChoices(input, session)
          },
          ignoreInit = TRUE
        )

        # Slider changes (update metric text)
        private$.observers$sliderChange <- shiny::observeEvent(
          c(input$volcano_pvalue_slider, input$volcano_fc_slider),
          {
            private$updateMetricsText(input, output)
          },
          ignoreInit = TRUE
        )

        # Generate button
        private$.observers$generate <- shiny::observeEvent(input$volcanoGenerate, {
          private$handleGenerate(input, output, session)
        }, ignoreInit = TRUE)

        # Clear button
        private$.observers$clear <- shiny::observeEvent(input$volcanoClear, {
          private$handleClear(input, output, session)
        }, ignoreInit = TRUE)

        # Submit (add to lists) button
        private$.observers$submit <- shiny::observeEvent(input$volcano_submit, {
          private$handleSubmit(input, output, session)
        }, ignoreInit = TRUE)

        # Modal OK button (confirm add to lists)
        private$.observers$confirmAdd <- shiny::observeEvent(input$volcano_ok, {
          private$handleConfirmAdd(input, session)
        }, ignoreInit = TRUE)

        # Plotly selection event (non-namespaced source, matching original)
        private$.observers$plotSelection <- shiny::observeEvent(
          plotly::event_data("plotly_selected", source = "Volcano"),
          {
            private$handlePlotSelection(input, output, session)
          },
          ignoreInit = TRUE
        )

      }) # end moduleServer
    }

    # cleanup() and getRegistry() are inherited from InputSession
  ),

  private = list(
    # Prefix for auto-generated list names
    .prefix = "volcano",

    # Color palette for expression groups
    .colors = c("default" = "#000000",
                "overexpressed" = "#eb6e6c",
                "underexpressed" = "#64e4ed"),

    # Current raw volcano data (from file/example)
    .currentVolcano = NULL,

    # Processed volcano plot data
    .volcanoPlotData = NULL,

    # Currently selected items from plot
    .selectedItems = NULL,

    # Whether the plot tab has been dynamically inserted
    .tabInserted = FALSE,

    # =========================================================================
    # HANDLER METHODS - Input Tab
    # =========================================================================

    handleVolcanoUpload = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Loading Volcano data.</p>")
        volcanoFile <- input$volcanoUpload
        fileType <- tolower(tools::file_ext(volcanoFile$name))

        # Read file based on type
        if (fileType == "tsv" || fileType == "txt") {
          volcanoInput <- read.delim(volcanoFile$datapath, header = TRUE,
                                     na.strings = c("", "NA"))
        } else if (fileType == "csv") {
          volcanoInput <- read.csv(volcanoFile$datapath, header = TRUE,
                                   na.strings = c("", "NA"))
        } else {
          stop("Unsupported file type")
        }

        private$processVolcanoData(volcanoInput, output, session)
      }, error = function(e) {
        cat(paste("[Volcano] File read error:", conditionMessage(e), "\n"))
        renderWarning(paste0(
          "Could not read file. Expected format:\n",
          "  \u2022 .csv (comma-separated) or .tsv/.txt (tab-separated)\n",
          "  \u2022 At least 3 columns\n",
          "  \u2022 At least 2 numeric columns (for logFC and p-value)\n",
          "You'll select which columns to use after upload."
        ))
      }, finally = {
        removeModal()
      })
    },

    handleVolcanoExample = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Loading Volcano example.</p>")
        examplePath <- tryCatch(
          getExamplesPath("volcano_example.RDS"),
          error = function(e) "examples/volcano_example.RDS"
        )
        volcanoInput <- readRDS(examplePath)
        private$processVolcanoData(volcanoInput, output, session)
      }, error = function(e) {
        cat(paste("[Volcano] Example error:", conditionMessage(e), "\n"))
        renderError("Could not load volcano example.")
      }, finally = {
        removeModal()
      })
    },

    processVolcanoData = function(volcanoInput, output, session) {
      # Validate input
      if (!private$isValidVolcanoInput(volcanoInput)) {
        stop("Invalid format")
      }

      # Insert the Volcano Plot tab dynamically if not already inserted
      if (!private$.tabInserted) {
        shiny::insertTab(
          inputId = "inputPlots",
          tab = volcanoPlotUI(self$id),
          target = "UpSet Plot",
          position = "after",
          select = TRUE,
          session = private$.parentSession
        )
        private$.tabInserted <- TRUE
      } else {
        # Tab already exists, just select it
        shiny::updateTabsetPanel(private$.parentSession, "inputPlots",
                                 selected = "Volcano Plot")
      }

      # Render data table preview
      output$volcanoViewer <- DT::renderDataTable(
        volcanoInput,
        server = FALSE,
        selection = "none",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          dom = "Blfiprt",
          buttons = list(
            list(extend = "excel", filename = "volcano"),
            list(extend = "csv", filename = "volcano"),
            list(extend = "copy"),
            list(extend = "pdf", filename = "volcano", orientation = "landscape"),
            list(extend = "print")
          )
        ),
        rownames = FALSE,
        escape = FALSE
      )

      # Store current data
      private$.currentVolcano <- volcanoInput

      # Prepare column dropdowns
      private$prepareColumnDropdowns(volcanoInput, session)

      # Clear plot and reset UI (matching original)
      output$volcanoPlot <- plotly::renderPlotly(c())
      output$volcanoPlotStatus <- shiny::renderText("")
      output$volcanoSelected <- shiny::renderText("")
      output$volcanoSelectionInfo <- shiny::renderText(
        "Pick the box or lasso from the plot toolbar and then select items."
      )
      private$.selectedItems <- character(0)
    },

    isValidVolcanoInput = function(volcanoInput) {
      # Must have at least 3 columns total
      if (ncol(volcanoInput) < 3) {
        return(FALSE)
      }

      # Must have at least 2 numeric columns (for logFC and pvalue)
      numeric_cols <- sapply(volcanoInput, is.numeric)
      if (sum(numeric_cols) < 2) {
        return(FALSE)
      }

      # Check object size (uses inherited method from InputSession)
      if (private$isInvalidObjectSize(volcanoInput)) {
        return(FALSE)
      }

      return(TRUE)
    },

    prepareColumnDropdowns = function(volcanoInput, session) {
      all_cols <- names(volcanoInput)
      numeric_cols <- all_cols[sapply(volcanoInput, is.numeric)]

      # Update Gene column dropdown (all columns)
      shiny::updateSelectInput(session, "volcano_gene_col",
                              choices = c("", all_cols),
                              selected = "")

      # Update LogFC dropdown (numeric only)
      shiny::updateSelectInput(session, "volcano_logfc_col",
                              choices = c("", numeric_cols),
                              selected = "")

      # Update P-value dropdown (numeric only)
      shiny::updateSelectInput(session, "volcano_pvalue_col",
                              choices = c("", numeric_cols),
                              selected = "")
    },

    # =========================================================================
    # HANDLER METHODS - Plot Panel
    # =========================================================================

    updateDropdownChoices = function(input, session) {
      if (is.null(private$.currentVolcano) || nrow(private$.currentVolcano) == 0) {
        return()
      }

      # Get current selections
      gene_col <- input$volcano_gene_col
      logfc_col <- input$volcano_logfc_col
      pvalue_col <- input$volcano_pvalue_col

      # Get all columns
      all_cols <- names(private$.currentVolcano)
      numeric_cols <- all_cols[sapply(private$.currentVolcano, is.numeric)]

      # Build list of selected columns (excluding empty strings)
      selected_cols <- c(gene_col, logfc_col, pvalue_col)
      selected_cols <- selected_cols[selected_cols != "" & !is.null(selected_cols)]

      # Helper function: get available choices excluding other selections
      get_available <- function(base_choices, current_selection) {
        other_selections <- selected_cols[selected_cols != current_selection]
        setdiff(base_choices, other_selections)
      }

      # Update each dropdown with available choices
      shiny::updateSelectInput(session, "volcano_gene_col",
                              choices = c("", get_available(all_cols, gene_col)),
                              selected = gene_col)

      shiny::updateSelectInput(session, "volcano_logfc_col",
                              choices = c("", get_available(numeric_cols, logfc_col)),
                              selected = logfc_col)

      shiny::updateSelectInput(session, "volcano_pvalue_col",
                              choices = c("", get_available(numeric_cols, pvalue_col)),
                              selected = pvalue_col)
    },

    updateMetricsText = function(input, output) {
      pval_threshold <- input$volcano_pvalue_slider
      fc_threshold <- input$volcano_fc_slider

      # Convert -log10(pvalue) to pvalue
      pvalue <- 10^(-pval_threshold)
      pvalue_text <- formatC(pvalue, format = "e", digits = 2)

      # Convert log2FC to fold change
      fc <- 2^fc_threshold

      output$volcanoMetricConversions <- shiny::renderText({
        paste0("-log10(", pvalue_text, ") = ", round(pval_threshold, 2),
               " | 2^", round(fc_threshold, 2), " = ", round(fc, 2), "x fold change")
      })
    },

    handleGenerate = function(input, output, session) {
      tryCatch({
        # Validate required selections
        gene_col <- input$volcano_gene_col
        logfc_col <- input$volcano_logfc_col
        pvalue_col <- input$volcano_pvalue_col

        if (is.null(gene_col) || gene_col == "") {
          renderWarning("Please select the Gene column.")
          return()
        }

        if (is.null(logfc_col) || logfc_col == "") {
          renderWarning("Please select the LogFC column.")
          return()
        }

        if (is.null(pvalue_col) || pvalue_col == "") {
          renderWarning("Please select the P-value column.")
          return()
        }

        renderModal("<h2>Please wait.</h2><br /><p>Generating Volcano plot.</p>")

        # Calculate exclusions and prepare data
        total_genes <- nrow(private$.currentVolcano)
        gene_col_data <- private$.currentVolcano[[gene_col]]
        logfc_col_data <- private$.currentVolcano[[logfc_col]]
        pvalue_col_data <- private$.currentVolcano[[pvalue_col]]

        # Count different types of missing/invalid data
        missing_gene_names <- is.na(gene_col_data) | gene_col_data == ""
        missing_logfc <- is.na(logfc_col_data)
        missing_pvalue <- is.na(pvalue_col_data) | pvalue_col_data <= 0
        excluded <- missing_gene_names | missing_logfc | missing_pvalue

        total_excluded <- sum(excluded)
        total_plotted <- total_genes - total_excluded

        # Build status message
        if (total_excluded > 0) {
          exclusion_parts <- c()
          if (sum(missing_gene_names) > 0) {
            exclusion_parts <- c(exclusion_parts,
                                paste0(sum(missing_gene_names), " missing gene names"))
          }
          if (sum(missing_logfc) > 0) {
            exclusion_parts <- c(exclusion_parts,
                                paste0(sum(missing_logfc), " missing logFC values"))
          }
          if (sum(missing_pvalue) > 0) {
            exclusion_parts <- c(exclusion_parts,
                                paste0(sum(missing_pvalue), " invalid p-values"))
          }

          status_msg <- paste0(
            "Plotting ", total_plotted, " of ", total_genes, " genes (",
            total_excluded, " excluded: ", paste(exclusion_parts, collapse = ", "), ")"
          )
        } else {
          status_msg <- paste0("Plotting ", total_plotted, " genes")
        }

        output$volcanoPlotStatus <- shiny::renderText(status_msg)

        # Prepare plot data
        private$prepareVolcanoForRendering(input)

        # Apply color column based on thresholds
        private$appendColorColumn(input)

        # Render the plot
        private$renderVolcanoPlot(input, output, session)

      }, error = function(e) {
        cat(paste("[Volcano] Generate error:", conditionMessage(e), "\n"))
        renderError("Volcano plot generation error.")
      }, finally = {
        removeModal()
      })
    },

    prepareVolcanoForRendering = function(input) {
      gene_col <- input$volcano_gene_col
      logfc_col <- input$volcano_logfc_col
      pvalue_col <- input$volcano_pvalue_col

      # Filter out invalid rows
      gene_col_data <- private$.currentVolcano[[gene_col]]
      logfc_col_data <- private$.currentVolcano[[logfc_col]]
      pvalue_col_data <- private$.currentVolcano[[pvalue_col]]

      valid_rows <- !is.na(gene_col_data) & gene_col_data != "" &
                    !is.na(logfc_col_data) &
                    !is.na(pvalue_col_data) & pvalue_col_data > 0

      # Create working copy with standardized column names (matching original)
      private$.volcanoPlotData <- data.frame(
        symbol = private$.currentVolcano[[gene_col]][valid_rows],
        logfc = private$.currentVolcano[[logfc_col]][valid_rows],
        `-log10Pvalue` = -log10(private$.currentVolcano[[pvalue_col]][valid_rows]),
        check.names = FALSE
      )

      # Update sliders based on data range (use parent session)
      maxAbsoluteLogFC <- max(
        max(private$.volcanoPlotData$logfc),
        abs(min(private$.volcanoPlotData$logfc))
      )
      maxLog10PValue <- max(private$.volcanoPlotData$`-log10Pvalue`)

      # Update sliders in module scope
      shiny::updateSliderInput(
        private$.moduleSession,
        "volcano_pvalue_slider",
        min = 0,
        max = ceiling(maxLog10PValue)
      )

      shiny::updateSliderInput(
        private$.moduleSession,
        "volcano_fc_slider",
        min = 0,
        max = ceiling(maxAbsoluteLogFC)
      )
    },

    appendColorColumn = function(input) {
      pvalueThreshold <- input$volcano_pvalue_slider
      logFCThreshold <- input$volcano_fc_slider

      private$.volcanoPlotData$expression <- "default"

      overexpressed <- (private$.volcanoPlotData$logfc > logFCThreshold) &
                       (private$.volcanoPlotData$`-log10Pvalue` > pvalueThreshold)
      if (any(overexpressed)) {
        private$.volcanoPlotData$expression[overexpressed] <- "overexpressed"
      }

      underexpressed <- (private$.volcanoPlotData$logfc < -logFCThreshold) &
                        (private$.volcanoPlotData$`-log10Pvalue` > pvalueThreshold)
      if (any(underexpressed)) {
        private$.volcanoPlotData$expression[underexpressed] <- "underexpressed"
      }
    },

    renderVolcanoPlot = function(input, output, session) {
      # Get thresholds and data range for threshold lines
      pvalueThreshold <- input$volcano_pvalue_slider
      logFCThreshold <- input$volcano_fc_slider
      plotData <- private$.volcanoPlotData

      minLog10PValue <- min(plotData$`-log10Pvalue`)
      maxLog10PValue <- max(plotData$`-log10Pvalue`)
      maxLog2FC <- max(plotData$logfc)

      output$volcanoPlot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = plotData,
          x = ~`logfc`,
          y = ~`-log10Pvalue`,
          customdata = ~symbol,
          type = "scatter",
          mode = "markers",
          marker = list(size = 6),
          color = ~expression,
          colors = private$.colors,
          hoverinfo = "text",
          hovertext = ~paste0("<b>Gene</b>: ", symbol,
                              "\nlogFC: ", logfc,
                              "\n-log10Pvalue: ", `-log10Pvalue`),
          source = "Volcano"
        ) %>%
          plotly::layout(
            xaxis = list(title = "log2FC"),
            yaxis = list(title = "-log10Pvalue"),
            showlegend = FALSE,
            dragmode = "lasso",
            shapes = list(
              private$renderLine(minLog10PValue, maxLog10PValue,
                                 logFCThreshold, logFCThreshold),
              private$renderLine(minLog10PValue, maxLog10PValue,
                                 -logFCThreshold, -logFCThreshold),
              private$renderLine(pvalueThreshold, pvalueThreshold,
                                 -maxLog2FC, maxLog2FC)
            )
          )
      })
    },

    # Helper for threshold lines (matching original)
    renderLine = function(y0, y1, x0, x1) {
      list(
        type = "line",
        y0 = y0,
        y1 = y1,
        x0 = x0,
        x1 = x1,
        line = list(color = "red")
      )
    },

    handleClear = function(input, output, session) {
      tryCatch({
        shiny::updateSelectInput(session, "volcano_gene_col", selected = "")
        shiny::updateSelectInput(session, "volcano_logfc_col", selected = "")
        shiny::updateSelectInput(session, "volcano_pvalue_col", selected = "")

        # Clear plot (matching original: renderPlotly(c()))
        output$volcanoPlot <- plotly::renderPlotly(c())
        output$volcanoPlotStatus <- shiny::renderText("")
        output$volcanoSelected <- shiny::renderText("")
        private$.selectedItems <- character(0)
      }, error = function(e) {
        cat(paste("[Volcano] Clear error:", conditionMessage(e), "\n"))
        renderError("Volcano clear error.")
      })
    },

    handlePlotSelection = function(input, output, session) {
      tryCatch({
        triggeredEvent <- plotly::event_data("plotly_selected", source = "Volcano")
        if (is.null(triggeredEvent) || is.null(triggeredEvent$customdata)) return()
        private$.selectedItems <- triggeredEvent$customdata
        output$volcanoSelected <- shiny::renderText(
          paste(private$.selectedItems, collapse = ", ")
        )
      }, error = function(e) {
        cat(paste("[Volcano] Plot selection error:", conditionMessage(e), "\n"))
      })
    },

    handleSubmit = function(input, output, session) {
      tryCatch({
        if (length(private$.selectedItems) == 0) {
          renderWarning("Please select points on the plot using box or lasso selection tool.")
          return()
        }

        shiny::showModal(shiny::modalDialog(
          title = "Volcano selected set",
          paste0("Add the selected items to the lists?"),
          footer = shiny::tagList(
            shiny::actionButton(session$ns("volcano_ok"), "OK"),
            shiny::modalButton("Cancel")
          )
        ))
      }, error = function(e) {
        cat(paste("[Volcano] Submit error:", conditionMessage(e), "\n"))
        renderError("Volcano submit selected list error.")
      })
    },

    handleConfirmAdd = function(input, session) {
      tryCatch({
        # Generate unique list name using inherited method
        listName <- self$generateUniqueName(private$.prefix)

        # Add to registry using inherited method
        self$addAnalyteList(listName, private$.selectedItems)

        # Clear selection after adding
        private$.selectedItems <- character(0)

        shiny::removeModal()
      }, error = function(e) {
        cat(paste("[Volcano] Confirm add error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      })
    }
  )
)
