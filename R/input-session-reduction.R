# =============================================================================
# FLAME Reduction Input Session
# =============================================================================
#
# Manages the 2D Reduction tab for uploading reduction data and selecting
# points from the plot to create gene lists.
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist.R (for AnalyteListRegistry)
#
# =============================================================================

# =============================================================================
# REDUCTION INPUT UI FUNCTIONS
# =============================================================================

#' Generate the reduction upload panel UI
#' @param id Module namespace ID (must match the ID used in ReductionInputSession$new)
#' @return A Shiny tabPanel
reductionInputUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "2D Reduction",
    icon = shiny::icon("braille"),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::fileInput(ns("reductionUpload"), "Upload 2D reduction data:", multiple = FALSE,
                  accept = c(".tsv", ".csv", ".txt")) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Upload file with at least: 1 column for gene identifiers + 2 numeric columns for coordinates (e.g., PC1, PC2). Additional columns for color/size (optional)."
              )
          ),
        shiny::actionButton(ns("reduction_addExample"), "Example", shiny::icon("bookmark"))
      ),
      shiny::column(
        8,
        DT::dataTableOutput(ns("reductionViewer"))
      )
    )
  )
}

#' Generate the reduction plot panel UI
#' @param id Module namespace ID (must match the ID used in ReductionInputSession$new)
#' @return A Shiny tabPanel
reductionPlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "2D Reduction Plot",
    icon = shiny::icon("braille"),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::textOutput(ns("reductionSelectionInfo")),
        plotly::plotlyOutput(ns("reductionPlot"), height = "750px")
      ),
      shiny::column(
        3,
        shiny::tags$div(
          id = ns("reductionPanel"),
          shiny::tags$h5("Plot Controls", class = "section-header"),
          shiny::selectInput(
            inputId = ns("reduction_gene_col"),
            label = "Gene column *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          shiny::selectInput(
            inputId = ns("reduction_x_axis"),
            label = "X axis *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          shiny::selectInput(
            inputId = ns("reduction_y_axis"),
            label = "Y axis *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          shiny::selectInput(
            inputId = ns("reduction_color"),
            label = "Color by:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          shiny::selectInput(
            inputId = ns("reduction_size"),
            label = "Size by:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          shiny::tags$br(),
          shiny::actionButton(ns("reductionGenerate"), "Generate Plot",
                       shiny::icon("palette"), class = "submit_button"),
          shiny::actionButton(ns("reductionClear"), "Clear",
                       shiny::icon("broom"), class = "submit_button"),
          shiny::actionButton(ns("reduction_submit"), "Add to lists", shiny::icon("paper-plane")),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$h5("Plot Information", class = "section-header"),
          shiny::tags$div(
            class = "plot-info-content",
            shiny::textOutput(ns("reductionPlotStatus"))
          ),
          shiny::tags$h5("Selected Genes", class = "section-header"),
          shiny::verbatimTextOutput(ns("reductionSelected"))
        )
      )
    )
  )
}

# =============================================================================
# REDUCTION INPUT SESSION
# =============================================================================

#' Reduction Input Session Class
#'
#' Manages the 2D Reduction tab for uploading reduction data and selecting
#' points from the plot to create gene lists.
#'
#' Inherits from InputSession and uses the shared methods:
#' - addAnalyteList(): Add an analyte list to the registry
#' - generateUniqueName(): Generate unique list names
#' - cleanup(): Destroy observers
#'
#' @section UI Components:
#' - Upload panel: file upload, example button, data table preview
#' - Plot panel: dropdowns for column selection, plot, selection controls
#'
#' @section Dependencies:
#' - AnalyteListRegistry for storing analyte lists
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' reductionInput <- ReductionInputSession$new(ModuleIds$INPUT_REDUCTION, registry)
#' reductionInput$server(input, session)
#' }
#'
ReductionInputSession <- R6::R6Class(
  "ReductionInputSession",

  inherit = InputSession,

  public = list(
    #' Initialize a ReductionInputSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      super$initialize(id, registry)
      private$.currentReduction <- data.frame()
      private$.reductionSelectedItems <- c()
      private$.tabInserted <- FALSE
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      # Remove dynamically inserted tab if present
      if (private$.tabInserted) {
        tryCatch({
          shiny::removeTab(inputId = "inputPlots", target = "2D Reduction Plot",
                          session = private$.parentSession)
        }, error = function(e) {
          # Tab may already be removed, ignore
        })
        private$.tabInserted <- FALSE
      }
      private$.currentReduction <- data.frame()
      private$.reductionSelectedItems <- c()
      super$cleanup()
    },

    # =========================================================================
    # SERVER METHOD
    # =========================================================================

    #' Initialize server logic
    #' @param parentInput Parent session's input object
    #' @param parentSession Parent session object
    server = function(parentInput, parentSession) {
      private$.parentSession <- parentSession

      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session

        # ---------------------------------------------------------------------
        # OBSERVERS
        # ---------------------------------------------------------------------

        # File upload
        private$.observers$fileUpload <- shiny::observeEvent(input$reductionUpload, {
          private$handleReductionInput(function() {
            private$readReductionInput(input)
          }, output, session)
        }, ignoreInit = TRUE)

        # Example button
        private$.observers$example <- shiny::observeEvent(input$reduction_addExample, {
          private$handleReductionInput(function() {
            private$readReductionExample()
          }, output, session)
        }, ignoreInit = TRUE)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(input$reductionGenerate, {
          private$handleReductionGenerate(input, output, session)
        }, ignoreInit = TRUE)

        # Clear button
        private$.observers$clear <- shiny::observeEvent(input$reductionClear, {
          private$handleReductionClear(output, session)
        }, ignoreInit = TRUE)

        # Plot selection - source is NOT namespaced (matches original)
        private$.observers$plotSelect <- shiny::observeEvent(
          plotly::event_data("plotly_selected", source = "ReductionPlot"),
          {
            triggeredEvent <- plotly::event_data("plotly_selected", source = "ReductionPlot")
            if (is.null(triggeredEvent) || is.null(triggeredEvent$customdata)) return()
            private$.reductionSelectedItems <- triggeredEvent$customdata
            output$reductionSelected <- shiny::renderText(
              paste(private$.reductionSelectedItems, collapse = ", "))
          }, ignoreInit = TRUE)

        # Submit button
        private$.observers$submit <- shiny::observeEvent(input$reduction_submit, {
          private$handleReductionSubmit(session)
        }, ignoreInit = TRUE)

        # OK button in modal
        private$.observers$ok <- shiny::observeEvent(input$reduction_ok, {
          private$handleReductionListAccept(session)
        }, ignoreInit = TRUE)

        # Dropdown changes - update choices to exclude already-selected columns
        private$.observers$dropdownChanges <- shiny::observe({
          input$reduction_gene_col
          input$reduction_x_axis
          input$reduction_y_axis
          input$reduction_color
          input$reduction_size
          private$updateReductionDropdownChoices(input, session)
        })

      }) # end moduleServer
    }

    # cleanup() and getRegistry() are inherited from InputSession
  ),

  private = list(
    # Prefix for auto-generated list names
    .prefix = "reduction",

    # Plot styling constants
    .naColor = "#4D4D4D",  # Dark grey for NA values
    .colorscaleContinuous = list(
      c(0, "#2166AC"), c(0.5, "#F7F7F7"), c(1, "#B2182B")  # Blue-white-red gradient
    ),
    .defaultSize = 6,  # Default marker size in pixels
    .sizeRange = c(3, 9),  # Min and max size for scaled markers
    .naSize = 1,  # Size for NA values (below minimum)
    .categoricalThreshold = 20,  # Max unique values to treat as categorical
    .markerOpacity = 0.7,

    # Current reduction data
    .currentReduction = NULL,

    # Selected items from plot
    .reductionSelectedItems = NULL,

    # Whether the plot tab has been dynamically inserted
    .tabInserted = FALSE,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    handleReductionInput = function(readCallBackFunction, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Loading 2D Reduction data.</p>")
        reductionInput <- readCallBackFunction()

        # Validate input (throws exception if invalid)
        if (!private$isValidReductionInput(reductionInput)) {
          stop("Invalid format")
        }

        # Insert the 2D Reduction Plot tab dynamically if not already inserted
        # Note: No target specified = append to end. This ensures Reduction always
        # comes after Volcano (which targets UpSet) regardless of insertion order.
        if (!private$.tabInserted) {
          shiny::insertTab(
            inputId = "inputPlots",
            tab = reductionPlotUI(self$id),
            select = TRUE,
            session = private$.parentSession
          )
          private$.tabInserted <- TRUE
        } else {
          # Tab already exists, just select it
          shiny::updateTabsetPanel(private$.parentSession, "inputPlots",
                                   selected = "2D Reduction Plot")
        }

        # Display data table preview (use local output, not renderShinyDataTable helper)
        output$reductionViewer <- DT::renderDataTable(
          reductionInput,
          server = FALSE,
          selection = 'none',
          extensions = 'Buttons',
          options = list(
            scrollX = TRUE,
            scroller = TRUE,
            "dom" = 'Blfiprt',
            buttons = createExportButtons("reduction", c())
          ),
          rownames = FALSE,
          escape = FALSE
        )

        # Prepare plot data and populate dropdowns
        private$.currentReduction <- private$prepareReductionPlot(reductionInput, session)

        # Clear plot and reset UI (use local output, not renderShinyText helper)
        output$reductionPlot <- plotly::renderPlotly(c())
        output$reductionPlotStatus <- shiny::renderText("")
        output$reductionSelected <- shiny::renderText("")
        private$.reductionSelectedItems <- c()
        shinyjs::show("reductionSelectionInfo")  # shinyjs handles namespace
        shinyjs::show("reductionPanel")  # shinyjs handles namespace
      }, error = function(e) {
        cat(paste("[Reduction] File read error:", conditionMessage(e), "\n"))
        renderWarning(paste0(
          "Could not read file. Expected format:\n",
          "  \u2022 .csv (comma-separated) or .tsv/.txt (tab-separated)\n",
          "  \u2022 At least 1 identifier column (e.g. gene symbols)\n",
          "  \u2022 At least 2 numeric columns (X/Y coordinates)\n",
          "Additional columns optional (for color/size). You'll select which columns to use after upload"
        ))
      }, finally = {
        removeModal()
      })
    },

    readReductionInput = function(input) {
      reductionFile <- input$reductionUpload
      fileType <- tolower(tools::file_ext(reductionFile$name))

      # Treat empty strings as NA for consistency
      if (fileType == "tsv" || fileType == "txt") {
        reductionInput <- read.delim(reductionFile$datapath, header = TRUE, na.strings = c("", "NA"))
      } else if (fileType == "csv") {
        reductionInput <- read.csv(reductionFile$datapath, header = TRUE, na.strings = c("", "NA"))
      } else {
        stop("Unsupported file type: ", fileType)
      }

      return(reductionInput)
    },

    readReductionExample = function() {
      # Load full example data
      examplePath <- tryCatch(getExamplesPath("reduction_example.RDS"),
                              error = function(e) "examples/reduction_example.RDS")
      reductionInput <- readRDS(examplePath)

      # Define the three reduction method coordinate pairs
      reduction_methods <- list(
        c("PC1", "PC2"),
        c("UMAP1", "UMAP2"),
        c("TSNE1", "TSNE2")
      )

      # Randomly select one method to keep example compact and demonstrate
      # different coordinate types (PCA/UMAP/t-SNE) on repeated use
      selected_coords <- sample(reduction_methods, 1)[[1]]

      # Keep only: symbol, selected coordinates, and other attributes
      columns_to_keep <- c("symbol", selected_coords, "expression", "cluster", "gene_type")

      # Filter and return
      reductionInput <- reductionInput[, columns_to_keep]
      return(reductionInput)
    },

    isValidReductionInput = function(reductionInput) {
      !private$hasInvalidColumns(reductionInput) &&
      !private$isInvalidObjectSize(reductionInput)
    },

    hasInvalidColumns = function(reductionInput) {
      # Must have at least 3 columns total
      if (ncol(reductionInput) < 3) {
        return(TRUE)
      }

      # Must have at least 2 numeric columns
      numeric_cols <- sapply(reductionInput, is.numeric)
      if (sum(numeric_cols) < 2) {
        return(TRUE)
      }

      return(FALSE)
    },

    prepareReductionPlot = function(reductionInput, session) {
      # Identify column types
      all_cols <- names(reductionInput)
      numeric_cols <- all_cols[sapply(reductionInput, is.numeric)]

      # Update Gene column dropdown (all columns)
      shiny::updateSelectInput(session, "reduction_gene_col",
                        choices = c("", all_cols),
                        selected = "")

      # Update X/Y axis dropdowns (numeric only)
      shiny::updateSelectInput(session, "reduction_x_axis",
                        choices = c("", numeric_cols),
                        selected = "")
      shiny::updateSelectInput(session, "reduction_y_axis",
                        choices = c("", numeric_cols),
                        selected = "")

      # Update Color dropdown (all columns)
      shiny::updateSelectInput(session, "reduction_color",
                        choices = c("", all_cols),
                        selected = "")

      # Update Size dropdown (numeric only)
      shiny::updateSelectInput(session, "reduction_size",
                        choices = c("", numeric_cols),
                        selected = "")

      return(reductionInput)
    },

    updateReductionDropdownChoices = function(input, session) {
      if (nrow(private$.currentReduction) == 0) return()

      # Get current selections
      gene_col <- input$reduction_gene_col
      x_col <- input$reduction_x_axis
      y_col <- input$reduction_y_axis
      color_col <- input$reduction_color
      size_col <- input$reduction_size

      # Get all columns
      all_cols <- names(private$.currentReduction)
      numeric_cols <- all_cols[sapply(private$.currentReduction, is.numeric)]

      # Build list of selected columns (excluding empty strings)
      selected_cols <- c(gene_col, x_col, y_col, color_col, size_col)
      selected_cols <- selected_cols[selected_cols != "" & !is.null(selected_cols)]

      # Helper function: get available choices excluding other selections
      get_available <- function(base_choices, current_selection) {
        other_selections <- selected_cols[selected_cols != current_selection]
        setdiff(base_choices, other_selections)
      }

      # Update each dropdown with available choices
      shiny::updateSelectInput(session, "reduction_gene_col",
                        choices = c("", get_available(all_cols, gene_col)),
                        selected = gene_col)

      shiny::updateSelectInput(session, "reduction_x_axis",
                        choices = c("", get_available(numeric_cols, x_col)),
                        selected = x_col)

      shiny::updateSelectInput(session, "reduction_y_axis",
                        choices = c("", get_available(numeric_cols, y_col)),
                        selected = y_col)

      shiny::updateSelectInput(session, "reduction_color",
                        choices = c("", get_available(all_cols, color_col)),
                        selected = color_col)

      shiny::updateSelectInput(session, "reduction_size",
                        choices = c("", get_available(numeric_cols, size_col)),
                        selected = size_col)
    },

    handleReductionGenerate = function(input, output, session) {
      tryCatch({
        # Validate required selections
        gene_col <- input$reduction_gene_col
        x_col <- input$reduction_x_axis
        y_col <- input$reduction_y_axis

        if (is.null(gene_col) || gene_col == "") {
          renderWarning("Please select the Gene column.")
          return()
        }

        if (is.null(x_col) || x_col == "" || is.null(y_col) || y_col == "") {
          renderWarning("Please select both X axis and Y axis columns.")
          return()
        }

        renderModal("<h2>Please wait.</h2><br /><p>Generating 2D Reduction plot.</p>")

        # Calculate exclusions
        total_genes <- nrow(private$.currentReduction)
        gene_col_data <- private$.currentReduction[[gene_col]]
        x_col_data <- private$.currentReduction[[x_col]]
        y_col_data <- private$.currentReduction[[y_col]]

        # Count different types of missing data
        missing_gene_names <- is.na(gene_col_data) | gene_col_data == ""
        missing_coords <- is.na(x_col_data) | is.na(y_col_data)
        excluded <- missing_gene_names | missing_coords

        total_excluded <- sum(excluded)
        total_plotted <- total_genes - total_excluded

        # Build status message
        if (total_excluded > 0) {
          exclusion_parts <- c()
          if (sum(missing_gene_names) > 0) {
            exclusion_parts <- c(exclusion_parts,
                                paste0(sum(missing_gene_names), " missing gene names"))
          }
          if (sum(missing_coords) > 0) {
            exclusion_parts <- c(exclusion_parts,
                                paste0(sum(missing_coords), " missing coordinates"))
          }

          status_msg <- paste0(
            "Plotting ", total_plotted, " of ", total_genes, " genes (",
            total_excluded, " excluded: ", paste(exclusion_parts, collapse = ", "), ")"
          )
        } else {
          status_msg <- paste0("Plotting ", total_plotted, " genes")
        }

        output$reductionPlotStatus <- shiny::renderText(status_msg)

        # Render the plot
        private$renderReduction(input, output, session)

      }, error = function(e) {
        cat(paste("[Reduction] Generate error:", conditionMessage(e), "\n"))
        renderError("2D Reduction plot generation error.")
      }, finally = {
        removeModal()
      })
    },

    renderReduction = function(input, output, session) {
      gene_col <- input$reduction_gene_col
      x_col <- input$reduction_x_axis
      y_col <- input$reduction_y_axis
      color_col <- input$reduction_color
      size_col <- input$reduction_size

      # Exclude genes with missing gene names or coordinates
      plot_data <- private$.currentReduction[
        !is.na(private$.currentReduction[[gene_col]]) & private$.currentReduction[[gene_col]] != "" &
        !is.na(private$.currentReduction[[x_col]]) &
        !is.na(private$.currentReduction[[y_col]]), ]

      # Build hover text
      hover_text <- paste0("<b>", gene_col, "</b>: ", plot_data[[gene_col]])
      for (col_name in names(plot_data)) {
        if (col_name != gene_col) {
          col_data <- plot_data[[col_name]]
          formatted_data <- if (is.numeric(col_data)) {
            ifelse(is.na(col_data), "NA", round(col_data, 2))
          } else {
            ifelse(is.na(col_data), "NA", as.character(col_data))
          }
          hover_text <- paste0(hover_text, "\n", col_name, ": ", formatted_data)
        }
      }
      plot_data$hover_text <- hover_text

      use_color <- !is.null(color_col) && color_col != ""
      use_size <- !is.null(size_col) && size_col != ""

      # Determine color type and prepare palette
      is_categorical_color <- FALSE
      color_palette <- NULL
      if (use_color) {
        color_data <- plot_data[[color_col]]
        is_categorical_color <- is.character(color_data) || is.factor(color_data) ||
                               length(unique(color_data[!is.na(color_data)])) <= private$.categoricalThreshold

        if (is_categorical_color) {
          n_colors <- length(unique(color_data[!is.na(color_data)]))
          colors_set <- RColorBrewer::brewer.pal(min(max(3, n_colors), 8), "Set2")
          color_palette <- if (n_colors > 8) grDevices::colorRampPalette(colors_set)(n_colors) else colors_set
        }
      }

      # Pre-transform size data
      if (use_size) {
        size_data_scaled <- log10(plot_data[[size_col]] + 1)
        size_range <- range(size_data_scaled, na.rm = TRUE)

        plot_data$size_scaled <- if (size_range[2] > size_range[1]) {
          private$.sizeRange[1] + (private$.sizeRange[2] - private$.sizeRange[1]) *
            (size_data_scaled - size_range[1]) / (size_range[2] - size_range[1])
        } else {
          rep(private$.defaultSize, length(size_data_scaled))
        }
        plot_data$size_scaled[is.na(plot_data[[size_col]])] <- private$.naSize
      }

      output$reductionPlot <- plotly::renderPlotly({
        p <- plotly::plot_ly(source = "ReductionPlot")

        # Categorical color - plot each category separately with legend
        if (use_color && is_categorical_color) {
          color_categories <- unique(plot_data[[color_col]])
          color_categories <- color_categories[!is.na(color_categories)]

          # Plot valid colors
          for (c_idx in seq_along(color_categories)) {
            subset_data <- plot_data[
              plot_data[[color_col]] == color_categories[c_idx] &
              !is.na(plot_data[[color_col]]), ]

            if (nrow(subset_data) > 0) {
              p <- p %>% plotly::add_trace(
                data = subset_data,
                x = ~get(x_col), y = ~get(y_col),
                type = "scatter", mode = "markers",
                marker = list(
                  color = color_palette[c_idx],
                  size = if (use_size) subset_data$size_scaled else private$.defaultSize,
                  line = list(width = 0),
                  opacity = private$.markerOpacity
                ),
                text = ~hover_text, hoverinfo = "text",
                customdata = ~get(gene_col),
                name = as.character(color_categories[c_idx]),
                showlegend = TRUE
              )
            }
          }

          # Plot NA color (use grey, keep default circle shape)
          subset_data <- plot_data[is.na(plot_data[[color_col]]), ]

          if (nrow(subset_data) > 0) {
            p <- p %>% plotly::add_trace(
              data = subset_data,
              x = ~get(x_col), y = ~get(y_col),
              type = "scatter", mode = "markers",
              marker = list(
                color = private$.naColor,
                size = if (use_size) subset_data$size_scaled else private$.defaultSize,
                line = list(width = 0),
                opacity = private$.markerOpacity
              ),
              text = ~hover_text, hoverinfo = "text",
              customdata = ~get(gene_col),
              name = "NA",
              showlegend = TRUE
            )
          }
        # Continuous color or no color mapping
        } else {
          if (use_color && !is_categorical_color) {
            # Plot points with valid color values
            subset_data <- plot_data[!is.na(plot_data[[color_col]]), ]

            if (nrow(subset_data) > 0) {
              marker_config <- list(
                color = subset_data[[color_col]],
                colorscale = private$.colorscaleContinuous,
                colorbar = list(title = color_col),
                showscale = TRUE,
                size = if (use_size) subset_data$size_scaled else private$.defaultSize,
                line = list(width = 0),
                opacity = private$.markerOpacity
              )

              p <- p %>% plotly::add_trace(
                data = subset_data,
                x = ~get(x_col), y = ~get(y_col),
                type = "scatter", mode = "markers",
                marker = marker_config,
                text = ~hover_text, hoverinfo = "text",
                customdata = ~get(gene_col),
                showlegend = FALSE
              )
            }

            # Plot NA color values (use grey, keep default circle shape)
            subset_data <- plot_data[is.na(plot_data[[color_col]]), ]

            if (nrow(subset_data) > 0) {
              p <- p %>% plotly::add_trace(
                data = subset_data,
                x = ~get(x_col), y = ~get(y_col),
                type = "scatter", mode = "markers",
                marker = list(
                  color = private$.naColor,
                  size = if (use_size) subset_data$size_scaled else private$.defaultSize,
                  line = list(width = 0),
                  opacity = private$.markerOpacity
                ),
                text = ~hover_text, hoverinfo = "text",
                customdata = ~get(gene_col),
                name = paste0(color_col, ": NA"),
                showlegend = TRUE
              )
            }
          } else {
            # No color mapping - plot all points
            marker_config <- list(
              size = if (use_size) plot_data$size_scaled else private$.defaultSize,
              line = list(width = 0),
              opacity = private$.markerOpacity
            )

            p <- p %>% plotly::add_trace(
              data = plot_data,
              x = ~get(x_col), y = ~get(y_col),
              type = "scatter", mode = "markers",
              marker = marker_config,
              text = ~hover_text, hoverinfo = "text",
              customdata = ~get(gene_col),
              showlegend = FALSE
            )
          }
        }

        # Configure legend title
        legend_title <- if (use_color && is_categorical_color) {
          color_col
        }

        p %>% plotly::layout(
          xaxis = list(title = x_col),
          yaxis = list(title = y_col),
          dragmode = "lasso",
          hovermode = "closest",
          legend = if (!is.null(legend_title)) list(title = list(text = legend_title)) else list()
        )
      })
    },

    handleReductionClear = function(output, session) {
      tryCatch({
        shiny::updateSelectInput(session, "reduction_gene_col", selected = "")
        shiny::updateSelectInput(session, "reduction_x_axis", selected = "")
        shiny::updateSelectInput(session, "reduction_y_axis", selected = "")
        shiny::updateSelectInput(session, "reduction_color", selected = "")
        shiny::updateSelectInput(session, "reduction_size", selected = "")

        # Clear plot and status
        output$reductionPlot <- plotly::renderPlotly(c())
        output$reductionPlotStatus <- shiny::renderText("")
        output$reductionSelected <- shiny::renderText("")
      }, error = function(e) {
        cat(paste("[Reduction] Clear error:", conditionMessage(e), "\n"))
        renderError("2D Reduction clear error.")
      })
    },

    handleReductionSubmit = function(session) {
      tryCatch({
        if (private$hasSelectedItems()) {
          shiny::showModal(shiny::modalDialog(
            title = "2D Reduction selected set",
            paste0("Add the selected genes to the lists?"),
            footer = shiny::tagList(
              shiny::actionButton(session$ns("reduction_ok"), "OK"),
              shiny::modalButton("Cancel")
            )
          ))
        }
      }, error = function(e) {
        cat(paste("[Reduction] Submit error:", conditionMessage(e), "\n"))
        renderError("2D Reduction submit selected list error.")
      })
    },

    hasSelectedItems = function() {
      if (length(private$.reductionSelectedItems) == 0) {
        renderWarning("Please select points on the plot using box or lasso selection tool.")
        return(FALSE)
      }
      return(TRUE)
    },

    handleReductionListAccept = function(session) {
      tryCatch({
        # Use inherited method from InputSession
        listName <- self$generateUniqueName(private$.prefix)
        ids <- as.character(private$.reductionSelectedItems)
        self$addAnalyteList(listName, ids)

        shiny::removeModal()
      }, error = function(e) {
        cat(paste("[Reduction] List accept error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      })
    }
  )
)
