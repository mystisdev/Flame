# =============================================================================
# HEATMAP OUTPUT SESSIONS
# =============================================================================
# All heatmap-related output sessions in one file for cohesion.
#
# Class Hierarchy:
# OutputSession (base - has .selectedTerms)
#     └── HeatmapOutputSession (intermediate - adds .selectedPairs, .selectedCells)
#             ├── Heatmap1OutputSession (Function vs Gene)
#             ├── Heatmap2OutputSession (Function vs Function)
#             └── Heatmap3OutputSession (Gene vs Gene)
#
# Heatmaps use pair-level selection (two dimensions) instead of term selection.
# =============================================================================

# =============================================================================
# HEATMAP OUTPUT SESSION (Intermediate Base Class)
# =============================================================================
# Adds pair/cell selection on top of OutputSession.
# NOT used directly - subclasses implement concrete heatmap types.
# =============================================================================

HeatmapOutputSession <- R6::R6Class(
  "HeatmapOutputSession",
  inherit = OutputSession,

  public = list(
    #' Initialize a new HeatmapOutputSession
    #'
    #' @param runKey Parent run key (e.g., "functional_gProfiler_1")
    #' @param enrichSession Parent ORAEnrichmentSession object
    #' @param outputType Type of heatmap (e.g., "heatmap1")
    #' @param instance Instance number for unique moduleServer ID
    initialize = function(runKey, enrichSession, outputType, instance = 1) {
      super$initialize(runKey, enrichSession, outputType, instance = instance)

      # Initialize heatmap-specific state (pair selection instead of term selection)
      private$.selectedPairs <- shiny::reactiveVal(
        data.frame(source = character(0), target = character(0), stringsAsFactors = FALSE)
      )
      private$.selectedCells <- shiny::reactiveVal(
        data.frame(x = character(0), y = character(0), stringsAsFactors = FALSE)
      )
    },

    #' Get selected pairs
    #'
    #' @return Data frame with source and target columns
    getSelectedPairs = function() {
      private$.selectedPairs()
    },

    #' Get selected cells for visual highlighting
    #'
    #' @return Data frame with x and y columns
    getSelectedCells = function() {
      private$.selectedCells()
    },

    #' Handle reset view button (override from base)
    #'
    #' Clears pair/cell selections and restores original data.
    handleReset = function() {
      # Clear heatmap-specific selections
      private$.selectedPairs(data.frame(source = character(0), target = character(0),
                                        stringsAsFactors = FALSE))
      private$.selectedCells(data.frame(x = character(0), y = character(0),
                                        stringsAsFactors = FALSE))

      originalData <- private$.originalData()
      if (is.null(originalData) || nrow(originalData) == 0) {
        shiny::showNotification("No data available. Please generate the heatmap first.",
                                type = "warning", duration = 3)
        return()
      }

      # Restore current view to original
      private$.currentView(originalData)
      private$setUpdateSource("reset")

      # Subclass implements this
      private$renderBothFromCurrentView()

      # Clear visual highlighting
      private$updateHeatmapHighlighting()
    },

    #' Clear for refresh (override from base)
    #'
    #' Clears heatmap-specific state in addition to base state.
    clearForRefresh = function() {
      # Clear heatmap-specific state first
      private$.selectedPairs(data.frame(source = character(0), target = character(0),
                                        stringsAsFactors = FALSE))
      private$.selectedCells(data.frame(x = character(0), y = character(0),
                                        stringsAsFactors = FALSE))

      # Call parent clearForRefresh
      super$clearForRefresh()
    },

    #' Clean up resources (override from base)
    #'
    #' Destroys observers and clears heatmap-specific state.
    cleanup = function() {
      # Clear heatmap-specific state first
      private$.selectedPairs(data.frame(source = character(0), target = character(0),
                                        stringsAsFactors = FALSE))
      private$.selectedCells(data.frame(x = character(0), y = character(0),
                                        stringsAsFactors = FALSE))

      # Call parent cleanup
      super$cleanup()
    }
  ),

  private = list(
    # Colorscale for heatmaps (Viridis for consistency across app)
    .colorscale = "Viridis",

    # Heatmap-specific state (pair selection instead of term selection)
    .selectedPairs = NULL,
    .selectedCells = NULL,

    # -------------------------------------------------------------------------
    # Pair Selection Methods
    # -------------------------------------------------------------------------

    #' Check if a pair exists (order-independent)
    pairExists = function(sourceId, targetId) {
      pairs <- private$.selectedPairs()
      if (nrow(pairs) == 0) return(FALSE)
      any(
        (pairs$source == sourceId & pairs$target == targetId) |
          (pairs$source == targetId & pairs$target == sourceId)
      )
    },

    #' Toggle pair selection (add if not selected, remove if selected)
    togglePairSelection = function(sourceId, targetId) {
      pairs <- private$.selectedPairs()

      if (private$pairExists(sourceId, targetId)) {
        # Remove pair (order-independent)
        newPairs <- pairs[!(
          (pairs$source == sourceId & pairs$target == targetId) |
            (pairs$source == targetId & pairs$target == sourceId)
        ), , drop = FALSE]
        private$.selectedPairs(newPairs)
      } else {
        # Add pair
        newPairs <- rbind(pairs, data.frame(source = sourceId, target = targetId,
                                            stringsAsFactors = FALSE))
        private$.selectedPairs(newPairs)
      }
    },

    #' Get unique terms from all selected pairs
    getTermsFromSelectedPairs = function() {
      pairs <- private$.selectedPairs()
      if (nrow(pairs) == 0) return(character(0))
      unique(c(pairs$source, pairs$target))
    },

    #' Clear all pair selections
    clearPairSelection = function() {
      private$.selectedPairs(data.frame(source = character(0), target = character(0),
                                        stringsAsFactors = FALSE))
    },

    # -------------------------------------------------------------------------
    # Cell Selection Methods (for visual highlighting)
    # -------------------------------------------------------------------------

    #' Check if a cell is selected
    cellExists = function(x, y) {
      cells <- private$.selectedCells()
      if (nrow(cells) == 0) return(FALSE)
      any(cells$x == x & cells$y == y)
    },

    #' Toggle cell selection
    toggleCellSelection = function(x, y) {
      cells <- private$.selectedCells()

      if (private$cellExists(x, y)) {
        newCells <- cells[!(cells$x == x & cells$y == y), , drop = FALSE]
        private$.selectedCells(newCells)
      } else {
        newCells <- rbind(cells, data.frame(x = as.character(x), y = as.character(y),
                                            stringsAsFactors = FALSE))
        private$.selectedCells(newCells)
      }
    },

    #' Clear all cell selections
    clearCellSelection = function() {
      private$.selectedCells(data.frame(x = character(0), y = character(0),
                                        stringsAsFactors = FALSE))
    },

    # -------------------------------------------------------------------------
    # Visual Highlighting (Rectangle Shapes)
    # -------------------------------------------------------------------------

    #' Update heatmap highlighting with rectangle shapes
    #'
    #' Draws rectangles around selected cells using plotlyProxy.
    updateHeatmapHighlighting = function() {
      cells <- private$.selectedCells()
      renderedData <- private$.renderedData()

      if (is.null(renderedData) || nrow(renderedData) == 0) return()

      proxy <- plotly::plotlyProxy("plot", private$.moduleSession)
      shapes <- list()

      if (nrow(cells) > 0) {
        # Get category arrays for proper shape positioning
        categoryInfo <- private$getHeatmapCategoryArrays()
        if (is.null(categoryInfo)) {
          plotly::plotlyProxyInvoke(proxy, "relayout", list(shapes = list()))
          return()
        }

        xCategories <- categoryInfo$x
        yCategories <- categoryInfo$y  # Already reversed

        for (i in seq_len(nrow(cells))) {
          cellX <- cells$x[i]
          cellY <- cells$y[i]

          # Convert to 0-based indices for plotly.js
          xIndex <- match(cellX, xCategories) - 1
          yIndex <- match(cellY, yCategories) - 1

          if (!is.na(xIndex) && !is.na(yIndex)) {
            shapes[[length(shapes) + 1]] <- list(
              type = "rect",
              xref = "x",
              yref = "y",
              x0 = xIndex - 0.5,
              x1 = xIndex + 0.5,
              y0 = yIndex - 0.5,
              y1 = yIndex + 0.5,
              line = list(color = "black", width = 2),
              fillcolor = "rgba(0,0,0,0)"
            )
          }
        }
      }

      plotly::plotlyProxyInvoke(proxy, "relayout", list(shapes = shapes))
    },

    #' Get heatmap category arrays for proper shape positioning
    #'
    #' Must be implemented by subclasses - returns list(x = xCategories, y = yCategories)
    getHeatmapCategoryArrays = function() {
      stop("Subclass must implement getHeatmapCategoryArrays()")
    },

    # -------------------------------------------------------------------------
    # Common Heatmap Helpers
    # -------------------------------------------------------------------------

    #' Resolve a clicked value to Term_ID_noLinks
    resolveToTermIdNoLinks = function(clickedValue) {
      if (is.null(clickedValue)) return(NULL)

      originalData <- private$.originalData()
      if (is.null(originalData) || nrow(originalData) == 0) return(NULL)

      # Try direct match on Term_ID_noLinks
      if ("Term_ID_noLinks" %in% names(originalData)) {
        if (clickedValue %in% originalData$Term_ID_noLinks) {
          return(clickedValue)
        }
      }

      # Try matching by Function name
      if ("Function" %in% names(originalData)) {
        match_row <- which(originalData$Function == clickedValue)
        if (length(match_row) > 0) {
          return(originalData$Term_ID_noLinks[match_row[1]])
        }
      }

      # Try matching by Term_ID
      if ("Term_ID" %in% names(originalData)) {
        match_row <- which(originalData$Term_ID == clickedValue)
        if (length(match_row) > 0) {
          return(originalData$Term_ID_noLinks[match_row[1]])
        }
      }

      return(NULL)
    },

    #' Render the enrichment table
    renderTable = function(data, sourceSelect) {
      fileName <- paste(self$id, paste(sourceSelect, collapse = "_"), sep = "_")
      private$markProgrammaticUpdate()

      # Use the encapsulated internal method from OutputSession base
      private$renderEnrichmentTableInternal(
        data = data,
        caption = "Enrichment Results",
        fileName = fileName,
        mode = "Positive Hits",
        hiddenColumns = c(10, 11),
        expandableColumn = 10,
        filter = 'top'
      )
    },

    #' Calculate plot height based on number of entries
    calculateHeight = function(entriesCount) {
      entriesCount * private$.entryHeightPx + private$.minPlotHeight
    },

    #' Filter data by source and top N entries (shared by all heatmap types)
    #'
    #' Now supports multi-select datasources.
    filterData = function(results, sourceSelect, sortMode, termCount) {
      filteredData <- subset(results, Source %in% sourceSelect)
      if (nrow(filteredData) == 0) return(NULL)

      if (sortMode == "Enrichment Score") {
        filteredData <- filteredData[order(-filteredData$`Enrichment Score %`), ]
      } else {
        filteredData <- filteredData[order(-filteredData$`-log10Pvalue`), ]
      }

      filteredData <- head(filteredData, termCount)
      filteredData$`Positive Hits` <- gsub(",", ", ", filteredData$`Positive Hits`)
      return(filteredData)
    }
  )
)


# =============================================================================
# HEATMAP1 OUTPUT SESSION (Function vs Gene)
# =============================================================================

Heatmap1OutputSession <- R6::R6Class(
  "Heatmap1OutputSession",
  inherit = HeatmapOutputSession,

  public = list(
    #' Initialize
    #' @param outputType Output type identifier from config (e.g., "heatmap1")
    initialize = function(runKey, enrichSession, outputType, instance = 1) {
      super$initialize(runKey, enrichSession, outputType = outputType, instance = instance)
    },

    #' Generate namespaced UI
    #'
    #' Composes UI from base class fragments.
    #' Heatmap1 has axis orientation control (unique to this type).
    ui = function() {
      ns <- shiny::NS(self$id)
      enrichmentType <- "functional"
      uiTermKeyword <- UI_TERM_KEYWORD[[enrichmentType]]

      shiny::tagList(
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(3, self$datasourcePickerUI(ns)),
          shiny::column(3, self$termCountSliderUI(ns, uiTermKeyword)),
          shiny::column(3, self$sortModeUI(ns, uiTermKeyword)),
          shiny::column(
            3,
            # Axis orientation - unique to Heatmap1
            shiny::radioButtons(
              inputId = ns("axisOrientation"),
              label = "Axis orientation:",
              choices = c(
                paste0(stringr::str_to_title(uiTermKeyword), "-Genes"),
                paste0("Genes-", stringr::str_to_title(uiTermKeyword))
              ),
              inline = TRUE
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          ),
          shiny::column(
            6,
            shiny::radioButtons(
              inputId = ns("drawFormat"),
              label = "Cell labels:",
              choices = c("Function", "Term_ID"),
              inline = TRUE
            )
          )
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "heatmapOutput",
          plotly::plotlyOutput(ns("plot"))
        ),
        shiny::tags$br(),
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate heatmap
    generate = function(sourceSelect, sortMode, termCount, axisConfig, drawFormat) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for heatmap.")
          return()
        }

        # Store axis config for click handling
        private$.axisConfig <- axisConfig

        # Filter by source and top N
        filteredData <- private$filterData(results, sourceSelect, sortMode, termCount)
        if (is.null(filteredData) || nrow(filteredData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Store as original data
        private$setOriginalData(filteredData)

        # Clear previous selections
        private$clearPairSelection()
        private$clearCellSelection()

        # Render table
        private$renderTable(filteredData, sourceSelect)

        # Transform and render heatmap
        private$renderHeatmapPlot(filteredData, sourceSelect, axisConfig, drawFormat)

      }, error = function(e) {
        renderWarning(paste("Cannot create heatmap:", e$message))
      }, finally = {
        removeModal()
      })
    },

    #' Handle click event
    handleClick = function(clickData) {
      if (is.null(clickData)) return()

      cellX <- as.character(clickData$x)
      cellY <- as.character(clickData$y)

      # Determine which axis is term vs gene based on axis config
      enrichmentType <- "functional"
      uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])
      axisConfig <- private$.axisConfig

      if (is.null(axisConfig)) {
        axisConfig <- paste0(uiTermKeyword, "-Genes")
      }

      if (axisConfig == paste0(uiTermKeyword, "-Genes")) {
        clickedTermId <- cellY
        clickedGene <- cellX
      } else {
        clickedTermId <- cellX
        clickedGene <- cellY
      }

      termId <- private$resolveToTermIdNoLinks(clickedTermId)

      if (!is.null(termId) && !is.null(clickedGene)) {
        private$togglePairSelection(termId, clickedGene)
        private$renderTableFromSelection()
        private$toggleCellSelection(cellX, cellY)
        private$updateHeatmapHighlighting()
      }
    },

    #' Set up server logic
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            if (!is.null(input$sourceSelect)) {
              self$generate(input$sourceSelect, input$sortMode, input$termCountSlider,
                            input$axisOrientation, input$drawFormat)
            }
          },
          ignoreInit = TRUE
        )

        # Reset button
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          { self$handleReset() },
          ignoreInit = TRUE
        )

        # Table filter
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          { self$handleTableFilter(input$table_rows_all) },
          ignoreInit = TRUE
        )

        # Plotly click observer
        # priority = "event" ensures each click fires even if same cell clicked twice
        private$.observers$plotClick <- shiny::observeEvent(
          plotly::event_data("plotly_click", source = self$id, priority = "event"),
          {
            clickData <- plotly::event_data("plotly_click", source = self$id)
            self$handleClick(clickData)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )
      })
    },

    #' Handle table filter
    handleTableFilter = function(filteredRowIndices) {
      if (!private$shouldProcessUpdate("table")) return()

      currentData <- private$.currentView()
      if (is.null(currentData) || nrow(currentData) == 0) return()
      if (is.null(filteredRowIndices) || length(filteredRowIndices) == 0) return()
      if (max(filteredRowIndices) > nrow(currentData)) return()
      if (private$isProgrammaticUpdate()) return()

      # All rows = filter cleared
      if (length(filteredRowIndices) == nrow(currentData)) {
        private$setUpdateSource("table_filter")
        private$renderPlotFromCurrentView()
        return()
      }

      # Render with filtered subset
      filteredData <- currentData[filteredRowIndices, , drop = FALSE]
      if (nrow(filteredData) == 0) return()

      private$clearPairSelection()
      private$clearCellSelection()
      private$setUpdateSource("table_filter")
      private$renderPlotWithData(filteredData)
    }
  ),

  private = list(
    .axisConfig = NULL,

    # Separate rows - explode Positive Hits
    separateRows = function(enrichmentData) {
      enrichmentData <- enrichmentData[, c(
        "Source", "Term_ID", "Term_ID_noLinks", "Function", "Positive Hits",
        "Enrichment Score %", "-log10Pvalue", "Intersection Size")]
      tidyr::separate_rows(enrichmentData, `Positive Hits`, sep = ", ")
    },

    # Render the heatmap plot
    renderHeatmapPlot = function(data, sourceSelect, axisConfig, drawFormat) {
      heatmapTable <- private$separateRows(data)
      heatmapTable$GeneExists <- 1

      enrichmentType <- "functional"
      uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])

      # Determine axis orientation and corresponding hover labels
      if (axisConfig == paste0(uiTermKeyword, "-Genes")) {
        yAxisColumn <- drawFormat
        xAxisColumn <- "Positive Hits"
        yLabel <- "Term"
        xLabel <- "Gene"
      } else {
        yAxisColumn <- "Positive Hits"
        xAxisColumn <- drawFormat
        yLabel <- "Gene"
        xLabel <- "Term"
      }

      # Store for category arrays
      private$.heatmapTable <- heatmapTable
      private$.yAxisColumn <- yAxisColumn
      private$.xAxisColumn <- xAxisColumn

      entriesCount <- length(unique(heatmapTable[[yAxisColumn]]))
      height <- private$calculateHeight(entriesCount)

      private$setRenderedData(data)
      private$renderHeatmapPlotly(heatmapTable, yAxisColumn, xAxisColumn,
                                  "GeneExists", height, FALSE, NULL,
                                  yLabel, xLabel)
    },

    # Render heatmap with plotly
    renderHeatmapPlotly = function(heatmapTable, yAxisColumn, xAxisColumn,
                                    weightColumn, height,
                                    showColorbar = TRUE, colorbarTitle = NULL,
                                    yLabel = "Y", xLabel = "X") {
      yCategories <- rev(unique(heatmapTable[[yAxisColumn]]))
      xCategories <- unique(heatmapTable[[xAxisColumn]])

      private$.output$plot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = heatmapTable,
          y = heatmapTable[[yAxisColumn]],
          x = heatmapTable[[xAxisColumn]],
          z = heatmapTable[[weightColumn]],
          type = 'heatmap',
          colorscale = private$.colorscale,
          hoverinfo = "text",
          hovertext = ~paste0(yLabel, ": ", get(yAxisColumn), "\n", xLabel, ": ", get(xAxisColumn)),
          height = height,
          source = self$id,
          showscale = showColorbar,
          colorbar = list(title = colorbarTitle)
        ) %>%
          plotly::layout(
            xaxis = list(showgrid = FALSE, categoryorder = "array", categoryarray = xCategories),
            yaxis = list(showgrid = FALSE, categoryorder = "array", categoryarray = yCategories)
          ) %>%
          plotly::config(doubleClick = "reset+autosize")
      })
    },

    # Get category arrays for highlighting
    getHeatmapCategoryArrays = function() {
      heatmapTable <- private$.heatmapTable
      yAxisColumn <- private$.yAxisColumn
      xAxisColumn <- private$.xAxisColumn

      if (is.null(heatmapTable)) return(NULL)

      list(
        x = unique(heatmapTable[[xAxisColumn]]),
        y = rev(unique(heatmapTable[[yAxisColumn]]))
      )
    },

    # Render table from selection
    renderTableFromSelection = function() {
      originalData <- private$.originalData()
      if (is.null(originalData) || nrow(originalData) == 0) return()

      pairs <- private$.selectedPairs()

      if (nrow(pairs) == 0) {
        tableData <- originalData
      } else {
        # Only show terms (source), not genes (target)
        selectedTerms <- unique(pairs$source)
        tableData <- originalData[originalData$Term_ID_noLinks %in% selectedTerms, , drop = FALSE]
      }

      if (nrow(tableData) > 0) {
        private$.currentView(tableData)
        private$setUpdateSource("selection")
        private$markProgrammaticUpdate()
        private$renderTable(tableData, unique(as.character(tableData$Source)))
      }
    },

    # Required by base class
    renderBothFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      # Get current settings
      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      axisConfig <- shiny::isolate(private$.moduleSession$input$axisOrientation)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      private$markProgrammaticUpdate()
      private$renderTable(data, sourceSelect)
      private$renderHeatmapPlot(data, sourceSelect, axisConfig, drawFormat)
    },

    renderPlotFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      axisConfig <- shiny::isolate(private$.moduleSession$input$axisOrientation)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      private$renderHeatmapPlot(data, sourceSelect, axisConfig, drawFormat)
    },

    renderPlotWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      axisConfig <- shiny::isolate(private$.moduleSession$input$axisOrientation)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      private$setRenderedData(data)
      private$renderHeatmapPlot(data, sourceSelect, axisConfig, drawFormat)
    },

    # Storage for heatmap rendering state
    .heatmapTable = NULL,
    .yAxisColumn = NULL,
    .xAxisColumn = NULL
  )
)


# =============================================================================
# HEATMAP2 OUTPUT SESSION (Function vs Function)
# =============================================================================

Heatmap2OutputSession <- R6::R6Class(
  "Heatmap2OutputSession",
  inherit = HeatmapOutputSession,

  public = list(
    #' Initialize
    #' @param outputType Output type identifier from config (e.g., "heatmap2")
    initialize = function(runKey, enrichSession, outputType, instance = 1) {
      super$initialize(runKey, enrichSession, outputType = outputType, instance = instance)
    },

    #' Generate namespaced UI
    #'
    #' Composes UI from base class fragments.
    ui = function() {
      ns <- shiny::NS(self$id)
      enrichmentType <- "functional"
      uiTermKeyword <- UI_TERM_KEYWORD[[enrichmentType]]

      shiny::tagList(
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(3, self$datasourcePickerUI(ns)),
          shiny::column(3, self$termCountSliderUI(ns, uiTermKeyword)),
          shiny::column(3, self$sortModeUI(ns, uiTermKeyword)),
          shiny::column(
            3,
            shiny::radioButtons(
              inputId = ns("drawFormat"),
              label = "Axis labels:",
              choices = c("Function", "Term_ID"),
              inline = TRUE
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          )
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "heatmapOutput",
          plotly::plotlyOutput(ns("plot"))
        ),
        shiny::tags$br(),
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate heatmap
    generate = function(sourceSelect, sortMode, termCount, drawFormat) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for heatmap.")
          return()
        }

        # Filter by source and top N
        filteredData <- private$filterData(results, sourceSelect, sortMode, termCount)
        if (is.null(filteredData) || nrow(filteredData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Store as original data
        private$setOriginalData(filteredData)

        # Clear previous selections
        private$clearPairSelection()
        private$clearCellSelection()

        # Render table
        private$renderTable(filteredData, sourceSelect)

        # Transform and render heatmap
        private$renderHeatmapPlot(filteredData, sourceSelect, drawFormat)

      }, error = function(e) {
        renderWarning(paste("Cannot create heatmap:", e$message))
      }, finally = {
        removeModal()
      })
    },

    #' Handle click event
    handleClick = function(clickData) {
      if (is.null(clickData)) return()

      # Both axes are terms
      termIds <- c(as.character(clickData$x), as.character(clickData$y))

      if (length(termIds) == 2) {
        sourceId <- private$resolveToTermIdNoLinks(termIds[1])
        targetId <- private$resolveToTermIdNoLinks(termIds[2])

        if (!is.null(sourceId) && !is.null(targetId)) {
          private$togglePairSelection(sourceId, targetId)
          private$renderTableFromSelection()

          cellX <- as.character(clickData$x)
          cellY <- as.character(clickData$y)
          private$toggleCellSelection(cellX, cellY)
          private$updateHeatmapHighlighting()
        }
      }
    },

    #' Set up server logic
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            if (!is.null(input$sourceSelect)) {
              self$generate(input$sourceSelect, input$sortMode, input$termCountSlider, input$drawFormat)
            }
          },
          ignoreInit = TRUE
        )

        # Reset button
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          { self$handleReset() },
          ignoreInit = TRUE
        )

        # Table filter
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          { self$handleTableFilter(input$table_rows_all) },
          ignoreInit = TRUE
        )

        # Plotly click observer
        # priority = "event" ensures each click fires even if same cell clicked twice
        private$.observers$plotClick <- shiny::observeEvent(
          plotly::event_data("plotly_click", source = self$id, priority = "event"),
          {
            clickData <- plotly::event_data("plotly_click", source = self$id)
            self$handleClick(clickData)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )
      })
    },

    #' Handle table filter
    handleTableFilter = function(filteredRowIndices) {
      if (!private$shouldProcessUpdate("table")) return()

      currentData <- private$.currentView()
      if (is.null(currentData) || nrow(currentData) == 0) return()
      if (is.null(filteredRowIndices) || length(filteredRowIndices) == 0) return()
      if (max(filteredRowIndices) > nrow(currentData)) return()
      if (private$isProgrammaticUpdate()) return()

      if (length(filteredRowIndices) == nrow(currentData)) {
        private$setUpdateSource("table_filter")
        private$renderPlotFromCurrentView()
        return()
      }

      filteredData <- currentData[filteredRowIndices, , drop = FALSE]
      if (nrow(filteredData) == 0) return()

      private$clearPairSelection()
      private$clearCellSelection()
      private$setUpdateSource("table_filter")
      private$renderPlotWithData(filteredData)
    }
  ),

  private = list(
    # Extract Function vs Function edgelist (complex similarity calculation)
    extractFunctionVsFunctionEdgelist = function(enrichmentData) {
      functionsEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
      totalGenesEdgelist <- private$calculateEdgeTotalGenes(functionsEdgelist)
      commonGenesEdgelist <- private$calculateEdgeCommonGenes(functionsEdgelist)
      functionsEdgelist <- merge(
        commonGenesEdgelist, totalGenesEdgelist,
        by = c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
      )
      functionsEdgelist <- private$calculateSimilarityScore(functionsEdgelist)
      functionsEdgelist <- private$tuneForHeatmap(functionsEdgelist)
      functionsEdgelist <- private$appendSourceDatabasesAndIds(functionsEdgelist)
      functionsEdgelist <- functionsEdgelist[order(-functionsEdgelist$`Similarity Score %`), ]
      return(functionsEdgelist)
    },

    calculateEdgeTotalGenes = function(totalGenesEdgelist) {
      totalGenesEdgelistCopy <- totalGenesEdgelist
      colnames(totalGenesEdgelistCopy) <- c("TermsCopy", "HitsCopy")
      totalGenesEdgelist <- merge(totalGenesEdgelist, totalGenesEdgelistCopy)
      totalGenesEdgelist$`Positive Hits` <-
        paste(totalGenesEdgelist$`Positive Hits`,
              totalGenesEdgelist$HitsCopy, sep = ", ")
      totalGenesEdgelist$HitsCopy <- NULL
      totalGenesEdgelist <-
        tidyr::separate_rows(totalGenesEdgelist, `Positive Hits`, sep = ", ")
      totalGenesEdgelist <- dplyr::distinct(totalGenesEdgelist)
      totalGenesEdgelist$`Positive Hits` <- NULL
      totalGenesEdgelist <- data.table::setDT(
        totalGenesEdgelist)[, list(`Total Genes` = .N), names(totalGenesEdgelist)]
      colnames(totalGenesEdgelist)[1:2] <- c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
      return(totalGenesEdgelist)
    },

    calculateEdgeCommonGenes = function(commonGenesEdgelist) {
      commonGenesEdgelist <-
        tidyr::separate_rows(commonGenesEdgelist, `Positive Hits`, sep = ", ")
      commonGenesEdgelist <- merge(
        commonGenesEdgelist, commonGenesEdgelist,
        by.x = "Positive Hits", by.y = "Positive Hits"
      )
      commonGenesEdgelist$`Positive Hits` <- NULL
      commonGenesEdgelist <- data.table::setDT(
        commonGenesEdgelist)[, list(`Common Genes` = .N), names(commonGenesEdgelist)]
      return(commonGenesEdgelist)
    },

    calculateSimilarityScore = function(functionsEdgelist) {
      functionsEdgelist$`Similarity Score %` <-
        functionsEdgelist$`Common Genes` / functionsEdgelist$`Total Genes` * 100
      functionsEdgelist$`Similarity Score %` <-
        as.numeric(format(round(functionsEdgelist$`Similarity Score %`, 2)))
      return(functionsEdgelist)
    },

    tuneForHeatmap = function(functionsEdgelist) {
      functionsEdgelist$`Similarity Score %` <- as.numeric(functionsEdgelist$`Similarity Score %`)
      colnames(functionsEdgelist) <-
        c("Source Node", "Target Node", "Common Genes", "Total Genes", "Similarity Score %")
      return(functionsEdgelist)
    },

    appendSourceDatabasesAndIds = function(functionsEdgelist) {
      enrichedNetworkData <- self$enrichSession$getResults()
      enrichedNetworkData <- enrichedNetworkData[, c("Source", "Term_ID_noLinks", "Function")]
      functionsEdgelist <- merge(functionsEdgelist, enrichedNetworkData,
                                 by.x = "Source Node", by.y = "Term_ID_noLinks")
      functionsEdgelist <- merge(functionsEdgelist, enrichedNetworkData,
                                 by.x = "Target Node", by.y = "Term_ID_noLinks")
      colnames(functionsEdgelist) <-
        c("Target Id", "Source Id", "Common Genes", "Total Genes",
          "Similarity Score %", "Source Database", "Source Name",
          "Target Database", "Target Name")
      functionsEdgelist <-
        functionsEdgelist[, c(
          "Source Database", "Source Id", "Source Name",
          "Target Database", "Target Id", "Target Name",
          "Common Genes", "Total Genes", "Similarity Score %"
        )]

      functionsEdgelist$`Source Database` <- as.factor(functionsEdgelist$`Source Database`)
      functionsEdgelist$`Target Database` <- as.factor(functionsEdgelist$`Target Database`)
      functionsEdgelist$`Common Genes` <- as.numeric(functionsEdgelist$`Common Genes`)
      functionsEdgelist$`Total Genes` <- as.numeric(functionsEdgelist$`Total Genes`)
      functionsEdgelist$`Similarity Score %` <- as.numeric(functionsEdgelist$`Similarity Score %`)

      return(functionsEdgelist)
    },

    # Render the heatmap plot
    renderHeatmapPlot = function(data, sourceSelect, drawFormat) {
      heatmapTable <- private$extractFunctionVsFunctionEdgelist(data)

      drawFormatColumn <- switch(
        drawFormat,
        "Term_ID" = "Id",
        "Function" = "Name"
      )
      yAxisColumn <- paste0("Source ", drawFormatColumn)
      xAxisColumn <- paste0("Target ", drawFormatColumn)

      # Store for category arrays
      private$.heatmapTable <- heatmapTable
      private$.yAxisColumn <- yAxisColumn
      private$.xAxisColumn <- xAxisColumn

      entriesCount <- length(unique(heatmapTable$`Source Name`))
      height <- private$calculateHeight(entriesCount)

      private$setRenderedData(data)
      private$renderHeatmapPlotly(heatmapTable, yAxisColumn, xAxisColumn,
                                  "Similarity Score %", height,
                                  TRUE, "Similarity %")
    },

    renderHeatmapPlotly = function(heatmapTable, yAxisColumn, xAxisColumn,
                                    weightColumn, height,
                                    showColorbar, colorbarTitle) {
      yCategories <- rev(unique(heatmapTable[[yAxisColumn]]))
      xCategories <- unique(heatmapTable[[xAxisColumn]])

      private$.output$plot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = heatmapTable,
          y = heatmapTable[[yAxisColumn]],
          x = heatmapTable[[xAxisColumn]],
          z = heatmapTable[[weightColumn]],
          type = 'heatmap',
          colorscale = private$.colorscale,
          hoverinfo = "text",
          hovertext = ~paste0("Source: ", `Source Name`, "\nTarget: ", `Target Name`,
                              "\nSimilarity: ", `Similarity Score %`, "%"),
          height = height,
          source = self$id,
          showscale = showColorbar,
          colorbar = list(title = colorbarTitle)
        ) %>%
          plotly::layout(
            xaxis = list(showgrid = FALSE, categoryorder = "array", categoryarray = xCategories),
            yaxis = list(showgrid = FALSE, categoryorder = "array", categoryarray = yCategories)
          ) %>%
          plotly::config(doubleClick = "reset+autosize")
      })
    },

    # Get category arrays for highlighting
    getHeatmapCategoryArrays = function() {
      heatmapTable <- private$.heatmapTable
      yAxisColumn <- private$.yAxisColumn
      xAxisColumn <- private$.xAxisColumn

      if (is.null(heatmapTable)) return(NULL)

      list(
        x = unique(heatmapTable[[xAxisColumn]]),
        y = rev(unique(heatmapTable[[yAxisColumn]]))
      )
    },

    # Render table from selection
    renderTableFromSelection = function() {
      originalData <- private$.originalData()
      if (is.null(originalData) || nrow(originalData) == 0) return()

      selectedTerms <- private$getTermsFromSelectedPairs()

      if (length(selectedTerms) == 0) {
        tableData <- originalData
      } else {
        tableData <- originalData[originalData$Term_ID_noLinks %in% selectedTerms, , drop = FALSE]
      }

      if (nrow(tableData) > 0) {
        private$.currentView(tableData)
        private$setUpdateSource("selection")
        private$markProgrammaticUpdate()
        private$renderTable(tableData, unique(as.character(tableData$Source)))
      }
    },

    renderBothFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      private$markProgrammaticUpdate()
      private$renderTable(data, sourceSelect)
      private$renderHeatmapPlot(data, sourceSelect, drawFormat)
    },

    renderPlotFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      private$renderHeatmapPlot(data, sourceSelect, drawFormat)
    },

    renderPlotWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      private$setRenderedData(data)
      private$renderHeatmapPlot(data, sourceSelect, drawFormat)
    },

    .heatmapTable = NULL,
    .yAxisColumn = NULL,
    .xAxisColumn = NULL
  )
)


# =============================================================================
# HEATMAP3 OUTPUT SESSION (Gene vs Gene)
# =============================================================================

Heatmap3OutputSession <- R6::R6Class(
  "Heatmap3OutputSession",
  inherit = HeatmapOutputSession,

  public = list(
    #' Initialize
    #' @param outputType Output type identifier from config (e.g., "heatmap3")
    initialize = function(runKey, enrichSession, outputType, instance = 1) {
      super$initialize(runKey, enrichSession, outputType = outputType, instance = instance)
    },

    #' Generate namespaced UI
    #'
    #' Composes UI from base class fragments.
    #' Heatmap3 (Genes vs Genes) has no drawFormat control.
    ui = function() {
      ns <- shiny::NS(self$id)
      enrichmentType <- "functional"
      uiTermKeyword <- UI_TERM_KEYWORD[[enrichmentType]]

      shiny::tagList(
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(4, self$datasourcePickerUI(ns)),
          shiny::column(4, self$termCountSliderUI(ns, uiTermKeyword)),
          shiny::column(4, self$sortModeUI(ns, uiTermKeyword))
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          )
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "heatmapOutput",
          plotly::plotlyOutput(ns("plot"))
        ),
        shiny::tags$br(),
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate heatmap
    generate = function(sourceSelect, sortMode, termCount) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for heatmap.")
          return()
        }

        # Filter by source and top N
        filteredData <- private$filterData(results, sourceSelect, sortMode, termCount)
        if (is.null(filteredData) || nrow(filteredData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Store as original data
        private$setOriginalData(filteredData)

        # Clear previous selections
        private$clearPairSelection()
        private$clearCellSelection()

        # Render table
        private$renderTable(filteredData, sourceSelect)

        # Transform and render heatmap
        private$renderHeatmapPlot(filteredData, sourceSelect)

      }, error = function(e) {
        renderWarning(paste("Cannot create heatmap:", e$message))
      }, finally = {
        removeModal()
      })
    },

    #' Handle click event
    handleClick = function(clickData) {
      if (is.null(clickData)) return()

      geneX <- as.character(clickData$x)
      geneY <- as.character(clickData$y)

      if (!is.null(geneX) && !is.null(geneY)) {
        private$togglePairSelection(geneX, geneY)
        private$renderTableFromSelection()
        private$toggleCellSelection(geneX, geneY)
        private$updateHeatmapHighlighting()
      }
    },

    #' Set up server logic
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            if (!is.null(input$sourceSelect)) {
              self$generate(input$sourceSelect, input$sortMode, input$termCountSlider)
            }
          },
          ignoreInit = TRUE
        )

        # Reset button
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          { self$handleReset() },
          ignoreInit = TRUE
        )

        # Table filter
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          { self$handleTableFilter(input$table_rows_all) },
          ignoreInit = TRUE
        )

        # Plotly click observer
        # priority = "event" ensures each click fires even if same cell clicked twice
        private$.observers$plotClick <- shiny::observeEvent(
          plotly::event_data("plotly_click", source = self$id, priority = "event"),
          {
            clickData <- plotly::event_data("plotly_click", source = self$id)
            self$handleClick(clickData)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )
      })
    },

    #' Handle table filter
    handleTableFilter = function(filteredRowIndices) {
      if (!private$shouldProcessUpdate("table")) return()

      currentData <- private$.currentView()
      if (is.null(currentData) || nrow(currentData) == 0) return()
      if (is.null(filteredRowIndices) || length(filteredRowIndices) == 0) return()
      if (max(filteredRowIndices) > nrow(currentData)) return()
      if (private$isProgrammaticUpdate()) return()

      if (length(filteredRowIndices) == nrow(currentData)) {
        private$setUpdateSource("table_filter")
        private$renderPlotFromCurrentView()
        return()
      }

      filteredData <- currentData[filteredRowIndices, , drop = FALSE]
      if (nrow(filteredData) == 0) return()

      private$clearPairSelection()
      private$clearCellSelection()
      private$setUpdateSource("table_filter")
      private$renderPlotWithData(filteredData)
    }
  ),

  private = list(
    # Extract Gene vs Gene edgelist
    extractGeneVsGeneEdgelist = function(enrichmentData) {
      genesEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
      genesEdgelist <- tidyr::separate_rows(genesEdgelist, `Positive Hits`, sep = ", ")
      genesEdgelist <- merge(genesEdgelist, genesEdgelist,
                             by.x = "Term_ID_noLinks", by.y = "Term_ID_noLinks")
      genesEdgelist$`Term_ID_noLinks` <- NULL
      # Create common functions counts column
      genesEdgelist <- data.table::setDT(
        genesEdgelist)[, list(`Common Functions` = .N), names(genesEdgelist)]
      colnames(genesEdgelist) <- c("Source Name", "Target Name", "Common Functions")
      return(genesEdgelist)
    },

    # Render the heatmap plot
    renderHeatmapPlot = function(data, sourceSelect) {
      heatmapTable <- private$extractGeneVsGeneEdgelist(data)

      # Store for category arrays
      private$.heatmapTable <- heatmapTable

      entriesCount <- length(unique(heatmapTable$`Source Name`))
      height <- private$calculateHeight(entriesCount)

      private$setRenderedData(data)
      private$renderHeatmapPlotly(heatmapTable, height)
    },

    renderHeatmapPlotly = function(heatmapTable, height) {
      yCategories <- rev(unique(heatmapTable$`Source Name`))
      xCategories <- unique(heatmapTable$`Target Name`)

      private$.output$plot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = heatmapTable,
          y = heatmapTable$`Source Name`,
          x = heatmapTable$`Target Name`,
          z = heatmapTable$`Common Functions`,
          type = 'heatmap',
          colorscale = private$.colorscale,
          hoverinfo = "text",
          hovertext = ~paste0("Gene 1: ", `Source Name`, "\nGene 2: ", `Target Name`,
                              "\nCommon Functions: ", `Common Functions`),
          height = height,
          source = self$id,
          showscale = TRUE,
          colorbar = list(title = "Common Functions")
        ) %>%
          plotly::layout(
            xaxis = list(showgrid = FALSE, categoryorder = "array", categoryarray = xCategories),
            yaxis = list(showgrid = FALSE, categoryorder = "array", categoryarray = yCategories)
          ) %>%
          plotly::config(doubleClick = "reset+autosize")
      })
    },

    # Get category arrays for highlighting
    getHeatmapCategoryArrays = function() {
      heatmapTable <- private$.heatmapTable
      if (is.null(heatmapTable)) return(NULL)

      list(
        x = unique(heatmapTable$`Target Name`),
        y = rev(unique(heatmapTable$`Source Name`))
      )
    },

    # Render table from gene pair selection
    renderTableFromSelection = function() {
      originalData <- private$.originalData()
      if (is.null(originalData) || nrow(originalData) == 0) return()

      genePairs <- private$.selectedPairs()

      if (nrow(genePairs) == 0) {
        tableData <- originalData
      } else {
        # Union of terms containing both genes for each selected pair
        allTermIds <- character(0)
        for (i in seq_len(nrow(genePairs))) {
          geneX <- genePairs$source[i]
          geneY <- genePairs$target[i]

          if (geneX == geneY) {
            # Diagonal: terms containing the single gene
            matchingTerms <- originalData$Term_ID_noLinks[
              sapply(originalData$`Positive Hits`, function(hits) {
                genes <- unlist(strsplit(as.character(hits), ",\\s*"))
                geneX %in% genes
              })
            ]
          } else {
            # Off-diagonal: terms containing both genes
            matchingTerms <- originalData$Term_ID_noLinks[
              sapply(originalData$`Positive Hits`, function(hits) {
                genes <- unlist(strsplit(as.character(hits), ",\\s*"))
                geneX %in% genes && geneY %in% genes
              })
            ]
          }
          allTermIds <- union(allTermIds, matchingTerms)
        }

        tableData <- originalData[originalData$Term_ID_noLinks %in% allTermIds, , drop = FALSE]
      }

      if (nrow(tableData) > 0) {
        private$.currentView(tableData)
        private$setUpdateSource("selection")
        private$markProgrammaticUpdate()
        private$renderTable(tableData, unique(as.character(tableData$Source)))
      }
    },

    renderBothFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)

      private$markProgrammaticUpdate()
      private$renderTable(data, sourceSelect)
      private$renderHeatmapPlot(data, sourceSelect)
    },

    renderPlotFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      private$renderHeatmapPlot(data, sourceSelect)
    },

    renderPlotWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      private$setRenderedData(data)
      private$renderHeatmapPlot(data, sourceSelect)
    },

    .heatmapTable = NULL
  )
)
