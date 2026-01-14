# =============================================================================
# SCATTER PLOT OUTPUT SESSION
# =============================================================================
# R6 class that owns scatter plot + synchronized table + state.
# Inherits from OutputSession base class.
#
# Uses proper Shiny module pattern:
# - ui() method generates namespaced UI
# - server() uses moduleServer() for namespaced input/output
# =============================================================================

ScatterOutputSession <- R6::R6Class(
  "ScatterOutputSession",
  inherit = OutputSession,

  public = list(
    #' Initialize a new ScatterOutputSession
    #'
    #' @param runKey Parent run key (e.g., "functional_gProfiler_1")
    #' @param enrichSession Parent ORAEnrichmentSession object
    initialize = function(runKey, enrichSession) {
      super$initialize(runKey, enrichSession, outputType = "scatterPlot")
    },

    #' Generate namespaced UI for the scatter plot panel
    #'
    #' Composes UI from base class fragments.
    #' Scatter plot has no drawFormat control (uses Function).
    #'
    #' @return Shiny UI elements (tagList)
    ui = function() {
      ns <- shiny::NS(self$id)

      # Get enrichment type for UI labels
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
            12,  # Full width - no draw format control for scatter
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          )
        ),
        shiny::tags$hr(),
        plotly::plotlyOutput(ns("plot")),
        shiny::tags$br(),
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate scatter plot from current inputs
    #'
    #' Called when user clicks "Generate" button.
    #' Filters data, adds jitter, stores state, renders table and plot.
    #'
    #' @param sourceSelect Character vector of selected datasources
    #' @param mode "Enrichment Score" or "-log10Pvalue"
    #' @param slider Number of top items to display
    generate = function(sourceSelect, mode, slider) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Scatter Plot.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for scatter plot.")
          return()
        }

        # Filter data by selected datasources and top N
        filteredData <- private$filterData(results, sourceSelect, mode, slider)
        if (is.null(filteredData) || nrow(filteredData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Store as original data (immutable baseline) - from base class
        private$setOriginalData(filteredData)

        # Render synchronized table
        private$renderTable(filteredData, sourceSelect)

        # Add jitter and render scatter plot
        jitteredData <- private$addJitter(filteredData)
        private$setRenderedData(jitteredData)
        private$renderPlot(jitteredData)

      }, error = function(e) {
        renderWarning(paste("Cannot create scatter plot:", e$message))
      }, finally = {
        removeModal()
      })
    },

    #' Handle plot click event
    #'
    #' Called when user clicks on a point in the scatter plot.
    #' Toggles term selection and updates table/highlighting.
    #'
    #' @param clickData Event data from plotly_click
    handleClick = function(clickData) {
      if (is.null(clickData)) return()

      # Extract term ID from click
      termId <- private$extractTermId(clickData)
      if (is.null(termId)) return()

      # Toggle selection
      currentSelection <- private$.selectedTerms()
      if (termId %in% currentSelection) {
        newSelection <- setdiff(currentSelection, termId)
      } else {
        newSelection <- c(currentSelection, termId)
      }
      private$.selectedTerms(newSelection)

      # Update table to show selection
      private$updateTableFromSelection()

      # Update plot highlighting
      private$updatePlotHighlighting()
    },

    #' Handle lasso/box selection
    #'
    #' Called when user selects multiple points via lasso or box.
    #' Adds selected terms to current selection (cumulative).
    #'
    #' @param selectionData Selection data from plotly_selected event
    handleSelection = function(selectionData) {
      if (is.null(selectionData) || nrow(selectionData) == 0) return()

      # Extract term IDs from selection - scatter uses customdata
      selectedValues <- selectionData$customdata
      if (is.null(selectedValues) || length(selectedValues) == 0) return()

      # Resolve to Term_ID_noLinks
      resolvedTermIds <- character(0)
      renderedData <- private$.renderedData()
      if (is.null(renderedData)) return()

      for (val in selectedValues) {
        if (val %in% renderedData$Term_ID_noLinks) {
          resolvedTermIds <- c(resolvedTermIds, val)
        }
      }

      if (length(resolvedTermIds) == 0) return()

      # Add to existing selection (cumulative)
      currentSelection <- private$.selectedTerms()
      newSelection <- unique(c(currentSelection, resolvedTermIds))
      private$.selectedTerms(newSelection)

      # Update table to show selection
      private$updateTableFromSelection()

      # Update plot highlighting
      private$updatePlotHighlighting()
    },

    #' Handle table filter change
    #'
    #' Called when user filters the synchronized table.
    #' Re-renders plot with filtered subset.
    #'
    #' @param filteredRowIndices Row indices from DT filter
    handleTableFilter = function(filteredRowIndices) {
      if (!private$shouldProcessUpdate("table")) return()

      currentData <- private$.currentView()
      if (is.null(currentData) || nrow(currentData) == 0) return()

      if (is.null(filteredRowIndices) || length(filteredRowIndices) == 0) return()
      if (max(filteredRowIndices) > nrow(currentData)) return()

      # Skip programmatic updates
      if (private$isProgrammaticUpdate()) return()

      # All rows visible = filter cleared
      if (length(filteredRowIndices) == nrow(currentData)) {
        private$setUpdateSource("table_filter")
        private$renderPlotFromCurrentView()
        return()
      }

      # Render plot with filtered subset
      filteredData <- currentData[filteredRowIndices, , drop = FALSE]
      if (nrow(filteredData) == 0) return()

      # Clear selection (may reference terms no longer visible)
      private$.selectedTerms(character(0))

      private$setUpdateSource("table_filter")
      private$renderPlotWithData(filteredData)
    },

    #' Set up server-side logic using moduleServer
    #'
    #' Uses Shiny's moduleServer for proper namespacing.
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        # Store module session for plotlyProxy
        private$.moduleSession <- session

        # Store output for rendering
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Initialize controls from base class (populates picker, sets slider range)
        private$initializeControls(session)

        # Generate button observer
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            sourceSelect <- input$sourceSelect
            sortMode <- input$sortMode
            termCount <- input$termCountSlider

            if (!is.null(sourceSelect)) {
              self$generate(sourceSelect, sortMode, termCount)
            }
          },
          ignoreInit = TRUE
        )

        # Table filter observer
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          {
            self$handleTableFilter(input$table_rows_all)
          },
          ignoreInit = TRUE
        )

        # Reset button observer
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          {
            self$handleReset()
          },
          ignoreInit = TRUE
        )

        # Plotly click observer - uses namespaced source (self$id)
        # priority = "event" ensures each click fires even if same point clicked twice
        private$.observers$plotClick <- shiny::observeEvent(
          plotly::event_data("plotly_click", source = self$id, priority = "event"),
          {
            clickData <- plotly::event_data("plotly_click", source = self$id)
            self$handleClick(clickData)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Plotly selection observer (lasso/box)
        private$.observers$plotSelect <- shiny::observeEvent(
          plotly::event_data("plotly_selected", source = self$id, priority = "event"),
          {
            selectionData <- plotly::event_data("plotly_selected", source = self$id)
            self$handleSelection(selectionData)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )
      })
    }
  ),

  private = list(
    # -------------------------------------------------------------------------
    # Scatter-specific Data Filtering and Transformation
    # -------------------------------------------------------------------------

    filterData = function(results, sourceSelect, mode, slider) {
      filteredData <- subset(results, Source %in% sourceSelect)
      if (nrow(filteredData) == 0) return(NULL)

      # Always sort - the "already sorted" assumption is wrong after Source filtering
      if (mode == "Enrichment Score") {
        filteredData <- filteredData[order(-filteredData$`Enrichment Score %`), ]
      } else {
        # Sort by -log10Pvalue descending
        filteredData <- filteredData[order(-filteredData$`-log10Pvalue`), ]
      }

      filteredData <- head(filteredData, slider)
      filteredData$`Positive Hits` <- gsub(",", ", ", filteredData$`Positive Hits`)
      filteredData$Source <- as.factor(filteredData$Source)

      return(filteredData)
    },

    # Add jitter to prevent point overlap
    # Copied from plot-scatter.R addJitter()
    addJitter = function(scatterData) {
      size <- nrow(scatterData)
      scatterData$`Enrichment Score %_jittered` <-
        scatterData$`Enrichment Score %` + runif(size, min = -0.5, max = 0.5)
      scatterData$`-log10Pvalue_jittered` <-
        scatterData$`-log10Pvalue` + runif(size, min = -0.005, max = 0.005)
      return(scatterData)
    },

    extractTermId = function(clickData) {
      if (!is.null(clickData$customdata)) {
        return(clickData$customdata[[1]])
      }
      return(NULL)
    },

    # -------------------------------------------------------------------------
    # Scatter-specific Rendering
    # -------------------------------------------------------------------------

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

    renderPlot = function(data) {
      private$renderScatterPlotly(data)
    },

    renderPlotFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      # Add jitter for rendering
      jitteredData <- private$addJitter(data)
      private$setRenderedData(jitteredData)
      private$renderScatterPlotly(jitteredData)
    },

    renderPlotWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      # Add jitter for rendering
      jitteredData <- private$addJitter(data)
      private$setRenderedData(jitteredData)
      private$renderScatterPlotly(jitteredData)
    },

    # Renders to namespaced "plot" output
    # Copied from func-render.R renderScatterPlot()
    renderScatterPlotly = function(scatterData) {
      private$.output$plot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = scatterData,
          x = ~`-log10Pvalue_jittered`,
          y = ~`Enrichment Score %_jittered`,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = 15,
            line = list(
              color = 'rgb(0, 0, 0)',
              width = 1
            )
          ),
          color = ~Source,
          colors = DATASOURCE_COLORS,
          hoverinfo = "text",
          hovertext = ~paste0("<b>Term ID</b>: ", Term_ID_noLinks,
                              "\n<b>Function</b>: ", Function,
                              "\nEnrichment Score %: ", `Enrichment Score %`,
                              "\n-log10Pvalue: ", `-log10Pvalue`),
          source = self$id,
          customdata = ~Term_ID_noLinks,
          unselected = list(marker = list(opacity = 1))
        ) %>%
          plotly::layout(
            xaxis = list(title = "-log10Pvalue"),
            yaxis = list(title = "Enrichment Score"),
            legend = list(
              title = list(text = "Source"),
              itemclick = FALSE,
              itemdoubleclick = FALSE
            )
          )
      })
    },

    # Required by base class handleReset()
    renderBothFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      # Render table
      private$markProgrammaticUpdate()
      private$renderTable(data, unique(as.character(data$Source)))

      # Render plot
      private$renderPlotFromCurrentView()
    },

    updateTableFromSelection = function() {
      selectedTerms <- private$.selectedTerms()
      originalData <- private$.originalData()

      if (is.null(originalData) || nrow(originalData) == 0) return()

      if (length(selectedTerms) == 0) {
        tableData <- originalData
      } else {
        tableData <- originalData[originalData$Term_ID_noLinks %in% selectedTerms,
                                  , drop = FALSE]
        if (nrow(tableData) == 0) return()
      }

      private$.currentView(tableData)
      private$setUpdateSource("selection")

      private$markProgrammaticUpdate()
      private$renderTable(tableData, unique(as.character(tableData$Source)))
    },

    # Multi-trace highlighting via plotlyProxy
    updatePlotHighlighting = function() {
      selectedTerms <- private$.selectedTerms()
      renderedData <- private$.renderedData()

      if (is.null(renderedData) || nrow(renderedData) == 0) return()

      proxy <- plotly::plotlyProxy("plot", private$.moduleSession)

      if (is.factor(renderedData$Source)) {
        sources <- levels(renderedData$Source)
        sources <- sources[sources %in% unique(renderedData$Source)]
      } else {
        sources <- sort(unique(as.character(renderedData$Source)))
      }

      for (i in seq_along(sources)) {
        traceData <- renderedData[renderedData$Source == sources[i], ]
        n <- nrow(traceData)
        traceIndex <- i - 1

        if (length(selectedTerms) == 0) {
          lineColors <- rep("rgb(0, 0, 0)", n)
          lineWidths <- rep(1, n)
        } else {
          isSelected <- traceData$Term_ID_noLinks %in% selectedTerms
          lineColors <- ifelse(isSelected, "black", "rgb(0, 0, 0)")
          lineWidths <- ifelse(isSelected, 3, 1)
        }

        plotly::plotlyProxyInvoke(proxy, "restyle",
                                  "marker.line.color", list(lineColors), traceIndex)
        plotly::plotlyProxyInvoke(proxy, "restyle",
                                  "marker.line.width", list(lineWidths), traceIndex)
      }
    }

    # NOTE: initializeControls and updateSliderMax removed - now in OutputSession base class
  )
)
