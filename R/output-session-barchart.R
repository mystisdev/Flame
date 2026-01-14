# =============================================================================
# BARCHART OUTPUT SESSION
# =============================================================================
# R6 class that owns barchart plot + synchronized table + state.
# Inherits from OutputSession base class.
#
# Uses proper Shiny module pattern:
# - ui() method generates namespaced UI
# - server() uses moduleServer() for namespaced input/output
# - No need to pass root input/output - namespacing handles it
# =============================================================================

BarchartOutputSession <- R6::R6Class(
  "BarchartOutputSession",
  inherit = OutputSession,

  public = list(
    #' Initialize a new BarchartOutputSession
    #'
    #' @param runKey Parent run key (e.g., "functional_gProfiler_1")
    #' @param enrichSession Parent ORAEnrichmentSession object
    initialize = function(runKey, enrichSession) {
      super$initialize(runKey, enrichSession, outputType = "barchart")
    },

    #' Generate namespaced UI for the barchart panel
    #'
    #' Composes UI from base class fragments.
    #' Called by parent session, result is inserted via insertUI().
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
            8,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          ),
          shiny::column(
            4,
            shiny::radioButtons(
              inputId = ns("drawFormat"),
              label = "Bar labels",
              choices = c("Function", "Term_ID"),
              inline = TRUE
            )
          )
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "barchartOutput",
          plotly::plotlyOutput(ns("plot"))
        ),
        shiny::tags$br(),
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate barchart from current inputs
    #'
    #' Called when user clicks "Generate Barchart" button.
    #' Filters data, stores state, renders table and plot.
    #'
    #' @param sourceSelect Character vector of selected datasources
    #' @param mode "Enrichment Score" or "-log10Pvalue"
    #' @param slider Number of top items to display
    #' @param drawFormat Column to use for Y-axis labels ("Function" or "Term_ID")
    generate = function(sourceSelect, mode, slider, drawFormat) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Barchart.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for barchart.")
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

        # Order and render barchart
        column <- private$getModeColumn(mode)
        orderedData <- private$orderByColumn(filteredData, column, drawFormat)
        private$setRenderedData(orderedData)
        private$renderPlot(orderedData, column, drawFormat)

      }, error = function(e) {
        renderWarning(paste("Cannot create barchart:", e$message))
      }, finally = {
        removeModal()
      })
    },

    #' Handle plot click event
    #'
    #' Called when user clicks on a bar in the chart.
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
    #' Called when user selects multiple bars via lasso or box.
    #' Adds selected terms to current selection (cumulative).
    #'
    #' @param selectionData Selection data from plotly_selected event
    handleSelection = function(selectionData) {
      if (is.null(selectionData) || nrow(selectionData) == 0) return()

      # Extract term IDs from selection
      selectedValues <- selectionData$customdata
      if (is.null(selectedValues) || length(selectedValues) == 0) {
        selectedValues <- selectionData$y
      }
      if (is.null(selectedValues) || length(selectedValues) == 0) return()

      # Resolve to Term_ID_noLinks
      resolvedTermIds <- character(0)
      renderedData <- private$.renderedData()
      if (is.null(renderedData)) return()

      for (val in selectedValues) {
        if (val %in% renderedData$Term_ID_noLinks) {
          resolvedTermIds <- c(resolvedTermIds, val)
        } else if (val %in% renderedData$Function) {
          idx <- match(val, renderedData$Function)
          if (!is.na(idx)) {
            resolvedTermIds <- c(resolvedTermIds, renderedData$Term_ID_noLinks[idx])
          }
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
    #' Input/output are automatically namespaced - no need to pass root objects.
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
            drawFormat <- input$drawFormat

            if (!is.null(sourceSelect)) {
              self$generate(sourceSelect, sortMode, termCount, drawFormat)
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
        # priority = "event" ensures each click fires even if same bar clicked twice
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
    # Barchart-specific Data Filtering and Transformation
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

    getModeColumn = function(mode) {
      if (mode == "Enrichment Score") {
        return("Enrichment Score %")
      } else {
        return("-log10Pvalue")
      }
    },

    orderByColumn = function(data, column, drawFormatColumn) {
      data[[drawFormatColumn]] <- factor(
        data[[drawFormatColumn]],
        levels = unique(data[[drawFormatColumn]])[
          order(data[[column]], decreasing = FALSE)
        ]
      )
      return(data)
    },

    extractTermId = function(clickData) {
      if (!is.null(clickData$customdata)) {
        return(clickData$customdata[[1]])
      }
      return(NULL)
    },

    # -------------------------------------------------------------------------
    # Barchart-specific Rendering
    # -------------------------------------------------------------------------

    calculatePlotHeight = function(entriesCount) {
      entriesCount * private$.entryHeightPx + private$.minPlotHeight
    },

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

    renderPlot = function(data, column, drawFormatColumn) {
      height <- private$calculatePlotHeight(nrow(data))
      private$renderBarchartPlotly(data, column, drawFormatColumn, height)
    },

    renderPlotFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      # Get sortMode from module session
      sortMode <- shiny::isolate(private$.moduleSession$input$sortMode)
      column <- private$getModeColumn(sortMode)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      orderedData <- private$orderByColumn(data, column, drawFormat)
      private$setRenderedData(orderedData)

      height <- private$calculatePlotHeight(nrow(orderedData))
      private$renderBarchartPlotly(orderedData, column, drawFormat, height)
    },

    renderPlotWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      sortMode <- shiny::isolate(private$.moduleSession$input$sortMode)
      column <- private$getModeColumn(sortMode)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      orderedData <- private$orderByColumn(data, column, drawFormat)
      private$setRenderedData(orderedData)

      height <- private$calculatePlotHeight(nrow(orderedData))
      private$renderBarchartPlotly(orderedData, column, drawFormat, height)
    },

    # Renders to namespaced "plot" output
    renderBarchartPlotly = function(data, column, drawFormatColumn, height) {
      private$.output$plot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = data,
          x = data[[column]],
          y = data[[drawFormatColumn]],
          type = 'bar',
          orientation = 'h',
          color = ~Source,
          colors = DATASOURCE_COLORS,
          marker = list(
            line = list(color = "rgba(0,0,0,0.3)", width = 1)
          ),
          text = sprintf("%s/%s",
                         data[["Intersection Size"]],
                         data[["Term Size"]]),
          textfont = list(color = '#000000', size = 16),
          textposition = 'outside',
          hoverinfo = "text",
          hovertext = ~paste0("<b>Term ID</b>: ", Term_ID_noLinks,
                              "\n<b>Function</b>: ", Function,
                              "\nEnrichment Score %: ", `Enrichment Score %`,
                              "\n-log10Pvalue: ", `-log10Pvalue`),
          height = height,
          source = self$id,
          customdata = ~Term_ID_noLinks,
          unselected = list(marker = list(opacity = 1))
        ) %>%
          plotly::layout(legend = list(
            title = list(text = "Source"),
            itemclick = FALSE,
            itemdoubleclick = FALSE
          ))
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

      # Use namespaced plot ID with module session
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
          lineColors <- rep("rgba(0,0,0,0.3)", n)
          lineWidths <- rep(1, n)
        } else {
          isSelected <- traceData$Term_ID_noLinks %in% selectedTerms
          lineColors <- ifelse(isSelected, "black", "rgba(0,0,0,0.3)")
          lineWidths <- ifelse(isSelected, 2, 1)
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
