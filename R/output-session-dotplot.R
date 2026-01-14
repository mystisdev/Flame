# =============================================================================
# DOT PLOT OUTPUT SESSION
# =============================================================================
# R6 class that owns dot plot + synchronized table + state.
# Inherits from OutputSession base class.
#
# Uses proper Shiny module pattern:
# - ui() method generates namespaced UI
# - server() uses moduleServer() for namespaced input/output
# =============================================================================

DotPlotOutputSession <- R6::R6Class(
  "DotPlotOutputSession",
  inherit = OutputSession,

  public = list(
    #' Initialize a new DotPlotOutputSession
    #'
    #' @param runKey Parent run key (e.g., "functional_gProfiler_1")
    #' @param enrichSession Parent ORAEnrichmentSession object
    initialize = function(runKey, enrichSession) {
      super$initialize(runKey, enrichSession, outputType = "dotPlot")
    },

    #' Generate namespaced UI for the dot plot panel
    #'
    #' Composes UI from base class fragments.
    #' Note: sortMode has custom choices (includes "Gene Ratio").
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
          shiny::column(
            4,
            # Custom sortMode with "Gene Ratio" option (not in base class)
            shiny::radioButtons(
              inputId = ns("sortMode"),
              label = paste0("Order retrieved ", uiTermKeyword, " by:"),
              choices = c("-log10Pvalue", "Enrichment Score", "Gene Ratio"),
              inline = TRUE
            )
          )
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
              label = "Dot labels",
              choices = c("Function", "Term_ID"),
              inline = TRUE
            )
          )
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "dotPlotOutput",
          plotly::plotlyOutput(ns("plot"))
        ),
        shiny::tags$br(),
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate dot plot from current inputs
    #'
    #' Called when user clicks "Generate" button.
    #' Filters data, computes Gene Ratio, stores state, renders table and plot.
    #'
    #' @param sourceSelect Character vector of selected datasources
    #' @param mode "Enrichment Score", "-log10Pvalue", or "Gene Ratio"
    #' @param slider Number of top items to display
    #' @param drawFormat Column to use for Y-axis labels ("Function" or "Term_ID")
    generate = function(sourceSelect, mode, slider, drawFormat) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Dot Plot.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for dot plot.")
          return()
        }

        # Filter data by selected datasources and top N
        filteredData <- private$filterData(results, sourceSelect, mode, slider)
        if (is.null(filteredData) || nrow(filteredData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Compute Gene Ratio: Intersection Size / Query Size
        filteredData$`Gene Ratio` <-
          filteredData$`Intersection Size` / filteredData$`Query size`

        # Store as original data (immutable baseline) - from base class
        private$setOriginalData(filteredData)

        # Render synchronized table
        private$renderTable(filteredData, sourceSelect)

        # Order and render dot plot
        orderedData <- private$orderForDotPlot(filteredData, mode, drawFormat)

        # Calculate dynamic height
        height <- nrow(orderedData) * private$.entryHeightPx + private$.minPlotHeight

        private$setRenderedData(orderedData)
        private$renderPlot(orderedData, drawFormat, height)

      }, error = function(e) {
        renderWarning(paste("Cannot create dot plot:", e$message))
      }, finally = {
        removeModal()
      })
    },

    #' Handle plot click event
    #'
    #' Called when user clicks on a point in the dot plot.
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

      # Extract term IDs from selection - dotplot uses customdata
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
    # Colorscale for dotplot (Viridis for consistency across app)
    .colorscale = "Viridis",

    # -------------------------------------------------------------------------
    # DotPlot-specific Configuration Constants (override base class)
    # -------------------------------------------------------------------------

    # DotPlot uses larger entry height than base (22 vs 18)
    .entryHeightPx = 22,

    # Dot size scaling range
    .dotSizeMin = 4,
    .dotSizeMax = 25,

    # -------------------------------------------------------------------------
    # DotPlot-specific Data Filtering and Transformation
    # -------------------------------------------------------------------------

    filterData = function(results, sourceSelect, mode, slider) {
      filteredData <- subset(results, Source %in% sourceSelect)
      if (nrow(filteredData) == 0) return(NULL)

      # For sorting, need Gene Ratio if that mode is selected
      if (mode == "Gene Ratio") {
        filteredData$`Gene Ratio` <-
          filteredData$`Intersection Size` / filteredData$`Query size`
        filteredData <- filteredData[order(-filteredData$`Gene Ratio`), ]
      } else if (mode == "Enrichment Score") {
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

    # Order Y-axis based on selected mode
    # Copied from orderForDotPlot() in plot-dotplot.R
    orderForDotPlot = function(data, mode, drawFormatColumn) {
      orderColumn <- switch(
        mode,
        "Enrichment Score" = "Enrichment Score %",
        "Gene Ratio" = "Gene Ratio",
        "-log10Pvalue"  # default
      )

      # Set factor levels ordered by the column value (ascending = bottom-to-top on Y-axis)
      data[[drawFormatColumn]] <- factor(
        data[[drawFormatColumn]],
        levels = unique(data[[drawFormatColumn]])[
          order(data[[orderColumn]], decreasing = FALSE)])

      return(data)
    },

    extractTermId = function(clickData) {
      if (!is.null(clickData$customdata)) {
        return(clickData$customdata[[1]])
      }
      return(NULL)
    },

    # -------------------------------------------------------------------------
    # DotPlot-specific Rendering
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

    renderPlot = function(data, drawFormatColumn, height) {
      private$renderDotPlotly(data, drawFormatColumn, height)
    },

    renderPlotFromCurrentView = function() {
      data <- private$.currentView()
      if (is.null(data) || nrow(data) == 0) return()

      # Get sortMode from module session
      sortMode <- shiny::isolate(private$.moduleSession$input$sortMode)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      # Ensure Gene Ratio is computed
      if (!"Gene Ratio" %in% names(data)) {
        data$`Gene Ratio` <- data$`Intersection Size` / data$`Query size`
      }

      orderedData <- private$orderForDotPlot(data, sortMode, drawFormat)
      height <- nrow(orderedData) * private$.entryHeightPx + private$.minPlotHeight

      private$setRenderedData(orderedData)
      private$renderDotPlotly(orderedData, drawFormat, height)
    },

    renderPlotWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      sortMode <- shiny::isolate(private$.moduleSession$input$sortMode)
      drawFormat <- shiny::isolate(private$.moduleSession$input$drawFormat)

      # Ensure Gene Ratio is computed
      if (!"Gene Ratio" %in% names(data)) {
        data$`Gene Ratio` <- data$`Intersection Size` / data$`Query size`
      }

      orderedData <- private$orderForDotPlot(data, sortMode, drawFormat)
      height <- nrow(orderedData) * private$.entryHeightPx + private$.minPlotHeight

      private$setRenderedData(orderedData)
      private$renderDotPlotly(orderedData, drawFormat, height)
    },

    # Renders to namespaced "plot" output
    # Copied from renderDotPlot() in func-render.R
    renderDotPlotly = function(dotPlotData, drawFormatColumn, height) {
      # Square root scaling for dot sizes
      minIntersection <- min(dotPlotData$`Intersection Size`)
      maxIntersection <- max(dotPlotData$`Intersection Size`)

      if (maxIntersection == minIntersection) {
        dotPlotData$scaledSize <- (private$.dotSizeMin + private$.dotSizeMax) / 2
      } else {
        dotPlotData$scaledSize <- private$.dotSizeMin +
          (private$.dotSizeMax - private$.dotSizeMin) *
          (sqrt(dotPlotData$`Intersection Size`) - sqrt(minIntersection)) /
          (sqrt(maxIntersection) - sqrt(minIntersection))
      }

      private$.output$plot <- plotly::renderPlotly({
        plotly::plot_ly(
          data = dotPlotData,
          x = ~`Gene Ratio`,
          y = dotPlotData[[drawFormatColumn]],
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = ~scaledSize,
            color = ~`-log10Pvalue`,
            colorscale = private$.colorscale,
            colorbar = list(
              title = "-log10(P-value)",
              tickformat = ".1f",
              len = 0.6,
              thickness = 15
            ),
            line = list(color = 'rgba(0,0,0,0.3)', width = 1)
          ),
          hoverinfo = "text",
          hovertext = ~paste0(
            "<b>Term ID</b>: ", Term_ID_noLinks,
            "\n<b>Function</b>: ", Function,
            "\nGene Ratio: ", round(`Gene Ratio`, 3),
            "\nIntersection Size: ", `Intersection Size`,
            "\nTerm Size: ", `Term Size`,
            "\nQuery Size: ", `Query size`,
            "\n-log10Pvalue: ", `-log10Pvalue`,
            "\nEnrichment Score %: ", `Enrichment Score %`
          ),
          height = height,
          source = self$id,
          customdata = ~Term_ID_noLinks,
          unselected = list(marker = list(opacity = 1))
        ) %>%
          plotly::layout(xaxis = list(title = "Gene Ratio"))
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

    # Single-trace highlighting via plotlyProxy (dotplot has one trace with color scale)
    updatePlotHighlighting = function() {
      selectedTerms <- private$.selectedTerms()
      renderedData <- private$.renderedData()

      if (is.null(renderedData) || nrow(renderedData) == 0) return()

      proxy <- plotly::plotlyProxy("plot", private$.moduleSession)

      n <- nrow(renderedData)

      if (length(selectedTerms) == 0) {
        lineColors <- rep("rgba(0,0,0,0.3)", n)
        lineWidths <- rep(1, n)
      } else {
        isSelected <- renderedData$Term_ID_noLinks %in% selectedTerms
        lineColors <- ifelse(isSelected, "black", "rgba(0,0,0,0.3)")
        lineWidths <- ifelse(isSelected, 2, 1)
      }

      # Dotplot has a single trace (trace index 0)
      plotly::plotlyProxyInvoke(proxy, "restyle",
                                "marker.line.color", list(lineColors), 0)
      plotly::plotlyProxyInvoke(proxy, "restyle",
                                "marker.line.width", list(lineWidths), 0)
    }

    # NOTE: initializeControls and updateSliderMax removed - now in OutputSession base class
  )
)
