# =============================================================================
# OUTPUT SESSION BASE CLASS
# =============================================================================
# Abstract base class for plot output sessions (barchart, scatter, dotplot, etc.)
# Each OutputSession owns its plot + synchronized table + three-tier state.
#
# Subclasses must implement:
# - ui() - generate namespaced UI elements
# - server() - set up moduleServer with observers
# - generate() - create plot from user inputs
#
# Common functionality provided:
# - Three-tier state management (originalData, currentView, renderedData)
# - Selection tracking (selectedTerms)
# - Cascade prevention (updateTracker, timing checks)
# - Observer cleanup
# =============================================================================

OutputSession <- R6::R6Class(
  "OutputSession",

  public = list(
    #' @field id Module namespace ID (e.g., "functional_gProfiler_1_barchart")
    id = NULL,

    #' @field runKey Parent run key (e.g., "functional_gProfiler_1")
    runKey = NULL,

    #' @field enrichSession Reference to parent ORAEnrichmentSession
    enrichSession = NULL,

    #' @field outputType Type of output (e.g., "barchart", "scatter", "dotplot")
    outputType = NULL,

    #' Initialize a new OutputSession
    #'
    #' @param runKey Parent run key (e.g., "functional_gProfiler_1")
    #' @param enrichSession Parent ORAEnrichmentSession object
    #' @param outputType Type of output (e.g., "barchart")
    initialize = function(runKey, enrichSession, outputType) {
      self$runKey <- runKey
      self$outputType <- outputType
      self$id <- paste(runKey, outputType, sep = "_")
      self$enrichSession <- enrichSession

      # Initialize reactive state
      private$.originalData <- shiny::reactiveVal(NULL)
      private$.currentView <- shiny::reactiveVal(NULL)
      private$.renderedData <- shiny::reactiveVal(NULL)
      private$.selectedTerms <- shiny::reactiveVal(character(0))

      # Update tracking for cascade prevention
      private$.updateTracker <- shiny::reactiveValues(
        counter = 0,
        source = "init",
        time = Sys.time()
      )
    },

    #' Generate namespaced UI for the output panel
    #'
    #' Must be implemented by subclasses.
    #'
    #' @return Shiny UI elements (tagList)
    ui = function() {
      stop("Subclass must implement ui()")
    },

    #' Set up server-side logic using moduleServer
    #'
    #' Must be implemented by subclasses.
    server = function() {
      stop("Subclass must implement server()")
    },

    #' Generate plot from current inputs
    #'
    #' Must be implemented by subclasses.
    generate = function(...) {
      stop("Subclass must implement generate()")
    },

    #' Handle reset view button
    #'
    #' Restores plot and table to original generated state.
    #' Can be overridden by subclasses if needed.
    handleReset = function() {
      # Clear selection
      private$.selectedTerms(character(0))

      originalData <- private$.originalData()
      if (is.null(originalData) || nrow(originalData) == 0) {
        shiny::showNotification("No data available. Please generate the plot first.",
                                type = "warning", duration = 3)
        return()
      }

      # Restore current view to original
      private$.currentView(originalData)
      private$setUpdateSource("reset")

      # Subclass should implement renderBothFromCurrentView()
      private$renderBothFromCurrentView()
    },

    #' Initialize the picker with datasources from enrichment results
    #' Called externally after results are updated (e.g., datasources changed)
    updateControls = function() {
      if (!is.null(private$.moduleSession)) {
        private$initializeControls(private$.moduleSession)
      }
    },

    #' Clear state and rendered content for refresh (datasources changed)
    #'
    #' Unlike cleanup(), this does NOT destroy observers. The session stays
    #' alive and functional - just with cleared outputs ready for new Generate.
    #' Used when enrichment results change but we keep the same session.
    clearForRefresh = function() {
      # Clear reactive state (but keep observers alive)
      private$.originalData(NULL)
      private$.currentView(NULL)
      private$.renderedData(NULL)
      private$.selectedTerms(character(0))

      # Reset update tracker
      private$.updateTracker$counter <- 0
      private$.updateTracker$source <- "refresh"
      private$.updateTracker$time <- Sys.time()

      # Render empty content to clear visible outputs
      if (!is.null(private$.output)) {
        tryCatch({
          private$.output$plot <- plotly::renderPlotly({})
          private$.output$table <- DT::renderDataTable(NULL)
        }, error = function(e) NULL)
      }
    },

    #' Clean up resources
    #'
    #' Destroys observers, clears state, and clears rendered outputs.
    cleanup = function() {
      # Destroy all observers
      for (obs in private$.observers) {
        if (!is.null(obs)) {
          tryCatch(obs$destroy(), error = function(e) NULL)
        }
      }
      private$.observers <- list()

      # Clear rendered outputs so stale content doesn't persist
      if (!is.null(private$.output)) {
        tryCatch({
          private$.output$plot <- plotly::renderPlotly({})
          private$.output$table <- DT::renderDataTable(NULL)
        }, error = function(e) NULL)
      }

      # Clear reactive state
      private$.originalData(NULL)
      private$.currentView(NULL)
      private$.renderedData(NULL)
      private$.selectedTerms(character(0))
    },

    #' Check if session has generated data
    #'
    #' @return TRUE if generate() has been called and data exists
    hasData = function() {
      !is.null(private$.originalData())
    },

    #' Get current selection
    #'
    #' @return Character vector of selected term IDs
    getSelectedTerms = function() {
      private$.selectedTerms()
    },

    #' Get the namespaced plot ID for external event routing
    #'
    #' @return Character. The full namespaced plot output ID.
    getPlotId = function() {
      paste(self$id, "plot", sep = "-")
    },

    #' Get original data
    #'
    #' @return Data frame or NULL
    getOriginalData = function() {
      private$.originalData()
    },

    #' Get current view data
    #'
    #' @return Data frame or NULL
    getCurrentView = function() {
      private$.currentView()
    },

    #' Get rendered data
    #'
    #' @return Data frame or NULL
    getRenderedData = function() {
      private$.renderedData()
    },

    # =========================================================================
    # UI Fragment Methods (Composition Pattern)
    # =========================================================================
    # Subclasses compose their ui() by calling these methods.
    # All fragment methods require ns (namespace function) as parameter.

    #' Generate datasource picker UI fragment
    #'
    #' Multi-select picker for choosing datasources to include.
    #' All plots now use multi-select for consistency.
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @return Shiny pickerInput element
    datasourcePickerUI = function(ns) {
      shinyWidgets::pickerInput(
        inputId = ns("sourceSelect"),
        label = "Select term datasource(s):",
        choices = NULL,
        multiple = TRUE,
        options = list('actions-box' = TRUE)
      )
    },

    #' Generate term count slider UI fragment
    #'
    #' Slider for filtering top N terms, with info tooltip.
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @param uiTermKeyword Label term (e.g., "functions", "terms")
    #' @return Shiny sliderInput element with tooltip
    termCountSliderUI = function(ns, uiTermKeyword = "functions") {
      shiny::sliderInput(
        inputId = ns("termCountSlider"),
        label = paste0("Filter number of top ", uiTermKeyword, ":"),
        min = 1, max = 10, value = 10, step = 1
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink("circle-info") %>%
            bsplus::bs_embed_popover(title = "Upper cap of 200 terms.")
        )
    },

    #' Generate sort mode radio buttons UI fragment
    #'
    #' Radio buttons for choosing sort order (-log10Pvalue or Enrichment Score).
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @param uiTermKeyword Label term (e.g., "functions", "terms")
    #' @return Shiny radioButtons element
    sortModeUI = function(ns, uiTermKeyword = "functions") {
      shiny::radioButtons(
        inputId = ns("sortMode"),
        label = paste0("Order retrieved ", uiTermKeyword, " by:"),
        choices = c("-log10Pvalue", "Enrichment Score"),
        inline = TRUE
      )
    },

    #' Generate generate button UI fragment
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @return Shiny actionButton element
    generateButtonUI = function(ns) {
      shiny::actionButton(
        inputId = ns("generateBtn"),
        label = "Generate",
        shiny::icon("palette"),
        class = "submit_button"
      )
    },

    #' Generate reset button UI fragment
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @return Shiny actionButton element
    resetButtonUI = function(ns) {
      shiny::actionButton(
        inputId = ns("resetBtn"),
        label = "Reset View",
        shiny::icon("refresh"),
        class = "reset_button",
        style = "margin-left: 10px;"
      )
    },

    # =========================================================================
    # Shared Server Logic
    # =========================================================================

    #' Register common observers for shared controls
    #'
    #' Call this from subclass server() after setting up moduleServer.
    #' Registers observers for: datasource picker change.
    #'
    #' @param input Shiny input object (from moduleServer)
    #' @param session Shiny session object (from moduleServer)
    registerCommonObservers = function(input, session) {
      # Datasource picker observer - updates term count slider when selection changes
      private$.observers$sourcePickerBase <- shiny::observeEvent(
        input$sourceSelect,
        {
          self$onDatasourceChange(input$sourceSelect, session)
        },
        ignoreInit = TRUE
      )
    },

    #' Handle datasource selection change
    #'
    #' Called when user changes datasource picker selection.
    #' Updates term count slider range. Subclasses can override
    #' to add additional logic (e.g., Network3 updating its threshold slider).
    #'
    #' @param selectedSources Character vector of selected datasources
    #' @param session Shiny session object
    onDatasourceChange = function(selectedSources, session) {
      private$updateTermCountSliderRange(selectedSources, session)
    }
  ),

  private = list(
    # -------------------------------------------------------------------------
    # Plot Configuration Constants (encapsulated, not global)
    # -------------------------------------------------------------------------
    # Subclasses can override these in their private list

    # Minimum plot height in pixels
    .minPlotHeight = 200,

    # Height per entry in pixels (for dynamic plot height calculation)
    # DotPlot overrides to 22
    .entryHeightPx = 18,

    # Maximum slider value for term count (capped in UI tooltip)
    .maxSliderValue = 200,

    # -------------------------------------------------------------------------
    # Module State
    # -------------------------------------------------------------------------

    # Module session (from moduleServer)
    .moduleSession = NULL,

    # Shiny output reference (from moduleServer)
    .output = NULL,

    # Three-tier reactive state
    .originalData = NULL,     # Immutable baseline from Generate
    .currentView = NULL,      # Working data (may be filtered by selection)
    .renderedData = NULL,     # What's actually displayed (after sorting)

    # Selection state
    .selectedTerms = NULL,    # reactiveVal of selected term IDs

    # Update tracking for cascade prevention
    .updateTracker = NULL,
    .programmaticUpdateTime = NULL,

    # Registered observers
    .observers = list(),

    # -------------------------------------------------------------------------
    # State Management (common to all output sessions)
    # -------------------------------------------------------------------------

    #' Set original data and reset all state
    #'
    #' Called when "Generate" button is clicked.
    #' Sets original, current, and rendered to the same data.
    #' Clears selection and resets update tracker.
    setOriginalData = function(data) {
      private$.originalData(data)
      private$.currentView(data)
      private$.renderedData(data)
      private$.selectedTerms(character(0))
      private$.updateTracker$counter <- 0
      private$.updateTracker$source <- "generate"
      private$.updateTracker$time <- Sys.time()
    },

    #' Set rendered data
    #'
    #' Called after sorting/ordering data for display.
    setRenderedData = function(data) {
      private$.renderedData(data)
    },

    #' Record update source for cascade prevention
    setUpdateSource = function(source) {
      private$.updateTracker$counter <- private$.updateTracker$counter + 1
      private$.updateTracker$source <- source
      private$.updateTracker$time <- Sys.time()
    },

    # -------------------------------------------------------------------------
    # Cascade Prevention (common to all output sessions)
    # -------------------------------------------------------------------------

    #' Check if update should be processed
    #'
    #' Prevents circular updates between plot and table.
    #' Uses timing to detect programmatic vs user-initiated changes.
    #'
    #' @param expectedSource Source of the expected update ("table", "plot")
    #' @return TRUE if update should be processed
    shouldProcessUpdate = function(expectedSource) {
      lastSource <- private$.updateTracker$source
      timeSince <- as.numeric(Sys.time() - private$.updateTracker$time)

      # Block table updates caused by selection changes (500ms window)
      if (expectedSource == "table" && lastSource == "selection" && timeSince < 0.5) {
        return(FALSE)
      }

      # Block plot updates from various programmatic sources (500ms window)
      if (expectedSource == "plot" &&
          lastSource %in% c("selection", "reset", "table_filter") &&
          timeSince < 0.5) {
        return(FALSE)
      }

      return(TRUE)
    },

    #' Mark that a programmatic table update is happening
    #'
    #' Used to distinguish programmatic updates from user filtering.
    markProgrammaticUpdate = function() {
      private$.programmaticUpdateTime <- Sys.time()
    },

    #' Check if current update is programmatic
    #'
    #' @return TRUE if within 200ms of a programmatic update
    isProgrammaticUpdate = function() {
      if (is.null(private$.programmaticUpdateTime)) return(FALSE)
      timeSince <- as.numeric(Sys.time() - private$.programmaticUpdateTime)
      return(timeSince < 0.2)
    },

    # -------------------------------------------------------------------------
    # Shared Control Initialization (used by all plots with shared controls)
    # -------------------------------------------------------------------------

    #' Initialize shared controls with data from enrichment results
    #'
    #' Populates datasource picker and sets term count slider range.
    #' Called from subclass initializeControls() via super$initializeControls().
    #'
    #' @param session Shiny session object
    initializeControls = function(session) {
      results <- self$enrichSession$getResults()
      if (is.null(results) || nrow(results) == 0) return()

      # Populate datasource picker with available sources, select all by default
      sources <- unique(as.character(results$Source))
      shinyWidgets::updatePickerInput(
        session,
        "sourceSelect",
        choices = sources,
        selected = sources
      )

      # Set term count slider range based on total results
      maxRows <- nrow(results)
      shiny::updateSliderInput(
        session,
        "termCountSlider",
        max = min(maxRows, private$.maxSliderValue),
        value = min(10, maxRows)
      )
    },

    #' Update term count slider range based on selected datasources
    #'
    #' Called when user changes datasource selection.
    #' Recalculates max based on rows matching selected sources.
    #'
    #' @param selectedSources Character vector of selected datasources
    #' @param session Shiny session object
    updateTermCountSliderRange = function(selectedSources, session) {
      if (is.null(selectedSources) || length(selectedSources) == 0) return()

      results <- self$enrichSession$getResults()
      if (is.null(results)) return()

      # Calculate max rows for selected sources
      maxRows <- nrow(subset(results, Source %in% selectedSources))
      if (maxRows == 0) maxRows <- 1

      # Get current slider value from module input
      currentValue <- shiny::isolate(private$.moduleSession$input$termCountSlider)
      if (is.null(currentValue)) currentValue <- 10

      shiny::updateSliderInput(
        session,
        "termCountSlider",
        max = min(maxRows, private$.maxSliderValue),
        value = min(currentValue, maxRows)
      )
    },

    # -------------------------------------------------------------------------
    # Abstract methods (must be implemented by subclasses)
    # -------------------------------------------------------------------------

    #' Render both plot and table from current view
    #'
    #' Called from handleReset(). Subclasses must implement.
    renderBothFromCurrentView = function() {
      stop("Subclass must implement renderBothFromCurrentView()")
    },

    # -------------------------------------------------------------------------
    # Rendering Utilities (shared by all output sessions)
    # -------------------------------------------------------------------------

    #' Render enrichment table with expandable rows
    #'
    #' Renders a DT table with expandable rows for showing detailed content.
    #' Used by plot OutputSessions for their synchronized tables.
    #'
    #' @param data Data frame to render
    #' @param caption Table caption
    #' @param fileName Base filename for exports
    #' @param mode Label for expandable content (e.g., "Positive Hits")
    #' @param hiddenColumns Column indices to hide (0-indexed after ⊕ column)
    #' @param expandableColumn Column index containing expandable content
    #' @param filter DT filter type ("none", "top", "bottom")
    #' @param exportExcludeColumns Columns to exclude from export
    renderEnrichmentTableInternal = function(data, caption, fileName, mode,
                                              hiddenColumns, expandableColumn,
                                              filter = 'none',
                                              exportExcludeColumns = c(0, 11)) {
      private$.output$table <- DT::renderDataTable({
        tableData <- cbind(' ' = '&oplus;', data)

        dt <- DT::datatable(
          tableData,
          escape = FALSE,
          rownames = FALSE,
          selection = 'none',
          filter = filter,
          extensions = c('Buttons'),
          caption = caption,
          options = list(
            scrollX = TRUE,
            "dom" = 'T<"clear">lBfrtip',
            buttons = createExportButtons(fileName, exportExcludeColumns),
            columnDefs = list(
              list(visible = FALSE, targets = hiddenColumns),
              list(orderable = FALSE, searchable = FALSE,
                   className = 'details-control', targets = 0)
            ),
            initComplete = htmlwidgets::JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('thead tr:eq(1) td:eq(0)').find('input,select').hide();",
              "}"
            )
          ),
          callback = htmlwidgets::JS(paste0(
            "table.column(0).nodes().to$().css({cursor: 'pointer'});
            let format = function(d) {
              return '<div style=\"background-color:#eee; padding: .5em;\"> <b>", mode, ":</b> ' +
                      d[", expandableColumn, "] + '</div>';
            };
            table.on('click', 'td.details-control', function() {
              let td = $(this), row = table.row(td.closest('tr'));
              if (row.child.isShown()) {
                row.child.hide();
                td.html('&oplus;');
              } else {
                row.child(format(row.data())).show();
                td.html('&CircleMinus;');
              }
            });"
          ))
        )

        # Format P-value column to show 3 significant figures
        dt <- DT::formatSignif(dt, columns = 'P-value', digits = 3)
        dt
      }, server = FALSE)
    },

    #' Filter top N results by datasources and ordering mode
    #'
    #' Filters enrichment results to top N entries per datasource,
    #' ordered by -log10Pvalue or Enrichment Score.
    #'
    #' @param results Full enrichment results data frame
    #' @param sources Character vector of datasource names to include
    #' @param slider Integer. Number of top results to return
    #' @param mode Character. Ordering mode: "-log10Pvalue" or "Enrichment Score"
    #' @return Filtered data frame
    filterTopData = function(results, sources, slider, mode) {
      if (is.null(results) || nrow(results) == 0) return(NULL)

      # Filter to selected sources
      filtered <- results[results$Source %in% sources, ]
      if (nrow(filtered) == 0) return(NULL)

      # Order by selected mode (descending)
      orderCol <- if (mode == "-log10Pvalue") "-log10Pvalue" else "Enrichment Score"
      if (orderCol %in% names(filtered)) {
        filtered <- filtered[order(-filtered[[orderCol]]), ]
      }

      # Take top N
      if (slider < nrow(filtered)) {
        filtered <- filtered[1:slider, ]
      }

      filtered
    }
  )
)
