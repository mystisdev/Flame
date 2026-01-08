# FLAME Server Function

function(input, output, session) {
  # Determine package root and source required files
  pkgRoot <- normalizePath(file.path(getwd(), "..", ".."))
  source(file.path(pkgRoot, "R", "aaa-helpers.R"), local = TRUE)

  # Source R6 infrastructure classes
  source(file.path(pkgRoot, "R", "infrastructure-config.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-analytelist.R"), local = TRUE)

  # Source utility services (used by input sessions)
  source(file.path(pkgRoot, "R", "utilities-extract.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "utilities-gsnpense.R"), local = TRUE)

  # Source input session classes
  source(file.path(pkgRoot, "R", "input-session-base.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-analytelist-manipulation.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-list.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-volcano.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-reduction.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-snps.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-textmining.R"), local = TRUE)

  # Source configuration files (letter prefixes ensure correct load order)
  source(file.path(pkgRoot, "R", "config-a-global_settings.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-b-global_variables.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-c-enrichment_types.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-d-server_variables.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-e-static_variables.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-f-ui_variables.R"), local = TRUE)

  # Source core functions
  source(file.path(pkgRoot, "R", "func-general.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-init.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-render.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-update.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-reset.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-runs.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-run.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "core-tool_registry.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-observers.R"), local = TRUE)

  # Source input functions
  # Note: input-main.R replaced by ListInputSession
  #       input-volcano.R replaced by VolcanoInputSession
  #       input-reduction.R replaced by ReductionInputSession
  #       input-snps.R replaced by SNPsInputSession
  #       input-text_mining.R replaced by TextMiningInputSession
  #       input-upset.R replaced by AnalyteListSetOperationsSession
  source(file.path(pkgRoot, "R", "input-conversion.R"), local = TRUE)

  # Source enrichment functions
  source(file.path(pkgRoot, "R", "enrich-inputs_panel.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-main.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-general.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-gprofiler.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-webgestalt.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-enrichr.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-string.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-panther.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-genecodis.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-combination.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-links.R"), local = TRUE)

  # Source plot functions
  source(file.path(pkgRoot, "R", "plot-general.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-networks.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-heatmaps.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-barchart.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-scatter.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-dotplot.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-manhattan.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "plot-arena3d.R"), local = TRUE)

  # Source remaining functions
  source(file.path(pkgRoot, "R", "func-stringNetwork.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-conversion.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-tabGeneration.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-registry.R"), local = TRUE)

  # Output Registry for cleanup management (per-session)
  outputRegistry <- OutputRegistry$new()

  # Observer Registry for cleanup management (per-session)
  observerRegistry <- ObserverRegistry$new()

  # AnalyteList Registry for managing input lists (per-session)
  # Must be created in reactive context (server function)
  analyteListRegistry <- AnalyteListRegistry$new()

  # AnalyteList Manager Session - manages sidebar and view panel for ALL lists
  analyteListManager <- AnalyteListManagerSession$new(
    ModuleIds$INPUT_MANIPULATION_MANAGER, analyteListRegistry
  )
  analyteListManager$server(input, session)

  # List Input Session - manages Upload tab only (creating lists from text/files)
  listInputSession <- ListInputSession$new(ModuleIds$INPUT_LIST, analyteListRegistry)
  listInputSession$server(input, session)

  # Volcano Input Session - manages Volcano tab and plot panel
  volcanoInputSession <- VolcanoInputSession$new(ModuleIds$INPUT_VOLCANO, analyteListRegistry)
  volcanoInputSession$server(input, session)

  # Reduction Input Session - manages 2D Reduction tab and plot panel
  reductionInputSession <- ReductionInputSession$new(ModuleIds$INPUT_REDUCTION, analyteListRegistry)
  reductionInputSession$server(input, session)

  # SNPs Input Session - manages SNPs tab for SNP to gene conversion
  snpsInputSession <- SNPsInputSession$new(ModuleIds$INPUT_SNPS, analyteListRegistry)
  snpsInputSession$server(input, session)

  # Text Mining Input Session - manages Text-mining tab for extracting genes from text
  textMiningInputSession <- TextMiningInputSession$new(ModuleIds$INPUT_TEXTMINING, analyteListRegistry)
  textMiningInputSession$server(input, session)

  # AnalyteList Operations Session - manages UpSet plot panel for set operations
  upsetSession <- AnalyteListSetOperationsSession$new(
    ModuleIds$INPUT_MANIPULATION_SETOPERATIONS,
    analyteListRegistry,
    analyteListManager
  )
  upsetSession$server(input, session)

  # Clean up all session objects when the Shiny session ends
  # Order: dependent sessions first, then sessions they depend on
  session$onSessionEnded(function() {
    # upsetSession depends on analyteListManager, so clean it up first
    upsetSession$cleanup()
    listInputSession$cleanup()
    volcanoInputSession$cleanup()
    reductionInputSession$cleanup()
    snpsInputSession$cleanup()
    textMiningInputSession$cleanup()
    analyteListManager$cleanup()
  })

  # Cross-module reactive updates: update selectors when registry changes

  # Helper to update selectInput while preserving current selection if still valid
  updateSelectPreserving <- function(inputId, choices) {
    currentSelection <- input[[inputId]]
    selected <- if (!is.null(currentSelection) && currentSelection %in% choices) {
      currentSelection
    } else {
      NULL
    }
    updateSelectInput(session, inputId, choices = choices, selected = selected)
  }

  # This replaces the imperative updateListBoxes() function with reactive updates
  observe({
    # Get list names reactively - this will re-run when registry changes
    listNames <- analyteListRegistry$getNamesReactive()

    # BACKWARD COMPATIBILITY: Sync registry to userInputLists for code that
    # hasn't been migrated yet (enrich-main.R, etc.)
    # This will be removed once all code is migrated to use the registry directly.
    userInputLists <<- lapply(analyteListRegistry$getAll(), function(analyteList) {
      analyteList$toDataFrame()
    })

    # Update enrichment selectors (preserving current selection if still valid)
    updateSelectPreserving("functional_enrichment_file", listNames)
    updateSelectPreserving("functional_enrichment_background_list", listNames)
    updateSelectPreserving("literature_enrichment_file", listNames)
    updateSelectPreserving("literature_enrichment_background_list", listNames)

    # Update utility selectors (preserving current selection if still valid)
    updateSelectPreserving("selectUpset", listNames)
    updateSelectPreserving("gconvert_select", listNames)
    updateSelectPreserving("gorth_select", listNames)
    updateSelectPreserving("literatureSelect", listNames)
    updateSelectPreserving("STRINGnetworkSelect", listNames)

    # Update background list choices (exclude current input list)
    updateBackgroundListChoices("functional")
    updateBackgroundListChoices("literature")

    # UpSet tab visibility is now managed by AnalyteListSetOperationsSession
  })

  # Initialize server app
  initializeServerApp()

  # Welcome page observers
  observeEvent(input$link_to_fileinput, {
    updateTabItems(session, "sideBarId", selected = "file_handler")
  }, ignoreInit = TRUE)

  # INPUT observers for list management are now handled by ListInputSession
  # (see listInputSession$server() call above)

  # Text-mining observers are now handled by TextMiningInputSession
  # (see textMiningInputSession$server() call above)

  # UpSet observers are now handled by AnalyteListSetOperationsSession
  # (see upsetSession$server() call above)

  # SNPs observers are now handled by SNPsInputSession
  # (see snpsInputSession$server() call above)

  # Volcano observers are now handled by VolcanoInputSession
  # (see volcanoInputSession$server() call above)

  # 2D Reduction observers are now handled by ReductionInputSession
  # (see reductionInputSession$server() call above)

  # ENRICHMENT observers
  observeEvent(input$functional_enrichment_organism, {
    handleFunctionalEnrichmentOrganismSelection()
  }, ignoreInit = TRUE)

  observeEvent(input$functional_enrichment_tool, {
    handleFunctionalEnrichmentToolSelection()
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_file")]], {
      handleBackgroundListUpdate(enrichmentType)
    }, ignoreInit = TRUE)
  })

  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_background_choice")]], {
      choice <- input[[paste0(enrichmentType, "_enrichment_background_choice")]]
      handleBackgroundModeSelection(choice, enrichmentType)
    }, ignoreInit = TRUE)
  })

  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_run")]], {
      handleBackgroundListUpdate(input[[paste0(enrichmentType, "_enrichment_file")]])
      handleEnrichment(enrichmentType)
    }, ignoreInit = TRUE)
  })

  observeEvent(input$functional_enrichment_all_clear, {
    handleMultiClear()
  }, ignoreInit = TRUE)

  observeEvent(input$literature_enrichment_all_clear, {
    handleLiteratureMultiClear()
  }, ignoreInit = TRUE)

  # Close individual run tab (via X button)
  observeEvent(input$closeRunTab, {
    runId <- input$closeRunTab
    fullRunKey <- paste("functional", runId, sep = "_")
    toolName <- parseFullRunKey(fullRunKey)$toolName
    clearRunCompletely(fullRunKey)
    if (countActiveRunsForTool(toolName) == 0) {
      resetRunCounterForTool(toolName)
    }
    if (getActiveFunctionalRunCount() == 0) {
      shinyjs::hide("functionalEnrichmentResultsPanel")
      shinyjs::hide("functional_enrichment_all_clear")
    }
    prepareCombinationTab()
  }, ignoreInit = TRUE)

  # Close individual literature run tab
  observeEvent(input$closeLiteratureRunTab, {
    runId <- input$closeLiteratureRunTab
    fullRunKey <- paste("literature", runId, sep = "_")
    toolName <- parseFullRunKey(fullRunKey)$toolName
    clearLiteratureRunCompletely(fullRunKey)
    if (countActiveRunsForTool(toolName) == 0) {
      resetRunCounterForTool(toolName)
    }
    if (getActiveLiteratureRunCount() == 0) {
      shinyjs::hide("literatureEnrichmentResultsPanel")
      shinyjs::hide("literature_enrichment_all_clear")
    }
  }, ignoreInit = TRUE)

  # Combination observers
  observeEvent(input$combo_datasources, {
    handleComboSourceSelect()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$upsetjsCombo_click, {
    handleComboUpsetClick()
  }, ignoreInit = TRUE)

  observeEvent(input$combo_visNetwork_run, {
    handleComboNetwork()
  }, ignoreInit = TRUE)

  # Manhattan plot observers
  observeEvent(event_data("plotly_click", source = "A"), {
    triggeredEvent <- event_data("plotly_click", source = "A")
    if (isEventFromManhattan(triggeredEvent))
      handleManhattanClick(triggeredEvent$key)
  }, ignoreInit = TRUE)

  observeEvent(event_data("plotly_selected", source = "A"), {
    triggeredEvent <- event_data("plotly_selected", source = "A")
    if (isEventFromManhattan(triggeredEvent))
      handleManhattanSelect(triggeredEvent$key)
  }, ignoreInit = TRUE)

  # Plot click observers
  observeEvent(event_data("plotly_click", source = "Barchart", priority = "event"), {
    handlePlotClick("barchart", "Barchart", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "Scatter", priority = "event"), {
    handlePlotClick("scatterPlot", "Scatter", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "DotPlot", priority = "event"), {
    handlePlotClick("dotPlot", "DotPlot", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Zoom/relayout observers
  observeEvent(event_data("plotly_relayout", source = "Barchart"), {
    handlePlotZoom("barchart", "Barchart")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Scatter"), {
    handlePlotZoom("scatterPlot", "Scatter")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "DotPlot"), {
    handlePlotZoom("dotPlot", "DotPlot")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Selection observers
  observeEvent(event_data("plotly_selected", source = "Barchart", priority = "event"), {
    handlePlotSelection("barchart", "Barchart", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Scatter", priority = "event"), {
    handlePlotSelection("scatterPlot", "Scatter", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "DotPlot", priority = "event"), {
    handlePlotSelection("dotPlot", "DotPlot", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Heatmap observers
  observeEvent(event_data("plotly_click", source = "Heatmap1", priority = "event"), {
    handlePlotClick("heatmap1", "Heatmap1", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Heatmap1"), {
    handlePlotZoom("heatmap1", "Heatmap1")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Heatmap1"), {
    handlePlotSelection("heatmap1", "Heatmap1", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "Heatmap2", priority = "event"), {
    handlePlotClick("heatmap2", "Heatmap2", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Heatmap2"), {
    handlePlotZoom("heatmap2", "Heatmap2")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Heatmap2"), {
    handlePlotSelection("heatmap2", "Heatmap2", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "Heatmap3", priority = "event"), {
    handlePlotClick("heatmap3", "Heatmap3", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Heatmap3"), {
    handlePlotZoom("heatmap3", "Heatmap3")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Heatmap3"), {
    handlePlotSelection("heatmap3", "Heatmap3", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # STRING observers
  observeEvent(input$string_network_organism, {
    handleStringOrganismSelection()
  }, ignoreInit = TRUE)

  observeEvent(input$runStringNetwork, {
    handleStringNetwork()
  }, ignoreInit = TRUE)

  observeEvent(input$string_submit_functional, {
    handleStringSubmitForEnrichment("functional")
  }, ignoreInit = TRUE)

  observeEvent(input$string_submit_literature, {
    handleStringSubmitForEnrichment("literature")
  }, ignoreInit = TRUE)

  # CONVERSION observers
  observeEvent(input$gconvert_button, {
    handle_gconvert()
  }, ignoreInit = TRUE)

  observeEvent(input$gorth_organism, {
    handle_gorthOrganism()
  }, ignoreInit = TRUE)

  observeEvent(input$gorth_button, {
    handle_gorth()
  }, ignoreInit = TRUE)

  observeEvent(input$gconvert_addList, {
    dType <- input$gconvert_dType
    addConversionResultToInput("gconvert", dType)
  }, ignoreInit = TRUE)

  observeEvent(input$gorth_addList, {
    dType <- input$gorth_dType
    addConversionResultToInput("gorth", dType)
  }, ignoreInit = TRUE)
}
