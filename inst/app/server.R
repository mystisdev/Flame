# FLAME Server Function

function(input, output, session) {
  # Determine package root and source required files
  pkgRoot <- normalizePath(file.path(getwd(), "..", ".."))
  source(file.path(pkgRoot, "R", "aaa-helpers.R"), local = TRUE)

  # Source R6 infrastructure classes
  source(file.path(pkgRoot, "R", "infrastructure-config.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-analyteset.R"), local = TRUE)

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
  source(file.path(pkgRoot, "R", "input-main.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-upset.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-text_mining.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-snps.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-volcano.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-reduction.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-api.R"), local = TRUE)
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

  # API handling
  observeEvent(session$clientData$url_search, {
    resolveAPI()
  })

  # Initialize server app
  initializeServerApp()

  # Welcome page observers
  observeEvent(input$link_to_fileinput, {
    updateTabItems(session, "sideBarId", selected = "file_handler")
  }, ignoreInit = TRUE)

  # INPUT observers
  observeEvent(input$example, {
    handleRandomExample()
  }, ignoreInit = TRUE)

  observeEvent(input$input_clear, {
    handleClearText()
  }, ignoreInit = TRUE)

  observeEvent(input$text_submit, {
    handleTextSubmit()
  }, ignoreInit = TRUE)

  observeEvent(input$fileUpload, {
    handleInputFiles()
  }, ignoreInit = TRUE)

  observeEvent(input$selectAll, {
    handleSelectAllLists()
  }, ignoreInit = TRUE)

  observeEvent(input$rename, {
    handlePrepareRenameLists()
  }, ignoreInit = TRUE)

  observeEvent(input$js_listNames, {
    handleRenameLists()
  }, ignoreInit = TRUE)

  observeEvent(input$remove, {
    handleRemoveLists()
  }, ignoreInit = TRUE)

  observeEvent(input$selectView, {
    handleSelectView()
  }, ignoreInit = TRUE)

  # Text-mining observers
  observeEvent(input$textmining_addExample, {
    loadTextMiningExample()
  }, ignoreInit = TRUE)

  observeEvent(input$textmining_clear, {
    resetTextMiningFields()
  }, ignoreInit = TRUE)

  observeEvent(input$textmining_submit, {
    handleTextMining()
  }, ignoreInit = TRUE)

  observeEvent(input$textmining_selectAll, {
    shinyjs::runjs("textmining_selectAll(true);")
  }, ignoreInit = TRUE)

  observeEvent(input$textmining_selectNone, {
    shinyjs::runjs("textmining_selectAll(false);")
  }, ignoreInit = TRUE)

  observeEvent(input$textmining_addList, {
    addTextMiningToFiles()
  }, ignoreInit = TRUE)

  observeEvent(input$textmining_delete, {
    deleteTextmining()
  }, ignoreInit = TRUE)

  # Upset observers
  observeEvent(input$submitUpset, {
    handleUpset()
  }, ignoreInit = TRUE)

  observeEvent(input$upsetjsView_hover, {
    handleUpsetHover()
  }, ignoreInit = TRUE)

  observeEvent(input$upsetjsView_click, {
    handleUpsetClick()
  }, ignoreInit = TRUE)

  observeEvent(input$upsetClick_ok, {
    handleUpsetListAccept()
  }, ignoreInit = TRUE)

  # SNPs observers
  observeEvent(input$snp_example, {
    loadVariantExample()
  }, ignoreInit = TRUE)

  observeEvent(input$snp_clear, {
    resetVariantFields()
  }, ignoreInit = TRUE)

  observeEvent(input$snp_submit, {
    handleVariantSubmit()
  }, ignoreInit = TRUE)

  observeEvent(input$snp_fileUpload, {
    handleVariantUpload()
  }, ignoreInit = TRUE)

  observeEvent(input$snp_addList, {
    addVariantsToFiles()
  }, ignoreInit = TRUE)

  observeEvent(input$snp_delete, {
    deleteVariants()
  }, ignoreInit = TRUE)

  # Volcano observers
  observeEvent(input$volcanoUpload, {
    handleVolcanoPlot(readVolcanoInput)
  }, ignoreInit = TRUE)

  observeEvent(input$volcano_addExample, {
    handleVolcanoPlot(readVolcanoExample)
  }, ignoreInit = TRUE)

  observeEvent(c(input$volcano_pvalue_slider, input$volcano_fc_slider), {
    updateVolcanoMetricsConversionText(input$volcano_pvalue_slider,
                                       input$volcano_fc_slider)
  }, ignoreInit = TRUE)

  observeEvent(event_data("plotly_selected", source = "Volcano"), {
    triggeredEvent <- event_data("plotly_selected", source = "Volcano")
    volcanoSelectedItems <<- triggeredEvent$customdata
    renderShinyText("volcanoSelected",
                    paste(volcanoSelectedItems, collapse = ", "))
  }, ignoreInit = TRUE)

  observeEvent(input$volcano_submit, {
    handleVolcanoSubmit()
  }, ignoreInit = TRUE)

  observeEvent(input$volcano_ok, {
    handleVolcanoListAccept()
  }, ignoreInit = TRUE)

  observeEvent(input$volcanoGenerate, {
    handleVolcanoGenerate()
  }, ignoreInit = TRUE)

  observeEvent(input$volcanoClear, {
    handleVolcanoClear()
  }, ignoreInit = TRUE)

  observeEvent(c(
    input$volcano_gene_col,
    input$volcano_logfc_col,
    input$volcano_pvalue_col
  ), {
    updateVolcanoDropdownChoices()
  }, ignoreInit = TRUE)

  # 2D Reduction observers
  observeEvent(input$reductionUpload, {
    handleReductionInput(readReductionInput)
  }, ignoreInit = TRUE)

  observeEvent(input$reduction_addExample, {
    handleReductionInput(readReductionExample)
  }, ignoreInit = TRUE)

  observeEvent(input$reductionGenerate, {
    handleReductionGenerate()
  }, ignoreInit = TRUE)

  observeEvent(input$reductionClear, {
    handleReductionClear()
  }, ignoreInit = TRUE)

  observeEvent(event_data("plotly_selected", source = "ReductionPlot"), {
    triggeredEvent <- event_data("plotly_selected", source = "ReductionPlot")
    reductionSelectedItems <<- triggeredEvent$customdata
    renderShinyText("reductionSelected",
                    paste(reductionSelectedItems, collapse = ", "))
  }, ignoreInit = TRUE)

  observeEvent(input$reduction_submit, {
    handleReductionSubmit()
  }, ignoreInit = TRUE)

  observeEvent(input$reduction_ok, {
    handleReductionListAccept()
  }, ignoreInit = TRUE)

  observeEvent(c(
    input$reduction_gene_col,
    input$reduction_x_axis,
    input$reduction_y_axis,
    input$reduction_color,
    input$reduction_size
  ), {
    updateReductionDropdownChoices()
  }, ignoreInit = TRUE)

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

  # Plot-Table Synchronization
  currentSelectedToolTab <<- NULL

  observeEvent(input$toolTabsPanel, {
    currentSelectedToolTab <<- input$toolTabsPanel
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  currentSelectedLiteratureTab <<- NULL

  observeEvent(input$literatureToolTabsPanel, {
    currentSelectedLiteratureTab <<- input$literatureToolTabsPanel
    currentSelectedToolTab <<- input$literatureToolTabsPanel
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

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
