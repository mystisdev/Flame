function(input, output, session) {
  source("config/global_settings.R", local = T)
  source("config/global_variables.R", local = T)
  source("config/enrichment_types.R", local = T)  # Must load after global_variables.R (depends on ENRICHMENT_DATASOURCES)
  source("config/server_variables.R", local = T)
  source("config/static_variables.R", local = T)
  source("config/ui_variables.R", local = T)
  
  source("functions/general.R", local = T)
  source("functions/init.R", local = T)
  source("functions/render.R", local = T)
  source("functions/update.R", local = T)
  source("functions/reset.R", local = T)
  source("functions/runs.R", local = T)
  source("functions/run.R", local = T)  # R6 Run classes (depends on runs.R helpers)
  source("functions/tool_registry.R", local = T)  # ToolRegistry must load BEFORE enrichment files that register strategies
  source("functions/observers.R", local = T)

  source("functions/input/main.R", local = T)
  source("functions/input/upset.R", local = T)
  source("functions/input/text_mining.R", local = T)
  source("functions/input/snps.R", local = T)
  source("functions/input/volcano.R", local = T)
  source("functions/input/reduction.R", local = T)
  source("functions/input/api.R", local = T)
  source("functions/input/conversion.R", local = T)
  
  source("functions/enrichment/inputs_panel.R", local = T)
  source("functions/enrichment/main.R", local = T)
  source("functions/enrichment/general.R", local = T)
  source("functions/enrichment/gprofiler.R", local = T)
  source("functions/enrichment/webgestalt.R", local = T)
  source("functions/enrichment/enrichr.R", local = T)
  source("functions/enrichment/string.R", local = T)
  source("functions/enrichment/panther.R", local = T)
  source("functions/enrichment/genecodis.R", local = T)
  source("functions/enrichment/combination.R", local = T)
  source("functions/links.R", local = T)
  
  source("functions/plots/general.R", local = T)
  source("functions/plots/networks.R", local = T)
  source("functions/plots/heatmaps.R", local = T)
  source("functions/plots/barchart.R", local = T)
  source("functions/plots/scatter.R", local = T)
  source("functions/plots/dotplot.R", local = T)
  source("functions/plots/manhattan.R", local = T)
  source("functions/plots/arena3d.R", local = T)

  source("functions/stringNetwork.R", local = T)
  source("functions/conversion.R", local = T)
  source("functions/tabGeneration.R", local = T)
  source("functions/registry.R", local = T)

  # Output Registry for cleanup management. Instantiate per-session
  outputRegistry <- OutputRegistry$new()

  # Observer Registry for cleanup management. Instantiate per-session
  observerRegistry <- ObserverRegistry$new()

  # API ####
  observeEvent(session$clientData$url_search, {
    resolveAPI()
  })
  
  # START ####
  initializeServerApp()

  # ~Welcome ####
  observeEvent(input$link_to_fileinput, {
    updateTabItems(session, "sideBarId", selected = "file_handler")
  }, ignoreInit = T)
  
  # INPUT ####
  observeEvent(input$example, {
    handleRandomExample()
  }, ignoreInit = T)
  
  observeEvent(input$input_clear, {
    handleClearText()
  }, ignoreInit = T)
  
  observeEvent(input$text_submit, {
    handleTextSubmit()
  }, ignoreInit = T)
  
  observeEvent(input$fileUpload, {
    handleInputFiles()
  }, ignoreInit = T)

  observeEvent(input$selectAll, {
    handleSelectAllLists()
  }, ignoreInit = T)
  
  observeEvent(input$rename, {
    handlePrepareRenameLists()
  }, ignoreInit = T)
  
  observeEvent(input$js_listNames, {
    handleRenameLists()
  }, ignoreInit = T)
  
  observeEvent(input$remove, {
    handleRemoveLists()
  }, ignoreInit = T)
  
  observeEvent(input$selectView, {
    handleSelectView()
  }, ignoreInit = T)
  
  # ~Text-mining ####
  observeEvent(input$textmining_addExample, {
    loadTextMiningExample()
  }, ignoreInit = T)

  observeEvent(input$textmining_clear, {
    resetTextMiningFields()
  }, ignoreInit = T)
  
  observeEvent(input$textmining_submit, {
    handleTextMining()
  }, ignoreInit = T)

  observeEvent(input$textmining_selectAll, {
    shinyjs::runjs("textmining_selectAll(true);")
    
  }, ignoreInit = T)  
    
  observeEvent(input$textmining_selectNone, {
    shinyjs::runjs("textmining_selectAll(false);")
    
  }, ignoreInit = T)  
  
  observeEvent(input$textmining_addList, {
    addTextMiningToFiles()
  }, ignoreInit = T)  
  
  observeEvent(input$textmining_delete, {
    deleteTextmining()
  }, ignoreInit = T)
  
  # ~Upset ####
  observeEvent(input$submitUpset, {
    handleUpset()
  }, ignoreInit = T)
  
  observeEvent(input$upsetjsView_hover, {
    handleUpsetHover()
  }, ignoreInit = T)
  
  observeEvent(input$upsetjsView_click, {
    handleUpsetClick()
  }, ignoreInit = T)
  
  observeEvent(input$upsetClick_ok, {
    handleUpsetListAccept()
  }, ignoreInit = T)

  # ~SNPs ####
  observeEvent(input$snp_example, {
    loadVariantExample()
  }, ignoreInit = T)
  
  observeEvent(input$snp_clear, {
    resetVariantFields()
  }, ignoreInit = T)
  
  observeEvent(input$snp_submit, {
    handleVariantSubmit()
  }, ignoreInit = T)
  
  observeEvent(input$snp_fileUpload, {
    handleVariantUpload()
  }, ignoreInit = T)
  
  observeEvent(input$snp_addList, {
    addVariantsToFiles()
  }, ignoreInit = T)  
  
  observeEvent(input$snp_delete, {
    deleteVariants()
  }, ignoreInit = T)
  
  # ~Volcano ####
  observeEvent(input$volcanoUpload, {
    handleVolcanoPlot(readVolcanoInput)
  }, ignoreInit = T)

  observeEvent(input$volcano_addExample, {
    handleVolcanoPlot(readVolcanoExample)
  }, ignoreInit = T)
  
  observeEvent(c(input$volcano_pvalue_slider, input$volcano_fc_slider), {
    updateVolcanoMetricsConversionText(input$volcano_pvalue_slider,
                                       input$volcano_fc_slider)
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_selected", source = "Volcano"), {
    triggeredEvent <- event_data("plotly_selected", source = "Volcano")
    volcanoSelectedItems <<- triggeredEvent$customdata
    renderShinyText("volcanoSelected",
                    paste(volcanoSelectedItems, collapse = ", "))
  }, ignoreInit = T)

  observeEvent(input$volcano_submit, {
    handleVolcanoSubmit()
  }, ignoreInit = T)

  observeEvent(input$volcano_ok, {
    handleVolcanoListAccept()
  }, ignoreInit = T)

  observeEvent(input$volcanoGenerate, {
    handleVolcanoGenerate()
  }, ignoreInit = T)

  observeEvent(input$volcanoClear, {
    handleVolcanoClear()
  }, ignoreInit = T)

  # Update dropdown choices when any dropdown changes
  observeEvent(c(
    input$volcano_gene_col,
    input$volcano_logfc_col,
    input$volcano_pvalue_col
  ), {
    updateVolcanoDropdownChoices()
  }, ignoreInit = T)

  # ~2D Reduction ####
  observeEvent(input$reductionUpload, {
    handleReductionInput(readReductionInput)
  }, ignoreInit = T)

  observeEvent(input$reduction_addExample, {
    handleReductionInput(readReductionExample)
  }, ignoreInit = T)

  observeEvent(input$reductionGenerate, {
    handleReductionGenerate()
  }, ignoreInit = T)

  observeEvent(input$reductionClear, {
    handleReductionClear()
  }, ignoreInit = T)

  observeEvent(event_data("plotly_selected", source = "ReductionPlot"), {
    triggeredEvent <- event_data("plotly_selected", source = "ReductionPlot")
    reductionSelectedItems <<- triggeredEvent$customdata
    renderShinyText("reductionSelected",
                    paste(reductionSelectedItems, collapse = ", "))
  }, ignoreInit = T)

  observeEvent(input$reduction_submit, {
    handleReductionSubmit()
  }, ignoreInit = T)

  observeEvent(input$reduction_ok, {
    handleReductionListAccept()
  }, ignoreInit = T)

  # Update dropdown choices when any dropdown changes
  observeEvent(c(
    input$reduction_gene_col,
    input$reduction_x_axis,
    input$reduction_y_axis,
    input$reduction_color,
    input$reduction_size
  ), {
    updateReductionDropdownChoices()
  }, ignoreInit = T)

  # ENRICHMENT ####
  observeEvent(input$functional_enrichment_organism, {
    handleFunctionalEnrichmentOrganismSelection()
  }, ignoreInit = T)
  
  observeEvent(input$functional_enrichment_tool, {
    handleFunctionalEnrichmentToolSelection()
  }, ignoreInit = T, ignoreNULL = F)

  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_file")]], {
      handleBackgroundListUpdate(enrichmentType)
    }, ignoreInit = T)
  })
  
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_background_choice")]], {
      choice <- input[[paste0(enrichmentType, "_enrichment_background_choice")]]
      handleBackgroundModeSelection(choice, enrichmentType)
    }, ignoreInit = T)
  })
    
  lapply(ENRICHMENT_TYPES, function(enrichmentType) {
    observeEvent(input[[paste0(enrichmentType, "_enrichment_run")]], {
      handleBackgroundListUpdate(input[[paste0(enrichmentType, "_enrichment_file")]])
      handleEnrichment(enrichmentType)
    }, ignoreInit = T)
  })

  observeEvent(input$functional_enrichment_all_clear, {
    handleMultiClear()
  }, ignoreInit = T)

  observeEvent(input$literature_enrichment_all_clear, {
    handleLiteratureMultiClear()
  }, ignoreInit = T)

  # Close individual run tab (via X button)
  observeEvent(input$closeRunTab, {
    runId <- input$closeRunTab
    fullRunKey <- paste("functional", runId, sep = "_")

    # Remember the tool name before we clear (since clearRunCompletely removes it from activeRuns)
    toolName <- parseFullRunKey(fullRunKey)$toolName

    # Clear all data and remove tab
    clearRunCompletely(fullRunKey)

    # If no tabs remain for this tool, reset its DISPLAY counter
    # (uniqueIdCounters never reset, so IDs stay unique - no caching bugs)
    if (countActiveRunsForTool(toolName) == 0) {
      resetRunCounterForTool(toolName)
    }

    # Update combination tab and results panel visibility
    if (getActiveFunctionalRunCount() == 0) {
      shinyjs::hide("functionalEnrichmentResultsPanel")
      shinyjs::hide("functional_enrichment_all_clear")
    }
    prepareCombinationTab()
  }, ignoreInit = TRUE)

  # Close individual literature run tab (via X button)
  observeEvent(input$closeLiteratureRunTab, {
    runId <- input$closeLiteratureRunTab
    fullRunKey <- paste("literature", runId, sep = "_")

    # Remember the tool name before we clear
    toolName <- parseFullRunKey(fullRunKey)$toolName

    # Clear all data and remove tab
    clearLiteratureRunCompletely(fullRunKey)

    # If no tabs remain for this tool, reset its DISPLAY counter
    if (countActiveRunsForTool(toolName) == 0) {
      resetRunCounterForTool(toolName)
    }

    # Update results panel visibility
    if (getActiveLiteratureRunCount() == 0) {
      shinyjs::hide("literatureEnrichmentResultsPanel")
      shinyjs::hide("literature_enrichment_all_clear")
    }
  }, ignoreInit = TRUE)

  # ~Combination ####
  observeEvent(input$combo_datasources, {
      handleComboSourceSelect()
    }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$upsetjsCombo_click, {
    handleComboUpsetClick()
  }, ignoreInit = T)
  
  observeEvent(input$combo_visNetwork_run, {
    handleComboNetwork()
  }, ignoreInit = T)
  
  # ~Plots ####

  # ~~Manhattan ####
  
  observeEvent(event_data("plotly_click", source = "A"), { # "A" = "Manhattan"
    triggeredEvent <- event_data("plotly_click", source = "A")
    if (isEventFromManhattan(triggeredEvent))
      handleManhattanClick(triggeredEvent$key)
  }, ignoreInit = T)
  
  observeEvent(event_data("plotly_selected", source = "A"), {
    triggeredEvent <- event_data("plotly_selected", source = "A")
    if (isEventFromManhattan(triggeredEvent))
      handleManhattanSelect(triggeredEvent$key)
  }, ignoreInit = T)

  # ~~Plot-Table Synchronization ####
  # Tracks which tool tab the user is currently viewing
  # For functional multi-run: runId like "gProfiler_1"
  # For literature: tool name like "STRING"
  currentSelectedToolTab <<- NULL

  observeEvent(input$toolTabsPanel, {
    currentSelectedToolTab <<- input$toolTabsPanel
    # Tab selection tracking - plot control updates are now handled
    # immediately via session$onFlushed() in handleEnrichmentRun()
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  # Literature multi-run tab selection observer
  currentSelectedLiteratureTab <<- NULL

  observeEvent(input$literatureToolTabsPanel, {
    currentSelectedLiteratureTab <<- input$literatureToolTabsPanel
    currentSelectedToolTab <<- input$literatureToolTabsPanel
    # Tab selection tracking - plot control updates are now handled
    # immediately via session$onFlushed() in handleEnrichmentRun()
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  # Plot click observers (simple plots)
  # priority = "event" fires on every click, even if same data
  observeEvent(event_data("plotly_click", source = "Barchart", priority = "event"), {
    handlePlotClick("barchart", "Barchart", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "Scatter", priority = "event"), {
    handlePlotClick("scatterPlot", "Scatter", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_click", source = "DotPlot", priority = "event"), {
    handlePlotClick("dotPlot", "DotPlot", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Zoom/relayout observers (simple plots)
  observeEvent(event_data("plotly_relayout", source = "Barchart"), {
    handlePlotZoom("barchart", "Barchart")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Scatter"), {
    handlePlotZoom("scatterPlot", "Scatter")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "DotPlot"), {
    handlePlotZoom("dotPlot", "DotPlot")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Selection (lasso/box) observers
  # priority = "event" fires on every selection, even if same data
  observeEvent(event_data("plotly_selected", source = "Barchart", priority = "event"), {
    handlePlotSelection("barchart", "Barchart", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Scatter", priority = "event"), {
    handlePlotSelection("scatterPlot", "Scatter", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "DotPlot", priority = "event"), {
    handlePlotSelection("dotPlot", "DotPlot", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Heatmap plot event observers
  # Heatmap1 - click, zoom, selection
  observeEvent(event_data("plotly_click", source = "Heatmap1", priority = "event"), {
    handlePlotClick("heatmap1", "Heatmap1", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Heatmap1"), {
    handlePlotZoom("heatmap1", "Heatmap1")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Heatmap1"), {
    handlePlotSelection("heatmap1", "Heatmap1", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Heatmap2 - click, zoom, selection
  observeEvent(event_data("plotly_click", source = "Heatmap2", priority = "event"), {
    handlePlotClick("heatmap2", "Heatmap2", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Heatmap2"), {
    handlePlotZoom("heatmap2", "Heatmap2")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Heatmap2"), {
    handlePlotSelection("heatmap2", "Heatmap2", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Heatmap3 - click, zoom, selection
  observeEvent(event_data("plotly_click", source = "Heatmap3", priority = "event"), {
    handlePlotClick("heatmap3", "Heatmap3", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_relayout", source = "Heatmap3"), {
    handlePlotZoom("heatmap3", "Heatmap3")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(event_data("plotly_selected", source = "Heatmap3"), {
    handlePlotSelection("heatmap3", "Heatmap3", session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # STRING ####
  observeEvent(input$string_network_organism, {
    handleStringOrganismSelection()
  }, ignoreInit = T)
  
  observeEvent(input$runStringNetwork, {
    handleStringNetwork()
  }, ignoreInit = T)
  
  observeEvent(input$string_submit_functional, {
    handleStringSubmitForEnrichment("functional")
  }, ignoreInit = T)

  observeEvent(input$string_submit_literature, {
    handleStringSubmitForEnrichment("literature")
  }, ignoreInit = T)
    
  # CONVERSION ####
  observeEvent(input$gconvert_button, {
    handle_gconvert()
  }, ignoreInit = T)
  
  observeEvent(input$gorth_organism, {
    handle_gorthOrganism()
  }, ignoreInit = T)
  
  observeEvent(input$gorth_button, {
    handle_gorth()
  }, ignoreInit = T)
  
  observeEvent(input$gconvert_addList, {
    dType <- input$gconvert_dType
    addConversionResultToInput("gconvert", dType)
  }, ignoreInit = T)
  
  observeEvent(input$gorth_addList, {
    dType <- input$gorth_dType
    addConversionResultToInput("gorth", dType)
  }, ignoreInit = T)
}
