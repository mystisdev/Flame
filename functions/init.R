initializeServerApp <- function() {
  toggleUpsetTab()
  session$sendCustomMessage("handler_setListLimit", LISTNAME_NCHAR_LIMIT)
  initialiazeOrganismSelectors()
  initializeDatasources()
  initializeVolcanoPanel()
  initializeReductionPanel()
  initializeEnrichmentResults()
  initializeArenaEdgelist()
  initializeSTRINGData()
  initializePlotTableState()
  initializeRunCounters()
  hideConversionBoxes()
  hideLiteratureVisNetworks()
  hideEnrichmentResultsPanel()
}

toggleUpsetTab <- function() {
  if (length(userInputLists) > 1) {
    showTab("inputPlots", "UpSet Plot")
    # Trigger pulse animation on the tab instead of forcing focus
    session$sendCustomMessage("handler_pulseUpsetTab", list())
  }
  else
    hideTab("inputPlots", "UpSet Plot")
}

initialiazeOrganismSelectors <- function() {
  ORGANISMS <- ORGANISMS[order(ORGANISMS$print_name),]
  selected <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
  gProfiler_printNames <- ORGANISMS[!is.na(ORGANISMS$short_name), ]$print_name
  STRING_printNames <- ORGANISMS[ORGANISMS$taxid %in% TOOL_ORGANISMS$STRING, ]$print_name
  updateSelectizeInput(session, 'textmining_organism',
                       choices = STRING_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'functional_enrichment_organism',
                       choices = ORGANISMS$print_name, selected = selected, server = T)
  updateSelectizeInput(session, 'literature_enrichment_organism',
                       choices = STRING_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'string_network_organism',
                       choices = STRING_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'gconvert_organism',
                       choices = gProfiler_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'gorth_organism',
                       choices = gProfiler_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'gorth_target', choices = gProfiler_printNames[gProfiler_printNames != selected],
                       selected = "Mus musculus (Mouse) [NCBI Tax. ID: 10090]", server = T)
}

initializeDatasources <- function() {
  for (datasourceName in names(DATASOURCES_CODES)) {
    DATASOURCES[[datasourceName]] <<- names(DATASOURCES_CODES[[datasourceName]])
  }
}

initializeVolcanoPanel <- function() {
  output$volcanoSelectionInfo <- renderText({
    "Pick the box or lasso from the plot toolbar and then select items. Double-click to reset view."
  })
  output$volcanoPlotStatus <- renderText({
    ""
  })
  shinyjs::hide("volcanoSelectionInfo")
  hideTab("inputPlots", "Volcano Plot")
  initializeVolcanoMetricsConversionText()
  shinyjs::hide("volcanoPanel")
}

initializeVolcanoMetricsConversionText <- function() {
  updateVolcanoMetricsConversionText(DEFAULT_VOLCANO_LOG10PVALUE_THRESHOLD,
                                     DEFAULT_VOLCANO_LOG2FC_THRESHOLD)
}

initializeReductionPanel <- function() {
  output$reductionSelectionInfo <- renderText({
    "Pick the box or lasso from the plot toolbar and then select items. Double-click to reset view."
  })
  output$reductionPlotStatus <- renderText({
    ""
  })
  shinyjs::hide("reductionSelectionInfo")
  hideTab("inputPlots", "2D Reduction Plot")
  shinyjs::hide("reductionPanel")
}

initializeEnrichmentResults <- function() {
  # Functional enrichment results are now created dynamically per-run
  # Only initialize literature enrichment (which stays single-result)
  enrichmentResults <<- list()
  newItem <- list(data.frame())
  names(newItem) <- paste("literature", "STRING", sep = "_")
  enrichmentResults <<- c(enrichmentResults, newItem)
}

initializeArenaEdgelist <- function() {
  # Functional arena edgelists are now created dynamically per-run
  # Only initialize literature enrichment (which stays single-result)
  arenaEdgelist <<- list()
  for (networkId in NETWORK_IDS) {
    newItem <- list(data.frame())
    names(newItem) <- paste("literature", "STRING", networkId, sep = "_")
    arenaEdgelist <<- c(arenaEdgelist, newItem)
  }
}

initializeSTRINGData <- function() {
  STRINGNetworkData <<- list()
}

# Hide only literature conversion boxes (functional ones are in dynamic tabs)
hideConversionBoxes <- function() {
  shinyjs::hide("literature_STRING_conversionBoxes")
}

# Hide only literature vis networks (functional ones are dynamic now)
hideLiteratureVisNetworks <- function() {
  lapply(NETWORK_IDS, function(networkId) {
    shinyjs::hide(paste("literature", "STRING", networkId, sep = "_"))
  })
}

# Hide the entire enrichment results panel (shown when first run is created)
hideEnrichmentResultsPanel <- function() {
  shinyjs::hide("functionalEnrichmentResultsPanel")
  # Combination tab should be hidden initially (shown when 2+ runs exist)
  hideTab(inputId = "toolTabsPanel", target = "Combination")
}
