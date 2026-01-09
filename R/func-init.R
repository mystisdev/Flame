initializeServerApp <- function() {
  # UpSet tab visibility is now managed by AnalyteListSetOperationsSession
  initialiazeOrganismSelectors()
  initializeDatasources()
  # Volcano and Reduction panels are dynamically inserted by session classes
  # via insertTab() when data is loaded - no initialization needed here
  initializeEnrichmentResults()
  initializeArenaEdgelist()
  # Note: STRING network data now initialized by NetworkAnalysisSession
  initializePlotTableState()
  initializeRunCounters()
  hideConversionBoxes()
  hideLiteratureVisNetworks()
  hideEnrichmentResultsPanel()
}

# toggleUpsetTab() removed - now managed by AnalyteListSetOperationsSession

initialiazeOrganismSelectors <- function() {
  ORGANISMS <- ORGANISMS[order(ORGANISMS$print_name),]
  selected <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
  STRING_printNames <- ORGANISMS[ORGANISMS$taxid %in% TOOL_ORGANISMS$STRING, ]$print_name
  updateSelectizeInput(session, 'textmining_organism',
                       choices = STRING_printNames, selected = selected, server = T)
  updateSelectizeInput(session, 'functional_enrichment_organism',
                       choices = ORGANISMS$print_name, selected = selected, server = T)
  updateSelectizeInput(session, 'literature_enrichment_organism',
                       choices = STRING_printNames, selected = selected, server = T)
  # Note: string_network_organism now initialized by NetworkAnalysisSession
  # Note: gconvert and gorth organism selectors now initialized by ConversionSession/OrthologySession
}

initializeDatasources <- function() {
  for (datasourceName in names(DATASOURCES_CODES)) {
    DATASOURCES[[datasourceName]] <<- names(DATASOURCES_CODES[[datasourceName]])
  }
}

# initializeVolcanoPanel() removed - uses insertTab() in VolcanoInputSession
# initializeReductionPanel() removed - uses insertTab() in ReductionInputSession

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

# initializeSTRINGData() removed - now handled by NetworkAnalysisSession

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
