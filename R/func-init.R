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
  # initializeRunCounters() removed - now handled by enrichmentSessionRegistry
  hideEnrichmentResultsPanel()
}

# toggleUpsetTab() removed - now managed by AnalyteListSetOperationsSession

initialiazeOrganismSelectors <- function() {
  ORGANISMS <- ORGANISMS[order(ORGANISMS$print_name),]
  selected <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
  STRING_printNames <- ORGANISMS[ORGANISMS$taxid %in% TOOL_ORGANISMS$STRING, ]$print_name
  updateSelectizeInput(session, 'textmining_organism',
                       choices = STRING_printNames, selected = selected, server = T)
  # Note: functional_enrichment_organism now initialized by EnrichmentFormSession
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
  # Enrichment results are created dynamically per-run
  enrichmentResults <<- list()
}

initializeArenaEdgelist <- function() {
  # Arena edgelists are created dynamically per-run
  arenaEdgelist <<- list()
}

# initializeSTRINGData() removed - now handled by NetworkAnalysisSession

# Hide the entire enrichment results panel (shown when first run is created)
hideEnrichmentResultsPanel <- function() {
  shinyjs::hide("functionalEnrichmentResultsPanel")
  # Combination tab should be hidden initially (shown when 2+ runs exist)
  hideTab(inputId = "toolTabsPanel", target = "Combination")
}
