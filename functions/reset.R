resetTextMiningResults <- function() {
  currentTextminingResult <<- c()
  output$extracted_text <- renderUI({ HTML("") })
  output$extracted_terms <- renderDataTable(c())
  shinyjs::hide("textmining_tagger_results")
}

resetCombination <- function() {
  combinationResult <<- data.frame()
  hideTab(inputId = "toolTabsPanel", target = "Combination")
  enrichmentBackgroundSizes <<- list()

  # Clear all combo outputs via registry
  outputRegistry$clearRun(COMBINATION_REGISTRY_KEY, output)
}

# Output clearing is now handled exclusively by outputRegistry$clearOutputs() 
# for consistency

resetEdgelist_ViewAndArenaObjects <- function(runKey, networkId) {
  arenaEdgelist[[paste(runKey, networkId, sep = "_")]] <<- data.frame()
  output[[paste(runKey, networkId, "edgelist", sep = "_")]] <- renderDataTable(c())
  output[[paste(runKey, networkId, sep = "_")]] <- renderVisNetwork({})
  shinyjs::hide(paste(runKey, networkId, sep = "_"))
}
