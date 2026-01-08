handleFunctionalEnrichmentOrganismSelection <- function() {
  tryCatch({
    updateAvailableTools()
  }, error = function(e) {
    cat(paste("[EnrichInputs] Organism selection error:", conditionMessage(e), "\n"))
    renderError("Unexpected error occurred.")
  })
}

handleFunctionalEnrichmentToolSelection <- function() {
  tryCatch({
    updateAvailableDatasources()
    updateAvailableNamespaces()
    updateAvailableSignificanceMetrics()
  }, error = function(e) {
    cat(paste("[EnrichInputs] Tool selection error:", conditionMessage(e), "\n"))
    renderError("Unexpected error occurred.")
  })
}


handleBackgroundModeSelection <- function(choice, enrichmentType) {
  tryCatch({
    updateBackgroundMode(choice, enrichmentType)
  }, error = function(e) {
    cat(paste("[EnrichInputs] Background mode error:", conditionMessage(e), "\n"))
    renderError("Unexpected error occurred.")
  })
}

handleBackgroundListUpdate <- function(enrichmentType) {
  tryCatch({
    updateBackgroundListChoices(enrichmentType)
  }, error = function(e) {
    cat(paste("[EnrichInputs] Background list error:", conditionMessage(e), "\n"))
    renderError("Unexpected error occurred.")
  })
}