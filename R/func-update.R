# =============================================================================
# UI UPDATE FUNCTIONS
# =============================================================================
# Functions for updating Shiny UI elements in response to user actions.
#
# NOTE: Enrichment form cascade functions (updateAvailableTools, updateAvailableDatasources,
# updateAvailableNamespaces, updateAvailableSignificanceMetrics, updateBackgroundMode,
# updateBackgroundListChoices) have been moved to EnrichmentFormSession in enrich-form.R.
# =============================================================================

# Generic slider input update helper
updateShinySliderInput <- function(shinyOutputId, minSliderValue, maxSliderValue,
                                   value = DEFAULT_SLIDER_VALUE, step = 1) {
  updateSliderInput(
    session, shinyOutputId,
    min = minSliderValue, max = maxSliderValue,
    value = value, step = step
  )
}

# Parameterized versions of plot control update functions (explicit inputs, no globals)

updatePlotControlPanelsForTool <- function(enrichmentType, tool) {
  type_Tool <- paste(enrichmentType, tool, sep = "_")
  selectedDataSource <- updatePlotDataSourcesForTool(enrichmentType, type_Tool)
  updatePlotSliderInputsForTool(enrichmentType, type_Tool, selectedDataSource)
}

updatePlotDataSourcesForTool <- function(enrichmentType, type_Tool) {
  # Get datasources that have results
  sources <- ENRICHMENT_DATASOURCES[
    which(ENRICHMENT_DATASOURCES %in% unique(enrichmentResults[[type_Tool]]$Source))
  ]
  selected <- sources[1]

  lapply(ALL_PLOT_IDS, function(plotId) {
    updatePickerInput(
      session, paste(type_Tool, plotId, "sourceSelect", sep = "_"),
      choices = sources, selected = selected
    )
  })
  return(selected)
}

updatePlotSliderInputsForTool <- function(enrichmentType, type_Tool, selectedDataSource) {
  maxSliderValue <- nrow(enrichmentResults[[type_Tool]][grepl(
    selectedDataSource, enrichmentResults[[type_Tool]]$Source), ])
  if (maxSliderValue > MAX_SLIDER_VALUE)
    maxSliderValue <- MAX_SLIDER_VALUE

  lapply(ALL_PLOT_IDS, function(plotId) {
    updateShinySliderInput(
      shinyOutputId = paste(type_Tool, plotId, "slider", sep = "_"),
      minSliderValue = 1, maxSliderValue)
  })
  updateShinySliderInput(
    shinyOutputId = paste(type_Tool, "network3_thresholdSlider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
}
