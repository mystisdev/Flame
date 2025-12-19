# Dynamic Observer Registration for Multi-Run Architecture
#
# This file contains functions to dynamically register Shiny observers
# when a new enrichment run tab is created. This is necessary because
# functional enrichment now supports multiple runs per tool, each with
# unique input IDs (e.g., functional_gProfiler_1_barchart_button).

# Register all observers for a new functional enrichment run
registerObserversForRun <- function(fullRunKey) {
  runInfo <- parseFullRunKey(fullRunKey)
  enrichmentType <- runInfo$enrichmentType
  toolName <- runInfo$toolName
  runNumber <- runInfo$runNumber

  # Register datasource picker observers
  registerDatasourcePickerObservers(fullRunKey)

  # Register plot button observers
  registerPlotButtonObservers(fullRunKey, enrichmentType, toolName)

  # Register table filter and reset view observers
  registerTableObservers(fullRunKey)
}

# Register observers for datasource pickers (ALL_PLOT_IDS)
registerDatasourcePickerObservers <- function(fullRunKey) {
  lapply(ALL_PLOT_IDS, function(plotId) {
    inputId <- paste(fullRunKey, plotId, "sourceSelect", sep = "_")
    observeEvent(input[[inputId]], {
      handleDatasourcePickerForRun(fullRunKey, plotId)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })
}

# Register observers for plot Generate buttons
registerPlotButtonObservers <- function(fullRunKey, enrichmentType, toolName) {
  # Network buttons
  lapply(NETWORK_IDS, function(networkId) {
    buttonId <- paste(fullRunKey, networkId, "button", sep = "_")
    observeEvent(input[[buttonId]], {
      handleEnrichmentNetworkForRun(fullRunKey, networkId)
    }, ignoreInit = TRUE)
  })

  # Network Arena buttons
  lapply(NETWORK_IDS, function(networkId) {
    arenaId <- paste(fullRunKey, networkId, "arena", sep = "_")
    observeEvent(input[[arenaId]], {
      arenaHandlerForRun(fullRunKey, networkId)
    }, ignoreInit = TRUE)
  })

  # Heatmap buttons
  lapply(HEATMAP_IDS, function(heatmapId) {
    buttonId <- paste(fullRunKey, heatmapId, "button", sep = "_")
    observeEvent(input[[buttonId]], {
      handleHeatmapForRun(fullRunKey, heatmapId)
    }, ignoreInit = TRUE)
  })

  # Barchart button
  barchartButtonId <- paste(fullRunKey, "barchart_button", sep = "_")
  observeEvent(input[[barchartButtonId]], {
    handleBarchartForRun(fullRunKey)
  }, ignoreInit = TRUE)

  # Scatter plot button
  scatterButtonId <- paste(fullRunKey, "scatterPlot_button", sep = "_")
  observeEvent(input[[scatterButtonId]], {
    handleScatterPlotForRun(fullRunKey)
  }, ignoreInit = TRUE)

  # Dot plot button
  dotPlotButtonId <- paste(fullRunKey, "dotPlot_button", sep = "_")
  observeEvent(input[[dotPlotButtonId]], {
    handleDotPlotForRun(fullRunKey)
  }, ignoreInit = TRUE)

  # Manhattan button (only for gProfiler)
  if (toolName == "gProfiler") {
    manhattanButtonId <- paste(fullRunKey, "manhattan_button", sep = "_")
    observeEvent(input[[manhattanButtonId]], {
      handleManhattanPlotForRun(fullRunKey)
    }, ignoreInit = TRUE)
  }
}

# Register observers for table filters and Reset View buttons
registerTableObservers <- function(fullRunKey) {
  # Simple plot table filters (barchart, scatter, dot)
  for (plotId in c("barchart", "scatterPlot", "dotPlot")) {
    tableId <- paste(fullRunKey, plotId, "table_rows_all", sep = "_")
    resetId <- paste(fullRunKey, plotId, "resetView", sep = "_")

    local({
      # Capture ALL variables used in observeEvent expressions
      localTableId <- tableId
      localResetId <- resetId
      localFullRunKey <- fullRunKey
      localPlotId <- plotId

      observeEvent(input[[localTableId]], {
        handleTableFilterChangeForRun(localFullRunKey, localPlotId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      observeEvent(input[[localResetId]], {
        handleResetViewForRun(localFullRunKey, localPlotId)
      }, ignoreInit = TRUE)
    })
  }

  # Heatmap table filters
  for (heatmapId in c("heatmap1", "heatmap2", "heatmap3")) {
    tableId <- paste(fullRunKey, heatmapId, "table_rows_all", sep = "_")
    resetId <- paste(fullRunKey, heatmapId, "resetView", sep = "_")

    local({
      # Capture ALL variables used in observeEvent expressions
      localTableId <- tableId
      localResetId <- resetId
      localFullRunKey <- fullRunKey
      localHeatmapId <- heatmapId

      observeEvent(input[[localTableId]], {
        handleTableFilterChangeForRun(localFullRunKey, localHeatmapId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      observeEvent(input[[localResetId]], {
        handleResetViewForRun(localFullRunKey, localHeatmapId)
      }, ignoreInit = TRUE)
    })
  }

  # Network table observers (enrichment table, edgelist table, reset view)
  for (networkId in NETWORK_IDS) {
    networkInputId <- paste(fullRunKey, networkId, sep = "_")
    tableId <- paste(fullRunKey, networkId, "table_rows_all", sep = "_")
    edgelistId <- paste(fullRunKey, networkId, "edgelist_rows_all", sep = "_")
    resetId <- paste(fullRunKey, networkId, "resetView", sep = "_")
    clickId <- paste(networkInputId, "click", sep = "_")
    selectedId <- paste(networkInputId, "selected", sep = "_")
    deselectId <- paste(networkInputId, "deselect", sep = "_")

    local({
      # Capture ALL variables used in observeEvent expressions
      localFullRunKey <- fullRunKey
      localNetworkId <- networkId
      localTableId <- tableId
      localEdgelistId <- edgelistId
      localResetId <- resetId
      localClickId <- clickId
      localSelectedId <- selectedId
      localDeselectId <- deselectId

      # Network click event (handles both node and edge clicks)
      observeEvent(input[[localClickId]], {
        handleNetworkNodeClickForRun(localFullRunKey, localNetworkId)
        handleNetworkEdgeClickForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Multi-node selection
      observeEvent(input[[localSelectedId]], {
        handleNetworkSelectionForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Deselection - restore view when nodes are deselected
      observeEvent(input[[localDeselectId]], {
        handleNetworkDeselectionForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Enrichment table filter -> network update
      observeEvent(input[[localTableId]], {
        handleNetworkEnrichmentTableFilterForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Edgelist table filter -> network update
      observeEvent(input[[localEdgelistId]], {
        handleNetworkEdgelistTableFilterForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Reset View button
      observeEvent(input[[localResetId]], {
        handleNetworkResetViewForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE)
    })
  }
}

# Handler wrappers for run-based operations
# These delegate to the existing handlers with the fullRunKey

# Higher-order function to set global context and call a handler
# This eliminates duplication across all the ForRun wrapper functions
withRunContext <- function(fullRunKey, handler, ...) {
  runInfo <- parseFullRunKey(fullRunKey)
  currentEnrichmentType <<- runInfo$enrichmentType
  currentEnrichmentTool <<- runInfo$toolName
  currentFullRunKey <<- fullRunKey
  currentType_Tool <<- fullRunKey
  handler(runInfo$enrichmentType, runInfo$runId, ...)
}

handleDatasourcePickerForRun <- function(fullRunKey, componentId) {
  tryCatch({
    datasources <- input[[paste(fullRunKey, componentId, "sourceSelect", sep = "_")]]
    maxSliderValue <- calculateMaxSliderValueForRun(fullRunKey, datasources)

    sliderId <- paste(fullRunKey, componentId, "slider", sep = "_")
    updateShinySliderInput(shinyOutputId = sliderId, minSliderValue = 1, maxSliderValue = maxSliderValue)

    if (componentId == "network3") {
      updateShinySliderInput(
        shinyOutputId = paste(fullRunKey, "network3_thresholdSlider", sep = "_"),
        minSliderValue = 1, maxSliderValue = maxSliderValue,
        value = round(maxSliderValue / 10)
      )
    }
  }, error = function(e) {
    message("Datasource picker error (run): ", e$message)
    renderWarning("Could not update slider filter values properly.")
  })
}

calculateMaxSliderValueForRun <- function(fullRunKey, datasources) {
  enrichmentResult <- enrichmentResults[[fullRunKey]]
  if (is.null(enrichmentResult)) return(10)

  maxSliderValue <- nrow(subset(enrichmentResult, Source %in% datasources))
  if (maxSliderValue > MAX_SLIDER_VALUE) {
    maxSliderValue <- MAX_SLIDER_VALUE
  }
  return(maxSliderValue)
}

# Wrapper handlers for plot generation
# These use withRunContext() to set global context and delegate to existing handlers
# NOTE: We pass runInfo$runId (e.g., "gProfiler_1") as toolName parameter
# so handlers construct the correct key (e.g., "functional_gProfiler_1")

handleEnrichmentNetworkForRun <- function(fullRunKey, networkId) {
  withRunContext(fullRunKey, handleEnrichmentNetwork, networkId)
}

arenaHandlerForRun <- function(fullRunKey, networkId) {
  withRunContext(fullRunKey, arenaHandler, networkId)
}

handleHeatmapForRun <- function(fullRunKey, heatmapId) {
  withRunContext(fullRunKey, handleHeatmap, heatmapId)
}

handleBarchartForRun <- function(fullRunKey) {
  withRunContext(fullRunKey, handleBarchart)
}

handleScatterPlotForRun <- function(fullRunKey) {
  withRunContext(fullRunKey, handleScatterPlot)
}

handleDotPlotForRun <- function(fullRunKey) {
  withRunContext(fullRunKey, handleDotPlot)
}

handleManhattanPlotForRun <- function(fullRunKey) {
  # Manhattan is special - it takes fullRunKey directly, not (type, runId)
  runInfo <- parseFullRunKey(fullRunKey)
  currentEnrichmentType <<- runInfo$enrichmentType
  currentEnrichmentTool <<- runInfo$toolName
  currentFullRunKey <<- fullRunKey
  currentType_Tool <<- fullRunKey
  handleManhattanPlot(fullRunKey)
}

handleTableFilterChangeForRun <- function(fullRunKey, plotId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleTableFilterChange(runInfo$enrichmentType, runInfo$runId, plotId)
}

handleResetViewForRun <- function(fullRunKey, plotId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleResetView(runInfo$enrichmentType, runInfo$runId, plotId)
}

# Network Handler Wrappers for Runs

handleNetworkNodeClickForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkNodeClick(runInfo$enrichmentType, runInfo$runId, networkId)
}

handleNetworkEdgeClickForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkEdgeClick(runInfo$enrichmentType, runInfo$runId, networkId)
}

handleNetworkSelectionForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkSelection(runInfo$enrichmentType, runInfo$runId, networkId)
}

handleNetworkDeselectionForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkDeselection(runInfo$enrichmentType, runInfo$runId, networkId)
}

handleNetworkEnrichmentTableFilterForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkEnrichmentTableFilter(runInfo$enrichmentType, runInfo$runId, networkId)
}

handleNetworkEdgelistTableFilterForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkEdgelistTableFilter(runInfo$enrichmentType, runInfo$runId, networkId)
}

handleNetworkResetViewForRun <- function(fullRunKey, networkId) {
  runInfo <- parseFullRunKey(fullRunKey)
  handleNetworkResetView(runInfo$enrichmentType, runInfo$runId, networkId)
}

# Control Panel Updates for Runs

updatePlotControlPanelsForRun <- function(fullRunKey) {
  selectedDataSource <- updatePlotDataSourcesForRun(fullRunKey)
  updatePlotSliderInputsForRun(fullRunKey, selectedDataSource)
}

updatePlotDataSourcesForRun <- function(fullRunKey) {
  result <- enrichmentResults[[fullRunKey]]
  if (is.null(result) || nrow(result) == 0) return(NULL)

  # Extract enrichment type from run key to get valid datasources
  runInfo <- parseFullRunKey(fullRunKey)
  enrichmentType <- runInfo$enrichmentType

  # Use config to get valid datasources for this type
  # This fixes the bug where PUBMED wasn't found for literature enrichment
  validDatasources <- getValidDatasources(enrichmentType)

  # Filter against what's actually in results
  availableSources <- unique(result$Source)
  sources <- validDatasources[which(validDatasources %in% availableSources)]

  if (length(sources) == 0) return(NULL)

  selected <- sources[1]

  lapply(ALL_PLOT_IDS, function(plotId) {
    updatePickerInput(
      session, paste(fullRunKey, plotId, "sourceSelect", sep = "_"),
      choices = sources, selected = selected
    )
  })
  return(selected)
}

updatePlotSliderInputsForRun <- function(fullRunKey, selectedDataSource) {
  result <- enrichmentResults[[fullRunKey]]
  if (is.null(result) || nrow(result) == 0 || is.null(selectedDataSource)) return()

  maxSliderValue <- nrow(result[grepl(selectedDataSource, result$Source), ])
  if (maxSliderValue > MAX_SLIDER_VALUE) {
    maxSliderValue <- MAX_SLIDER_VALUE
  }

  lapply(ALL_PLOT_IDS, function(plotId) {
    updateShinySliderInput(
      shinyOutputId = paste(fullRunKey, plotId, "slider", sep = "_"),
      minSliderValue = 1, maxSliderValue)
  })
  updateShinySliderInput(
    shinyOutputId = paste(fullRunKey, "network3_thresholdSlider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
}
