# ============================================================================
# Dynamic Observer Registration
# ============================================================================
#
# This file contains functions to dynamically register Shiny observers
# when a new enrichment run tab is created. This is necessary because
# functional enrichment now supports multiple runs per tool, each with
# unique input IDs (e.g., functional_gProfiler_1_barchart_button).
#
# All observers are registered with observerRegistry for cleanup
# when tabs are closed. All observers per run MUST be destroyed to prevent
# memory leaks and race conditions from orphaned observers.
#
# Data Access (Part 2 Refactoring):
# - Sessions are retrieved from enrichmentSessionRegistry (not activeRuns)
# - Results are accessed via session$getResults() (not enrichmentResults global)
# - Arena edgelists accessed via session$getArenaEdgelist() (not arenaEdgelist global)

# Register all observers for a new functional enrichment run
registerObserversForRun <- function(fullRunKey) {
  runInfo <- parseFullRunKey(fullRunKey)
  enrichmentType <- runInfo$enrichmentType
  toolName <- runInfo$toolName
  runNumber <- runInfo$runNumber

  # Register datasource picker observers and track for cleanup
  datasourceObservers <- registerDatasourcePickerObservers(fullRunKey)
  observerRegistry$registerObservers(fullRunKey, datasourceObservers)

  # Register plot button observers and track for cleanup
  plotButtonObservers <- registerPlotButtonObservers(fullRunKey, enrichmentType, toolName)
  observerRegistry$registerObservers(fullRunKey, plotButtonObservers)

  # Register table filter and reset view observers and track for cleanup
  tableObservers <- registerTableObservers(fullRunKey)
  observerRegistry$registerObservers(fullRunKey, tableObservers)
}

# Register observers for datasource pickers (ALL_PLOT_IDS)
# Returns: list of 9 observer handles (one per plot component)
registerDatasourcePickerObservers <- function(fullRunKey) {
  observers <- lapply(ALL_PLOT_IDS, function(plotId) {
    inputId <- paste(fullRunKey, plotId, "sourceSelect", sep = "_")
    observeEvent(input[[inputId]], {
      handleDatasourcePickerForRun(fullRunKey, plotId)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })
  return(observers)
}

# Register observers for plot Generate buttons
# Returns: list of 12-13 observer handles (13 if gProfiler for Manhattan)
registerPlotButtonObservers <- function(fullRunKey, enrichmentType, toolName) {
  observers <- list()

  # Network buttons (3 observers)
  networkButtonObservers <- lapply(NETWORK_IDS, function(networkId) {
    buttonId <- paste(fullRunKey, networkId, "button", sep = "_")
    observeEvent(input[[buttonId]], {
      handleEnrichmentNetworkForRun(fullRunKey, networkId)
    }, ignoreInit = TRUE)
  })
  observers <- append(observers, networkButtonObservers)

  # Network Arena buttons (3 observers)
  arenaButtonObservers <- lapply(NETWORK_IDS, function(networkId) {
    arenaId <- paste(fullRunKey, networkId, "arena", sep = "_")
    observeEvent(input[[arenaId]], {
      arenaHandlerForRun(fullRunKey, networkId)
    }, ignoreInit = TRUE)
  })
  observers <- append(observers, arenaButtonObservers)

  # Heatmap buttons (3 observers)
  heatmapButtonObservers <- lapply(HEATMAP_IDS, function(heatmapId) {
    buttonId <- paste(fullRunKey, heatmapId, "button", sep = "_")
    observeEvent(input[[buttonId]], {
      handleHeatmapForRun(fullRunKey, heatmapId)
    }, ignoreInit = TRUE)
  })
  observers <- append(observers, heatmapButtonObservers)

  # Barchart button (1 observer)
  barchartButtonId <- paste(fullRunKey, "barchart_button", sep = "_")
  barchartObserver <- observeEvent(input[[barchartButtonId]], {
    handleBarchartForRun(fullRunKey)
  }, ignoreInit = TRUE)
  observers <- append(observers, list(barchartObserver))

  # Scatter plot button (1 observer)
  scatterButtonId <- paste(fullRunKey, "scatterPlot_button", sep = "_")
  scatterObserver <- observeEvent(input[[scatterButtonId]], {
    handleScatterPlotForRun(fullRunKey)
  }, ignoreInit = TRUE)
  observers <- append(observers, list(scatterObserver))

  # Dot plot button (1 observer)
  dotPlotButtonId <- paste(fullRunKey, "dotPlot_button", sep = "_")
  dotPlotObserver <- observeEvent(input[[dotPlotButtonId]], {
    handleDotPlotForRun(fullRunKey)
  }, ignoreInit = TRUE)
  observers <- append(observers, list(dotPlotObserver))

  # Manhattan button (only for gProfiler - 1 observer)
  if (toolName == "gProfiler") {
    manhattanButtonId <- paste(fullRunKey, "manhattan_button", sep = "_")
    manhattanObserver <- observeEvent(input[[manhattanButtonId]], {
      handleManhattanPlotForRun(fullRunKey)
    }, ignoreInit = TRUE)
    observers <- append(observers, list(manhattanObserver))
  }

  return(observers)
}

# Register observers for table filters and Reset View buttons
# Returns: list of ~30 observer handles (6 simple + 6 heatmap + 18 network)
registerTableObservers <- function(fullRunKey) {
  observers <- list()

  # Simple plot table filters (barchart, scatter, dot) - 6 observers (2 per plot)
  for (plotId in c("barchart", "scatterPlot", "dotPlot")) {
    tableId <- paste(fullRunKey, plotId, "table_rows_all", sep = "_")
    resetId <- paste(fullRunKey, plotId, "resetView", sep = "_")

    localObservers <- local({
      # Capture ALL variables used in observeEvent expressions
      localTableId <- tableId
      localResetId <- resetId
      localFullRunKey <- fullRunKey
      localPlotId <- plotId

      tableObs <- observeEvent(input[[localTableId]], {
        handleTableFilterChangeForRun(localFullRunKey, localPlotId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      resetObs <- observeEvent(input[[localResetId]], {
        handleResetViewForRun(localFullRunKey, localPlotId)
      }, ignoreInit = TRUE)

      # Return observers from local() scope
      list(tableObs, resetObs)
    })
    observers <- append(observers, localObservers)
  }

  # Heatmap table filters - 6 observers (2 per heatmap)
  for (heatmapId in c("heatmap1", "heatmap2", "heatmap3")) {
    tableId <- paste(fullRunKey, heatmapId, "table_rows_all", sep = "_")
    resetId <- paste(fullRunKey, heatmapId, "resetView", sep = "_")

    localObservers <- local({
      # Capture ALL variables used in observeEvent expressions
      localTableId <- tableId
      localResetId <- resetId
      localFullRunKey <- fullRunKey
      localHeatmapId <- heatmapId

      tableObs <- observeEvent(input[[localTableId]], {
        handleTableFilterChangeForRun(localFullRunKey, localHeatmapId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      resetObs <- observeEvent(input[[localResetId]], {
        handleResetViewForRun(localFullRunKey, localHeatmapId)
      }, ignoreInit = TRUE)

      # Return observers from local() scope
      list(tableObs, resetObs)
    })
    observers <- append(observers, localObservers)
  }

  # Network table observers - 18 observers (6 per network)
  for (networkId in NETWORK_IDS) {
    networkInputId <- paste(fullRunKey, networkId, sep = "_")
    tableId <- paste(fullRunKey, networkId, "table_rows_all", sep = "_")
    edgelistId <- paste(fullRunKey, networkId, "edgelist_rows_all", sep = "_")
    resetId <- paste(fullRunKey, networkId, "resetView", sep = "_")
    clickId <- paste(networkInputId, "click", sep = "_")
    selectedId <- paste(networkInputId, "selected", sep = "_")
    deselectId <- paste(networkInputId, "deselect", sep = "_")

    localObservers <- local({
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
      clickObs <- observeEvent(input[[localClickId]], {
        handleNetworkNodeClickForRun(localFullRunKey, localNetworkId)
        handleNetworkEdgeClickForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Multi-node selection
      selectedObs <- observeEvent(input[[localSelectedId]], {
        handleNetworkSelectionForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Deselection - restore view when nodes are deselected
      deselectObs <- observeEvent(input[[localDeselectId]], {
        handleNetworkDeselectionForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Enrichment table filter -> network update
      tableObs <- observeEvent(input[[localTableId]], {
        handleNetworkEnrichmentTableFilterForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Edgelist table filter -> network update
      edgelistObs <- observeEvent(input[[localEdgelistId]], {
        handleNetworkEdgelistTableFilterForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # Reset View button
      resetObs <- observeEvent(input[[localResetId]], {
        handleNetworkResetViewForRun(localFullRunKey, localNetworkId)
      }, ignoreInit = TRUE)

      # Return observers from local() scope
      list(clickObs, selectedObs, deselectObs, tableObs, edgelistObs, resetObs)
    })
    observers <- append(observers, localObservers)
  }

  return(observers)
}

# Handler wrappers for run-based operations
# These delegate to the existing handlers with the fullRunKey

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
  # Get session from registry instead of global enrichmentResults

  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return(10)

  enrichmentResult <- session$getResults()
  if (is.null(enrichmentResult)) return(10)

  maxSliderValue <- nrow(subset(enrichmentResult, Source %in% datasources))
  if (maxSliderValue > MAX_SLIDER_VALUE) {
    maxSliderValue <- MAX_SLIDER_VALUE
  }
  return(maxSliderValue)
}

# Wrapper handlers for plot generation
# These get sessions from enrichmentSessionRegistry and pass to handlers

handleEnrichmentNetworkForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) {
    warning(paste("handleEnrichmentNetworkForRun: No session found for key:", fullRunKey))
    return()
  }
  handleEnrichmentNetwork(session, networkId)
}

arenaHandlerForRun <- function(fullRunKey, networkId) {
  enrichSession <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(enrichSession)) {
    warning(paste("arenaHandlerForRun: No session found for key:", fullRunKey))
    return()
  }

  # Get edgelist from session instead of global
  edgelist <- enrichSession$getArenaEdgelist(networkId)

  if (is.null(edgelist) || nrow(edgelist) == 0) {
    renderWarning("Make sure a visible network exists.")
    return()
  }

  tryCatch({
    renderModal("<h2>Please wait.</h2><p>Building network for Arena3Dweb</p>")
    result <- arena_export(edgelist)

    if (result$success) {
      session$sendCustomMessage("handler_browseUrl", result$url)
    } else {
      renderWarning(result$error)
    }
  }, error = function(e) {
    cat("[arenaHandlerForRun] Error:", conditionMessage(e), "\n")
    renderWarning("Cannot open Arena3Dweb network at this time. Try again in a while.")
  }, finally = {
    removeModal()
  })
}

handleHeatmapForRun <- function(fullRunKey, heatmapId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) {
    warning(paste("handleHeatmapForRun: No session found for key:", fullRunKey))
    return()
  }
  handleHeatmap(session, heatmapId)
}

handleBarchartForRun <- function(fullRunKey) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) {
    warning(paste("handleBarchartForRun: No session found for key:", fullRunKey))
    return()
  }
  handleBarchart(session)
}

handleScatterPlotForRun <- function(fullRunKey) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) {
    warning(paste("handleScatterPlotForRun: No session found for key:", fullRunKey))
    return()
  }
  handleScatterPlot(session)
}

handleDotPlotForRun <- function(fullRunKey) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) {
    warning(paste("handleDotPlotForRun: No session found for key:", fullRunKey))
    return()
  }
  handleDotPlot(session)
}

handleManhattanPlotForRun <- function(fullRunKey) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) {
    warning(paste("handleManhattanPlotForRun: No session found for key:", fullRunKey))
    return()
  }
  handleManhattanPlot(session)
}

handleTableFilterChangeForRun <- function(fullRunKey, plotId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleTableFilterChange(session, plotId)
}

handleResetViewForRun <- function(fullRunKey, plotId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleResetView(session, plotId)
}

# Network Handler Wrappers for Sessions

handleNetworkNodeClickForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkNodeClick(session, networkId)
}

handleNetworkEdgeClickForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkEdgeClick(session, networkId)
}

handleNetworkSelectionForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkSelection(session, networkId)
}

handleNetworkDeselectionForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkDeselection(session, networkId)
}

handleNetworkEnrichmentTableFilterForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkEnrichmentTableFilter(session, networkId)
}

handleNetworkEdgelistTableFilterForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkEdgelistTableFilter(session, networkId)
}

handleNetworkResetViewForRun <- function(fullRunKey, networkId) {
  session <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(session)) return()
  handleNetworkResetView(session, networkId)
}

# Control Panel Updates for Sessions

updatePlotControlPanelsForRun <- function(fullRunKey) {
  selectedDataSource <- updatePlotDataSourcesForRun(fullRunKey)
  updatePlotSliderInputsForRun(fullRunKey, selectedDataSource)
}

updatePlotDataSourcesForRun <- function(fullRunKey) {
  # Get session from registry instead of global enrichmentResults
  enrichSession <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(enrichSession)) return(NULL)

  result <- enrichSession$getResults()
  if (is.null(result) || nrow(result) == 0) return(NULL)

  # Extract enrichment type from run key to get valid datasources
  runInfo <- parseFullRunKey(fullRunKey)
  enrichmentType <- runInfo$enrichmentType

  # Use config to get valid datasources for this type
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
  # Get session from registry instead of global enrichmentResults
  enrichSession <- enrichmentSessionRegistry$get(fullRunKey)
  if (is.null(enrichSession)) return()

  result <- enrichSession$getResults()
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
