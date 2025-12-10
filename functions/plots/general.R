# Plot-Table Synchronization State Management
#
# Maintains separate original/current data stores with source-aware update
# tracking to prevent feedback loops between plots and tables.

initializePlotTableState <- function() {
  # Original data set on Generate, used for Reset View
  plotOriginalData <<- reactiveValues()

  # Current view data that both plot and table render from
  plotCurrentView <<- reactiveValues()

  # Tracks update source and timing to prevent feedback loops
  plotUpdateTracker <<- reactiveValues()

  # Current network view for restoring tables on deselect
  currentNetworkView <<- reactiveValues()

  # Expected row counts for cascade detection (simple list, not reactive)
  expectedRowCounts <<- list()
}

# Simple list for tracking programmatic table updates (not reactiveValues)
# This avoids reactivity issues when setting/getting values
pendingTableUpdates <- list()

getStateKey <- function(type_Tool, plotId) {
  paste(type_Tool, plotId, sep = "_")
}


# Original data functions (immutable after Generate)

setOriginalData <- function(type_Tool, plotId, data) {
  key <- getStateKey(type_Tool, plotId)
  plotOriginalData[[key]] <<- data
  plotCurrentView[[key]] <<- data
  plotUpdateTracker[[key]] <<- list(counter = 0, source = "generate", time = Sys.time())
}

getOriginalData <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  return(plotOriginalData[[key]])
}



# Current view functions

updateCurrentView <- function(type_Tool, plotId, data, source) {
  key <- getStateKey(type_Tool, plotId)
  plotCurrentView[[key]] <<- data

  tracker <- plotUpdateTracker[[key]]
  if (is.null(tracker)) {
    tracker <- list(counter = 0, source = "", time = NULL)
  }

  plotUpdateTracker[[key]] <<- list(
    counter = tracker$counter + 1,
    source = source,
    time = Sys.time()
  )
}

getCurrentView <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  return(plotCurrentView[[key]])
}


# Feedback loop prevention

shouldProcessUpdate <- function(type_Tool, plotId, expectedSource) {
  key <- getStateKey(type_Tool, plotId)
  tracker <- plotUpdateTracker[[key]]

  if (is.null(tracker)) return(TRUE)

  lastSource <- tracker$source
  lastTime <- tracker$time

  # Allow if enough time has passed (500ms indicates new user action)
  if (!is.null(lastTime) && as.numeric(Sys.time() - lastTime) > 0.5) {
    return(TRUE)
  }

  # Skip table events that were triggered by plot updates (within 500ms)
  if (expectedSource == "table") {
    if (lastSource %in% c("plot_click", "plot_zoom", "plot_select")) {
      return(FALSE)
    }
  }

  # Skip plot events triggered by table updates or our own re-renders
  if (expectedSource == "plot") {
    blockedSources <- c("table_filter", "plot_click", "plot_select",
                        "plot_zoom", "reset", "legend_click")
    if (lastSource %in% blockedSources) {
      return(FALSE)
    }
  }

  return(TRUE)
}

setUpdateSource <- function(type_Tool, plotId, source) {
  key <- getStateKey(type_Tool, plotId)
  tracker <- plotUpdateTracker[[key]]
  if (is.null(tracker)) {
    tracker <- list(counter = 0, source = "", time = NULL)
  }
  plotUpdateTracker[[key]] <<- list(
    counter = tracker$counter + 1,
    source = source,
    time = Sys.time()
  )
}

markUpdateProcessed <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  tracker <- plotUpdateTracker[[key]]

  if (!is.null(tracker)) {
    plotUpdateTracker[[key]] <<- list(
      counter = tracker$counter,
      source = "",
      time = NULL
    )
  }
}


# Utility functions

hasPlotData <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  data <- plotCurrentView[[key]]
  return(!is.null(data) && nrow(data) > 0)
}

clearPlotState <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  plotOriginalData[[key]] <<- NULL
  plotCurrentView[[key]] <<- NULL
  plotUpdateTracker[[key]] <<- NULL
}


# Network-specific state functions
# Networks have 4 components: enrichment, edgelist, nodes, edges

setNetworkOriginalData <- function(type_Tool, networkId,
                                   enrichmentData, edgelistData, nodes, edges) {
  setOriginalData(type_Tool, paste0(networkId, "_enrichment"), enrichmentData)
  setOriginalData(type_Tool, paste0(networkId, "_edgelist"), edgelistData)
  setOriginalData(type_Tool, paste0(networkId, "_nodes"), nodes)
  setOriginalData(type_Tool, paste0(networkId, "_edges"), edges)
}

getNetworkEnrichmentData <- function(type_Tool, networkId) {
  getCurrentView(type_Tool, paste0(networkId, "_enrichment"))
}

getNetworkEdgelistData <- function(type_Tool, networkId) {
  getCurrentView(type_Tool, paste0(networkId, "_edgelist"))
}

getNetworkNodes <- function(type_Tool, networkId) {
  getCurrentView(type_Tool, paste0(networkId, "_nodes"))
}

getNetworkEdges <- function(type_Tool, networkId) {
  getCurrentView(type_Tool, paste0(networkId, "_edges"))
}

getNetworkOriginalEnrichment <- function(type_Tool, networkId) {
  getOriginalData(type_Tool, paste0(networkId, "_enrichment"))
}

getNetworkOriginalEdgelist <- function(type_Tool, networkId) {
  getOriginalData(type_Tool, paste0(networkId, "_edgelist"))
}

getNetworkOriginalNodes <- function(type_Tool, networkId) {
  getOriginalData(type_Tool, paste0(networkId, "_nodes"))
}

getNetworkOriginalEdges <- function(type_Tool, networkId) {
  getOriginalData(type_Tool, paste0(networkId, "_edges"))
}

updateNetworkEnrichmentView <- function(type_Tool, networkId, data, source) {
  updateCurrentView(type_Tool, paste0(networkId, "_enrichment"), data, source)
}

updateNetworkEdgelistView <- function(type_Tool, networkId, data, source) {
  updateCurrentView(type_Tool, paste0(networkId, "_edgelist"), data, source)
}

updateNetworkStructureView <- function(type_Tool, networkId, nodes, edges, source) {
  updateCurrentView(type_Tool, paste0(networkId, "_nodes"), nodes, source)
  updateCurrentView(type_Tool, paste0(networkId, "_edges"), edges, source)
}

# Cascade blocking is handled by isReRenderCascade() with explicit row count tracking
shouldProcessNetworkUpdate <- function(type_Tool, networkId, expectedSource, tableType = NULL) {
  key <- getStateKey(type_Tool, paste0(networkId, "_enrichment"))
  tracker <- plotUpdateTracker[[key]]

  if (is.null(tracker)) return(TRUE)

  # Network events are always user-initiated (visNetwork doesn't fire on programmatic re-renders)
  if (expectedSource == "network") {
    return(TRUE)
  }

  # Table observers use isReRenderCascade() for blocking
  return(TRUE)
}

setNetworkUpdateSource <- function(type_Tool, networkId, source) {
  setUpdateSource(type_Tool, paste0(networkId, "_enrichment"), source)
  setUpdateSource(type_Tool, paste0(networkId, "_edgelist"), source)
}

clearNetworkState <- function(type_Tool, networkId) {
  clearPlotState(type_Tool, paste0(networkId, "_enrichment"))
  clearPlotState(type_Tool, paste0(networkId, "_edgelist"))
  clearPlotState(type_Tool, paste0(networkId, "_nodes"))
  clearPlotState(type_Tool, paste0(networkId, "_edges"))
}

hasNetworkData <- function(type_Tool, networkId) {
  hasPlotData(type_Tool, paste0(networkId, "_enrichment"))
}


# Programmatic table update tracking
# Uses a simple list (not reactiveValues) to avoid reactivity complications.
# When we programmatically update a table, we mark it as pending.
# When handleTableFilterChange fires, it checks and clears this flag.

markTableUpdatePending <- function(tableId) {
  pendingTableUpdates[[tableId]] <<- TRUE
}

isProgrammaticTableUpdate <- function(tableId) {
  pending <- pendingTableUpdates[[tableId]]
  if (!is.null(pending) && isTRUE(pending)) {
    pendingTableUpdates[[tableId]] <<- NULL
    return(TRUE)
  }
  return(FALSE)
}

# Row count tracking for cascade detection
# When we render a table, we set the expected row count. When a table filter
# event fires, we check if the count matches - if so, it's a cascade from
# our render (not a user filter action) and should be skipped.

setExpectedRowCount <- function(tableId, count) {
  expectedRowCounts[[tableId]] <<- count
}

isReRenderCascade <- function(tableId, filteredRows) {
  expected <- expectedRowCounts[[tableId]]
  if (!is.null(expected) && length(filteredRows) == expected) {
    expectedRowCounts[[tableId]] <<- NULL
    return(TRUE)
  }
  return(FALSE)
}


# Current network view tracking (for deselect restore)

setCurrentNetworkView <- function(type_Tool, networkId,
                                   enrichmentData, edgelistData) {
  enrichKey <- paste(type_Tool, networkId, "enrichment", sep = "_")
  edgelistKey <- paste(type_Tool, networkId, "edgelist", sep = "_")
  currentNetworkView[[enrichKey]] <<- enrichmentData
  currentNetworkView[[edgelistKey]] <<- edgelistData
}

getCurrentNetworkViewEnrichment <- function(type_Tool, networkId) {
  key <- paste(type_Tool, networkId, "enrichment", sep = "_")
  currentNetworkView[[key]]
}

getCurrentNetworkViewEdgelist <- function(type_Tool, networkId) {
  key <- paste(type_Tool, networkId, "edgelist", sep = "_")
  currentNetworkView[[key]]
}


# General plot functions

handleDatasourcePicker <- function(enrichmentType, toolName, componentId) {
  tryCatch({
    type_Tool <- paste(enrichmentType, toolName, sep = "_")
    datasources <- input[[paste(type_Tool, componentId, "sourceSelect", sep = "_")]]
    maxSliderValue <- calculateMaxSliderValue(enrichmentType, toolName, datasources)

    sliderId <- paste(type_Tool, componentId, "slider", sep = "_")
    updateShinySliderInput(shinyOutputId = sliderId,
                           min = 1, maxSliderValue)
    if (componentId == "network3") {
      updateShinySliderInput(
        shinyOutputId = paste(type_Tool, "network3_thresholdSlider", sep = "_"),
        min = 1, maxSliderValue,
        value = round(maxSliderValue / 10)
      )
    }
  }, error = function(e) {
    message("Datasource picker error: ", e$message)
    renderWarning("Could not update slider filter values properly.")
  })
}

calculateMaxSliderValue <- function(enrichmentType, toolName, datasources) {
  enrichmentResult <- getGlobalEnrichmentResult(enrichmentType, toolName)
  maxSliderValue <- nrow(
    subset(
      enrichmentResult,
      Source %in% datasources
    )
  )
  if (maxSliderValue > MAX_SLIDER_VALUE)
    maxSliderValue <- MAX_SLIDER_VALUE
  return(maxSliderValue)
}

existEnrichmentResults <- function(enrichmentType, enrichmentTool) {
  enrichmentResult <- getGlobalEnrichmentResult(enrichmentType, enrichmentTool)
  exist <- FALSE
  if (nrow(enrichmentResult) > 0){
    exist <- TRUE
  } else
    renderWarning(paste0(
      "Execute ", enrichmentType, " enrichment analysis 
      with ", enrichmentTool, " first."
    ))
  return(exist)
}

isSourceNotNull <- function(sourceSelect) {
  isNotNull <- FALSE
  if (!is.null(sourceSelect)){
    isNotNull <- TRUE
  } else
    renderWarning("Select at least one datasource.")
  return(isNotNull)
}

filterAndPrintTable <- function(enrichmentType, enrichmentTool,
                                outputId, sourceSelect, mode, slider,
                                filter = 'top') {
  enrichmentFilteredData <-
    filterTopData(enrichmentType, enrichmentTool, sourceSelect, mode, slider)

  # Convert Source to factor for dropdown filtering (like Results tab)
  enrichmentFilteredData$Source <- as.factor(enrichmentFilteredData$Source)

  # Extract type_Tool and plotId from outputId
  # outputId format: "{type_Tool}_{plotId}" e.g., "Functional_gProfiler_barchart"
  type_Tool <- paste(enrichmentType, enrichmentTool, sep = "_")
  plotId <- gsub(paste0(type_Tool, "_"), "", outputId)

  # Store as ORIGINAL data (this is called from "Generate" button)
  # This sets both original and current view to the same data
  setOriginalData(type_Tool, plotId, enrichmentFilteredData)

  renderEnrichmentTable(
    shinyOutputId = paste0(outputId, "_table"),
    enrichmentFilteredData,
    caption = "Enrichment Results",
    fileName = paste(outputId, paste(sourceSelect, collapse = "_"), sep = "_"),
    mode = "Positive Hits",
    hiddenColumns = c(0, 11, 12),
    expandableColumn = 11,
    filter = filter
  )
  return(enrichmentFilteredData)
}

filterTopData <- function(enrichmentType, enrichmentTool,
                          sourceSelect, mode, slider) {
  enrichmentResult <- getGlobalEnrichmentResult(enrichmentType, enrichmentTool)
  filteredData <- subset(
    enrichmentResult,
    Source %in% sourceSelect
  )
  if (mode == "Enrichment Score") {
    filteredData <-
      filteredData[order(-filteredData$`Enrichment Score %`), ]
  } # else already sorted by descending -log10Pvalue
  filteredData <- head(filteredData, slider)
  filteredData$`Positive Hits` <-
    gsub(",", ", ", filteredData$`Positive Hits`)
  return(filteredData)
}

separateRows <- function(enrichmentData) {
  enrichmentData <- enrichmentData[, c(
    "Source", "Term_ID", "Term_ID_noLinks", "Function", "Positive Hits",
    "Enrichment Score %", "-log10Pvalue", "Intersection Size")]
  enrichmentData <-
    tidyr::separate_rows(enrichmentData, `Positive Hits`, sep = ", ")
  return(enrichmentData)
}

calculatePlotHeight <- function(entriesCount) {
  height <- entriesCount * SINGLE_BAR_HEIGHT_PX + MIN_BAR_HEIGHT_PX
  return(height)
}

extractFunctionVsFunctionEdgelist <- function(enrichmentType, enrichmentTool,
                                              enrichmentData,
                                              thresholdSlider = NULL,
                                              simplifyForNetwork = FALSE) {
  functionsEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
  totalGenesEdgelist <- calculateEdgeTotalGenes(functionsEdgelist)
  commonGenesEdgelist <- calculateEdgeCommonGenes(functionsEdgelist)
  functionsEdgelist <- merge(
    commonGenesEdgelist, totalGenesEdgelist,
    by = c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
  )
  functionsEdgelist <- calculateSimilarityScore(functionsEdgelist)
  weightColumn <- "Similarity Score %"
  if (simplifyForNetwork) {
    functionsEdgelist <-
      removeDuplicateSelfAndOppositeEdges(functionsEdgelist, weightColumn)
  } else {
    functionsEdgelist <- tuneForHeatmap(functionsEdgelist)
  }
  if (!is.null(thresholdSlider)) {
    functionsEdgelist <- filterBySliderThreshold(functionsEdgelist,
                                                 weightColumn,
                                                 thresholdSlider)
  }
  functionsEdgelist <- appendSourceDatabasesAndIds(enrichmentType, enrichmentTool, functionsEdgelist)
  functionsEdgelist <- functionsEdgelist[order(-functionsEdgelist$`Similarity Score %`), ]
  return(functionsEdgelist)
}

calculateEdgeTotalGenes <- function(totalGenesEdgelist) {
  totalGenesEdgelistCopy <- totalGenesEdgelist
  colnames(totalGenesEdgelistCopy) <- c("TermsCopy", "HitsCopy")
  totalGenesEdgelist <- merge(totalGenesEdgelist , totalGenesEdgelistCopy)
  totalGenesEdgelist$`Positive Hits` <-
    paste(totalGenesEdgelist$`Positive Hits`,
          totalGenesEdgelist$HitsCopy,
          sep = ", ")
  totalGenesEdgelist$HitsCopy <- NULL
  totalGenesEdgelist <-
    tidyr::separate_rows(totalGenesEdgelist, `Positive Hits`, sep = ", ")
  totalGenesEdgelist <- dplyr::distinct(totalGenesEdgelist)
  totalGenesEdgelist$`Positive Hits` <- NULL
  totalGenesEdgelist <- data.table::setDT(
    totalGenesEdgelist)[, list(`Total Genes` = .N), names(totalGenesEdgelist)]
  colnames(totalGenesEdgelist)[1:2] <- c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
  return(totalGenesEdgelist)
}

calculateEdgeCommonGenes <- function(commonGenesEdgelist) {
  commonGenesEdgelist <-
    tidyr::separate_rows(commonGenesEdgelist, `Positive Hits`, sep = ", ")
  commonGenesEdgelist <- merge(
    commonGenesEdgelist, commonGenesEdgelist,
    by.x = "Positive Hits", by.y = "Positive Hits"
  )
  commonGenesEdgelist$`Positive Hits` <- NULL
  # Create common genes counts column
  commonGenesEdgelist <- data.table::setDT(
    commonGenesEdgelist)[, list(`Common Genes` = .N), names(commonGenesEdgelist)]
  return(commonGenesEdgelist)
}

calculateSimilarityScore <- function(functionsEdgelist) {
  functionsEdgelist$`Similarity Score %` <-
    functionsEdgelist$`Common Genes` / functionsEdgelist$`Total Genes` * 100
  functionsEdgelist$`Similarity Score %` <-
    format(round(functionsEdgelist$`Similarity Score %`, 2))
  return(functionsEdgelist)
}

removeDuplicateSelfAndOppositeEdges <- function(networkEdgelist, weightColumn) {
  graph <- igraph::graph_from_data_frame(networkEdgelist, directed = FALSE)
  igraph::E(graph)$weight <- networkEdgelist[[weightColumn]]
  graph <- igraph::simplify(
    graph,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = "first"
  )
  networkEdgelist <- appendEdgelistColumns(graph, weightColumn)
  networkEdgelist[[weightColumn]] <-
    as.numeric(networkEdgelist[[weightColumn]])
  return(networkEdgelist)
}

appendEdgelistColumns <- function(graph, weightColumn) {
  if (weightColumn == "Similarity Score %") {
    graphEdgelist <- as.data.frame(
      cbind(
        igraph::get.edgelist(graph),
        igraph::E(graph)$`Common Genes`,
        igraph::E(graph)$`Total Genes`,
        igraph::E(graph)$weight
      )
    )
    colnames(graphEdgelist) <-
      c("Source Node", "Target Node",
        "Common Genes", "Total Genes", "Similarity Score %")
  } else if (weightColumn == "Common Functions") {
    graphEdgelist <- as.data.frame(
      cbind(
        igraph::get.edgelist(graph),
        igraph::E(graph)$weight
      )
    )
    colnames(graphEdgelist) <-
      c("Source Node", "Target Node", "Common Functions")
  }
  return(graphEdgelist)
}

tuneForHeatmap <- function(functionsEdgelist) {
  functionsEdgelist$`Similarity Score %` <- as.numeric(functionsEdgelist$`Similarity Score %`)
  colnames(functionsEdgelist) <-
    c("Source Node", "Target Node",
      "Common Genes", "Total Genes", "Similarity Score %")
  return(functionsEdgelist)
}

filterBySliderThreshold <- function(edgelist, weightColumn, thresholdSlider) {
  edgelist <-
    edgelist[
      edgelist[[weightColumn]] >= thresholdSlider, , drop = FALSE
    ]
  return(edgelist)
}

appendSourceDatabasesAndIds <- function(enrichmentType, enrichmentTool, functionsEdgelist) {
  enrichedNetworkData <- getGlobalEnrichmentResult(enrichmentType, enrichmentTool)
  enrichedNetworkData <- enrichedNetworkData[, c(
    "Source", "Term_ID_noLinks", "Function")]
  functionsEdgelist <- merge(functionsEdgelist, enrichedNetworkData,
                             by.x = "Source Node", by.y = "Term_ID_noLinks")
  functionsEdgelist <- merge(functionsEdgelist, enrichedNetworkData,
                             by.x = "Target Node", by.y = "Term_ID_noLinks")
  colnames(functionsEdgelist) <-
    c("Target Id", "Source Id", "Common Genes", "Total Genes",
      "Similarity Score %", "Source Database", "Source Name",
      "Target Database", "Target Name")
  functionsEdgelist <-
    functionsEdgelist[, c(
      "Source Database", "Source Id", "Source Name",
      "Target Database", "Target Id", "Target Name",
      "Common Genes", "Total Genes", "Similarity Score %"
    )]

  # Convert column types for proper DT filters
  # Factor columns get dropdown filters, numeric columns get slider filters
  functionsEdgelist$`Source Database` <- as.factor(functionsEdgelist$`Source Database`)
  functionsEdgelist$`Target Database` <- as.factor(functionsEdgelist$`Target Database`)
  functionsEdgelist$`Common Genes` <- as.numeric(functionsEdgelist$`Common Genes`)
  functionsEdgelist$`Total Genes` <- as.numeric(functionsEdgelist$`Total Genes`)
  functionsEdgelist$`Similarity Score %` <- as.numeric(functionsEdgelist$`Similarity Score %`)

  return(functionsEdgelist)
}

extractGeneVsGeneEdgelist <- function(enrichmentData, thresholdSlider = NULL,
                                      simplifyForNetwork = FALSE) {
  genesEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
  genesEdgelist <-
    tidyr::separate_rows(genesEdgelist, `Positive Hits`, sep = ", ")
  genesEdgelist <- merge(genesEdgelist, genesEdgelist,
                         by.x = "Term_ID_noLinks", by.y = "Term_ID_noLinks")
  genesEdgelist$`Term_ID_noLinks` <- NULL
  # Create common functions counts column
  genesEdgelist <- data.table::setDT(
    genesEdgelist)[, list(`Common Functions` = .N), names(genesEdgelist)]
  weightColumn <- "Common Functions"
  if (simplifyForNetwork) {
    genesEdgelist <-
      removeDuplicateSelfAndOppositeEdges(genesEdgelist, weightColumn)
  }
  if (!is.null(thresholdSlider)) {
    genesEdgelist <- filterBySliderThreshold(genesEdgelist,
                                             weightColumn,
                                             thresholdSlider)
  }
  colnames(genesEdgelist) <- c("Source Name", "Target Name", "Common Functions")
  return(genesEdgelist)
}

# Plot-Table Synchronization Functions
# Plot and table always show the SAME data. Compare actual Term_IDs to detect real changes.

# Handle plot click events - filter TABLE to show clicked term (plot unchanged)
handlePlotClick <- function(plotId, plotSource) {
  tryCatch({
    type_Tool <- currentType_Tool
    if (is.null(type_Tool) || type_Tool == "") return()

    clickData <- event_data("plotly_click", source = plotSource)
    if (is.null(clickData)) return()

    # All heatmaps: click filters TABLE only, plot stays unchanged
    # Filter from ORIGINAL data so each click is independent
    if (plotId %in% c("heatmap1", "heatmap2", "heatmap3")) {
      originalData <- getOriginalData(type_Tool, plotId)
      if (is.null(originalData) || nrow(originalData) == 0) return()

      selectedData <- filterDataByHeatmapClick(originalData, clickData, plotId, type_Tool)
      if (!is.null(selectedData) && nrow(selectedData) > 0) {
        updateCurrentView(type_Tool, plotId, selectedData, "plot_click")
        renderTableFromCurrentView(type_Tool, plotId)
      }
      return()
    }

    # Other plots (barchart, scatter, dot): check feedback loop prevention
    if (!shouldProcessUpdate(type_Tool, plotId, "plot")) {
      return()
    }

    originalData <- getOriginalData(type_Tool, plotId)
    if (is.null(originalData) || nrow(originalData) == 0) return()

    clickedTermId <- clickData$customdata
    if (is.null(clickedTermId) || length(clickedTermId) == 0) {
      clickedTermId <- clickData$y
    }
    if (is.null(clickedTermId)) return()

    # Safe column matching against original data
    matchVector <- rep(FALSE, nrow(originalData))
    if ("Term_ID_noLinks" %in% names(originalData)) {
      matchVector <- matchVector | (originalData$Term_ID_noLinks == clickedTermId)
    }
    if ("Function" %in% names(originalData)) {
      matchVector <- matchVector | (originalData$Function == clickedTermId)
    }
    if ("Term_ID" %in% names(originalData)) {
      matchVector <- matchVector | (originalData$Term_ID == clickedTermId)
    }
    matchVector[is.na(matchVector)] <- FALSE
    selectedData <- originalData[matchVector, , drop = FALSE]

    if (!is.null(selectedData) && nrow(selectedData) > 0) {
      updateCurrentView(type_Tool, plotId, selectedData, "plot_click")
      # Only update table, don't re-render plot
      renderTableFromCurrentView(type_Tool, plotId)
    }
  }, error = function(e) {
    message("Plot click error: ", e$message)
  })
}

# Filter enrichment data based on heatmap click coordinates
filterDataByHeatmapClick <- function(currentData, clickData, plotId, type_Tool) {
  yValue <- clickData$y
  xValue <- clickData$x

  matchVector <- rep(FALSE, nrow(currentData))

  if (plotId == "heatmap1") {
    # For heatmap1, y or x could be term depending on axis setting
    # Try to match against term columns first
    if ("Term_ID_noLinks" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$Term_ID_noLinks == yValue)
      matchVector <- matchVector | (currentData$Term_ID_noLinks == xValue)
    }
    if ("Function" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$Function == yValue)
      matchVector <- matchVector | (currentData$Function == xValue)
    }
    if ("Term_ID" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$Term_ID == yValue)
      matchVector <- matchVector | (currentData$Term_ID == xValue)
    }

  } else if (plotId == "heatmap2") {
    # For heatmap2, both axes are terms (source and target)
    # Match either - typically user clicks to see that specific term
    if ("Term_ID_noLinks" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$Term_ID_noLinks == yValue)
      matchVector <- matchVector | (currentData$Term_ID_noLinks == xValue)
    }
    if ("Function" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$Function == yValue)
      matchVector <- matchVector | (currentData$Function == xValue)
    }
    # Also check with "Source"/"Target" prefixes in case labels are from edgelist
    if ("Source Id" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$`Source Id` == yValue)
    }
    if ("Source Name" %in% names(currentData)) {
      matchVector <- matchVector | (currentData$`Source Name` == yValue)
    }

  } else if (plotId == "heatmap3") {
    # For heatmap3, axes are genes - filter to terms containing BOTH genes
    # The cell represents shared functions, so show terms that explain the connection
    if ("Positive Hits" %in% names(currentData)) {
      for (i in seq_len(nrow(currentData))) {
        genes <- unlist(strsplit(as.character(currentData$`Positive Hits`[i]), ",\\s*"))
        if (yValue %in% genes && xValue %in% genes) {
          matchVector[i] <- TRUE
        }
      }
    }
  }

  matchVector[is.na(matchVector)] <- FALSE
  return(currentData[matchVector, , drop = FALSE])
}

# Handle plot zoom events - filter table to visible terms
handlePlotZoom <- function(plotId, plotSource) {
  tryCatch({
    type_Tool <- currentType_Tool
    if (is.null(type_Tool) || type_Tool == "") return()

    relayoutData <- event_data("plotly_relayout", source = plotSource)
    if (is.null(relayoutData)) return()

    isResetEvent <- any(grepl("autorange", names(relayoutData)))
    isZoomEvent <- any(grepl("range", names(relayoutData)))

    # Always process reset (autoscale), only check feedback loop for zoom
    if (!isResetEvent && !shouldProcessUpdate(type_Tool, plotId, "plot")) {
      return()
    }

    if (isResetEvent) {
      # Autoscale (double-click): restore original data
      originalData <- getOriginalData(type_Tool, plotId)
      if (!is.null(originalData) && nrow(originalData) > 0) {
        updateCurrentView(type_Tool, plotId, originalData, "reset")
        renderTableFromCurrentView(type_Tool, plotId)
      }
    } else if (isZoomEvent) {
      # Heatmap3: zoom is visual only, don't update table
      if (plotId == "heatmap3") return()

      originalData <- getOriginalData(type_Tool, plotId)
      if (is.null(originalData) || nrow(originalData) == 0) return()

      filteredData <- filterDataByZoomBounds(originalData, relayoutData, plotId)
      if (nrow(filteredData) > 0) {
        updateCurrentView(type_Tool, plotId, filteredData, "plot_zoom")
        renderTableFromCurrentView(type_Tool, plotId)
      }
    }
  }, error = function(e) {
    message("Plot zoom error: ", e$message)
  })
}

# Handle plot selection events (lasso/box) - filter TABLE only
handlePlotSelection <- function(plotId, plotSource) {
  tryCatch({
    type_Tool <- currentType_Tool
    if (is.null(type_Tool) || type_Tool == "") return()

    # Heatmaps: selection doesn't apply well (use click for cells)
    if (plotId %in% c("heatmap1", "heatmap2", "heatmap3")) return()

    if (!shouldProcessUpdate(type_Tool, plotId, "plot")) {
      return()
    }

    # Filter from original data so each selection is independent
    originalData <- getOriginalData(type_Tool, plotId)
    if (is.null(originalData) || nrow(originalData) == 0) return()

    selectionData <- event_data("plotly_selected", source = plotSource)
    if (is.null(selectionData) || nrow(selectionData) == 0) return()

    selectedTermIds <- selectionData$customdata
    if (is.null(selectedTermIds) || length(selectedTermIds) == 0) {
      selectedTermIds <- selectionData$y
    }
    if (is.null(selectedTermIds) || length(selectedTermIds) == 0) return()

    # Match against original data
    matchVector <- rep(FALSE, nrow(originalData))

    if ("Term_ID_noLinks" %in% names(originalData)) {
      matchVector <- matchVector | (originalData$Term_ID_noLinks %in% selectedTermIds)
    }
    if ("Function" %in% names(originalData)) {
      matchVector <- matchVector | (originalData$Function %in% selectedTermIds)
    }
    if ("Term_ID" %in% names(originalData)) {
      matchVector <- matchVector | (originalData$Term_ID %in% selectedTermIds)
    }

    matchVector[is.na(matchVector)] <- FALSE
    selectedData <- originalData[matchVector, , drop = FALSE]

    if (nrow(selectedData) > 0) {
      updateCurrentView(type_Tool, plotId, selectedData, "plot_select")
      # Only update table, don't re-render plot
      renderTableFromCurrentView(type_Tool, plotId)
    }
  }, error = function(e) {
    message("Plot selection error: ", e$message)
  })
}

# Filter data based on plotly zoom bounds
filterDataByZoomBounds <- function(data, relayoutData, plotId) {
  # Extract axis ranges from relayout event
  xMin <- relayoutData$`xaxis.range[0]`
  xMax <- relayoutData$`xaxis.range[1]`
  yMin <- relayoutData$`yaxis.range[0]`
  yMax <- relayoutData$`yaxis.range[1]`

  filteredData <- data
  type_Tool <- currentType_Tool

  if (plotId == "barchart") {
    # Barchart: x-axis is the value column, y-axis is categorical
    # Get mode and drawFormat to reconstruct the correct y-axis order
    mode <- input[[paste(type_Tool, "barchart_mode", sep = "_")]]
    drawFormatColumn <- input[[paste(type_Tool, "barchart_drawFormat", sep = "_")]]

    column <- switch(mode,
      "Enrichment Score" = "Enrichment Score %",
      "-log10Pvalue"
    )

    # Reconstruct factor levels in same order as plot (ascending by value, so top = highest)
    factorLevels <- unique(data[[drawFormatColumn]])[order(data[[column]], decreasing = FALSE)]

    # Filter by visible y categories
    if (!is.null(yMin) && !is.null(yMax)) {
      visibleIndices <- ceiling(yMin):floor(yMax)
      visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(factorLevels)]
      if (length(visibleIndices) > 0) {
        # Get the actual category values at these positions (1-indexed for R)
        visibleCategories <- factorLevels[visibleIndices + 1]
        filteredData <- data[data[[drawFormatColumn]] %in% visibleCategories, , drop = FALSE]
      }
    }

  } else if (plotId == "scatterPlot") {
    # Scatter: both axes are numeric
    if (!is.null(xMin) && !is.null(xMax)) {
      filteredData <- filteredData[
        filteredData$`-log10Pvalue` >= xMin &
        filteredData$`-log10Pvalue` <= xMax, , drop = FALSE]
    }
    if (!is.null(yMin) && !is.null(yMax)) {
      filteredData <- filteredData[
        filteredData$`Enrichment Score %` >= yMin &
        filteredData$`Enrichment Score %` <= yMax, , drop = FALSE]
    }

  } else if (plotId == "dotPlot") {
    # DotPlot: x = Gene Ratio (numeric), y = categorical term names
    # Get mode and drawFormat to reconstruct the correct y-axis order
    mode <- input[[paste(type_Tool, "dotPlot_mode", sep = "_")]]
    drawFormatColumn <- input[[paste(type_Tool, "dotPlot_drawFormat", sep = "_")]]

    # Need Gene Ratio for ordering
    dataCopy <- data
    if (!"Gene Ratio" %in% names(dataCopy)) {
      dataCopy$`Gene Ratio` <- dataCopy$`Intersection Size` / dataCopy$`Query size`
    }

    orderColumn <- switch(mode,
      "Enrichment Score" = "Enrichment Score %",
      "Gene Ratio" = "Gene Ratio",
      "-log10Pvalue"
    )

    # Reconstruct factor levels in same order as plot
    factorLevels <- unique(dataCopy[[drawFormatColumn]])[order(dataCopy[[orderColumn]], decreasing = FALSE)]

    # Filter by visible y categories
    if (!is.null(yMin) && !is.null(yMax)) {
      visibleIndices <- ceiling(yMin):floor(yMax)
      visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(factorLevels)]
      if (length(visibleIndices) > 0) {
        visibleCategories <- factorLevels[visibleIndices + 1]
        filteredData <- data[data[[drawFormatColumn]] %in% visibleCategories, , drop = FALSE]
      }
    }

    # Also filter by x-axis (Gene Ratio) if zoomed
    if (!is.null(xMin) && !is.null(xMax)) {
      if (!"Gene Ratio" %in% names(filteredData)) {
        filteredData$`Gene Ratio` <- filteredData$`Intersection Size` / filteredData$`Query size`
      }
      filteredData <- filteredData[
        filteredData$`Gene Ratio` >= xMin &
        filteredData$`Gene Ratio` <= xMax, , drop = FALSE]
    }

  } else if (plotId == "heatmap1") {
    # Heatmap1: one axis is terms, other is genes
    # Get axis setting to determine which is which
    enrichmentType <- strsplit(type_Tool, "_")[[1]][1]
    uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])
    heatmap1_axis <- input[[paste(type_Tool, "heatmap1_axis", sep = "_")]]
    drawFormatColumn <- input[[paste(type_Tool, "heatmap1_drawFormat", sep = "_")]]

    # Transform data to heatmap format to get correct axis labels
    heatmapTable <- separateRows(data)

    if (heatmap1_axis == paste0(uiTermKeyword, "-Genes")) {
      # Y-axis is terms, X-axis is genes
      # Y-categories are reversed to show highest values at top (matching barchart)
      if (!is.null(yMin) && !is.null(yMax)) {
        termLevels <- rev(unique(heatmapTable[[drawFormatColumn]]))
        visibleIndices <- ceiling(yMin):floor(yMax)
        visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(termLevels)]
        if (length(visibleIndices) > 0) {
          visibleTerms <- termLevels[visibleIndices + 1]
          filteredData <- data[data[[drawFormatColumn]] %in% visibleTerms, , drop = FALSE]
        }
      }
    } else {
      # X-axis is terms, Y-axis is genes
      if (!is.null(xMin) && !is.null(xMax)) {
        termLevels <- unique(heatmapTable[[drawFormatColumn]])
        visibleIndices <- ceiling(xMin):floor(xMax)
        visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(termLevels)]
        if (length(visibleIndices) > 0) {
          visibleTerms <- termLevels[visibleIndices + 1]
          filteredData <- data[data[[drawFormatColumn]] %in% visibleTerms, , drop = FALSE]
        }
      }
    }

  } else if (plotId == "heatmap2") {
    # Heatmap2: both axes are terms (source/target)
    # Need to include terms from BOTH x and y axes
    drawFormatColumn <- switch(
      input[[paste(type_Tool, "heatmap2_drawFormat", sep = "_")]],
      "Term_ID" = "Id",
      "Function" = "Name"
    )

    # Extract enrichment type and tool for the edgelist function
    parts <- strsplit(type_Tool, "_")[[1]]
    enrichmentType <- parts[1]
    enrichmentTool <- parts[2]

    # Transform to get heatmap axis labels
    heatmapTable <- extractFunctionVsFunctionEdgelist(enrichmentType, enrichmentTool, data)
    if (!is.null(heatmapTable) && nrow(heatmapTable) > 0) {
      yAxisColumn <- paste0("Source ", drawFormatColumn)
      xAxisColumn <- paste0("Target ", drawFormatColumn)

      visibleTerms <- c()

      # Filter by Y-axis (source terms) visible range
      # Plotly y-axis categories are REVERSED (to show highest at top)
      if (!is.null(yMin) && !is.null(yMax)) {
        yTermLevels <- rev(unique(heatmapTable[[yAxisColumn]]))
        visibleIndices <- ceiling(yMin):floor(yMax)
        visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(yTermLevels)]
        if (length(visibleIndices) > 0) {
          visibleTerms <- c(visibleTerms, yTermLevels[visibleIndices + 1])
        }
      }

      # Also filter by X-axis (target terms) visible range
      if (!is.null(xMin) && !is.null(xMax)) {
        xTermLevels <- unique(heatmapTable[[xAxisColumn]])
        visibleIndices <- ceiling(xMin):floor(xMax)
        visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(xTermLevels)]
        if (length(visibleIndices) > 0) {
          visibleTerms <- c(visibleTerms, xTermLevels[visibleIndices + 1])
        }
      }

      # Get unique terms from both axes
      visibleTerms <- unique(visibleTerms)

      if (length(visibleTerms) > 0) {
        # Map back to original data column
        dataColumn <- switch(
          input[[paste(type_Tool, "heatmap2_drawFormat", sep = "_")]],
          "Term_ID" = "Term_ID_noLinks",
          "Function"
        )
        filteredData <- data[data[[dataColumn]] %in% visibleTerms, , drop = FALSE]
      }
    }

  } else if (plotId == "heatmap3") {
    # Heatmap3: both axes are genes - filter to terms involving visible genes
    # Transform to get heatmap axis labels
    heatmapTable <- extractGeneVsGeneEdgelist(data)
    if (!is.null(heatmapTable) && nrow(heatmapTable) > 0) {
      # Filter by Y-axis (source genes) visible range
      # Plotly y-axis categories are REVERSED (to show highest at top, matching barchart)
      # So we need to use the same reversed order here
      if (!is.null(yMin) && !is.null(yMax)) {
        geneLevels <- rev(unique(heatmapTable$`Source Name`))
        visibleIndices <- ceiling(yMin):floor(yMax)
        visibleIndices <- visibleIndices[visibleIndices >= 0 & visibleIndices < length(geneLevels)]
        if (length(visibleIndices) > 0) {
          visibleGenes <- geneLevels[visibleIndices + 1]
          # Filter to terms that contain at least one visible gene
          matchVector <- sapply(data$`Positive Hits`, function(hits) {
            genes <- unlist(strsplit(as.character(hits), ",\\s*"))
            any(genes %in% visibleGenes)
          })
          filteredData <- data[matchVector, , drop = FALSE]
        }
      }
    }
  }

  return(filteredData)
}

# Render helper functions
# These functions render plot/table from the current view data

# Render only the table from current view
renderTableFromCurrentView <- function(type_Tool, plotId) {
  data <- getCurrentView(type_Tool, plotId)
  if (is.null(data) || nrow(data) == 0) return()

  tableOutputId <- paste(type_Tool, plotId, "table", sep = "_")

  # Mark this as a programmatic update so handleTableFilterChange skips plot re-render
  markTableUpdatePending(tableOutputId)

  # Ensure Source is a factor for dropdown filtering
  data$Source <- as.factor(data$Source)

  renderEnrichmentTable(
    shinyOutputId = tableOutputId,
    input_table = data,
    caption = "Enrichment Results",
    fileName = paste(type_Tool, plotId, sep = "_"),
    mode = "Positive Hits",
    hiddenColumns = c(0, 11, 12),
    expandableColumn = 11,
    filter = 'top'
  )
}

# Render table with specific data (without updating currentView)
renderTableWithData <- function(type_Tool, plotId, data) {
  if (is.null(data) || nrow(data) == 0) return()

  tableOutputId <- paste(type_Tool, plotId, "table", sep = "_")
  data$Source <- as.factor(data$Source)

  renderEnrichmentTable(
    shinyOutputId = tableOutputId,
    input_table = data,
    caption = "Enrichment Results",
    fileName = paste(type_Tool, plotId, sep = "_"),
    mode = "Positive Hits",
    hiddenColumns = c(0, 11, 12),
    expandableColumn = 11,
    filter = 'top'
  )
}

# Render only the plot from current view
renderPlotFromCurrentView <- function(type_Tool, plotId) {
  data <- getCurrentView(type_Tool, plotId)
  if (is.null(data) || nrow(data) == 0) return()

  plotOutputId <- paste(type_Tool, plotId, sep = "_")

  if (plotId == "barchart") {
    mode <- input[[paste(type_Tool, "barchart_mode", sep = "_")]]
    column <- switch(mode,
      "Enrichment Score" = "Enrichment Score %",
      "-log10Pvalue"
    )
    drawFormatColumn <- input[[paste(type_Tool, "barchart_drawFormat", sep = "_")]]

    # Re-order data for barchart display
    data <- orderForBarchartByColumn(data, column, drawFormatColumn)
    height <- calculatePlotHeight(nrow(data))

    renderBarchart(plotOutputId, data, column, drawFormatColumn, height)

  } else if (plotId == "scatterPlot") {
    # Add jitter for scatter plot
    data <- addJitter(data)

    renderScatterPlot(plotOutputId, data)

  } else if (plotId == "dotPlot") {
    # Calculate Gene Ratio if not present
    if (!"Gene Ratio" %in% names(data)) {
      data$`Gene Ratio` <- data$`Intersection Size` / data$`Query size`
    }

    mode <- input[[paste(type_Tool, "dotPlot_mode", sep = "_")]]
    drawFormatColumn <- input[[paste(type_Tool, "dotPlot_drawFormat", sep = "_")]]

    # Re-order data for dot plot display
    data <- orderForDotPlot(data, mode, drawFormatColumn)
    height <- nrow(data) * DOTPLOT_ENTRY_HEIGHT_PX + MIN_BAR_HEIGHT_PX

    renderDotPlot(plotOutputId, data, drawFormatColumn, height)

  } else if (plotId == "heatmap1") {
    renderHeatmap1WithData(type_Tool, data)

  } else if (plotId == "heatmap2") {
    renderHeatmap2WithData(type_Tool, data)

  } else if (plotId == "heatmap3") {
    renderHeatmap3WithData(type_Tool, data)
  }
}

# Render BOTH plot and table from current view (used for reset/autoscale)
renderBothFromCurrentView <- function(type_Tool, plotId) {
  renderTableFromCurrentView(type_Tool, plotId)
  renderPlotFromCurrentView(type_Tool, plotId)
}

# Render plot with specific data (not from currentView)
# Used for DT filter display where we don't want to update state
renderPlotWithData <- function(type_Tool, plotId, data) {
  if (is.null(data) || nrow(data) == 0) return()

  plotOutputId <- paste(type_Tool, plotId, sep = "_")

  if (plotId == "barchart") {
    mode <- input[[paste(type_Tool, "barchart_mode", sep = "_")]]
    column <- switch(mode, "Enrichment Score" = "Enrichment Score %", "-log10Pvalue")
    drawFormatColumn <- input[[paste(type_Tool, "barchart_drawFormat", sep = "_")]]
    data <- orderForBarchartByColumn(data, column, drawFormatColumn)
    height <- calculatePlotHeight(nrow(data))
    renderBarchart(plotOutputId, data, column, drawFormatColumn, height)

  } else if (plotId == "scatterPlot") {
    data <- addJitter(data)
    renderScatterPlot(plotOutputId, data)

  } else if (plotId == "dotPlot") {
    if (!"Gene Ratio" %in% names(data)) {
      data$`Gene Ratio` <- data$`Intersection Size` / data$`Query size`
    }
    mode <- input[[paste(type_Tool, "dotPlot_mode", sep = "_")]]
    drawFormatColumn <- input[[paste(type_Tool, "dotPlot_drawFormat", sep = "_")]]
    data <- orderForDotPlot(data, mode, drawFormatColumn)
    height <- nrow(data) * DOTPLOT_ENTRY_HEIGHT_PX + MIN_BAR_HEIGHT_PX
    renderDotPlot(plotOutputId, data, drawFormatColumn, height)

  } else if (plotId == "heatmap1") {
    renderHeatmap1WithData(type_Tool, data)

  } else if (plotId == "heatmap2") {
    renderHeatmap2WithData(type_Tool, data)

  } else if (plotId == "heatmap3") {
    renderHeatmap3WithData(type_Tool, data)
  }
}

# Heatmap rendering helpers

# Render heatmap1 (Function vs Gene) with enrichment data
renderHeatmap1WithData <- function(type_Tool, enrichmentData) {
  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  # Transform to heatmap format
  heatmapTable <- separateRows(enrichmentData)
  heatmapTable$GeneExists <- 1

  # Get UI settings
  enrichmentType <- strsplit(type_Tool, "_")[[1]][1]
  uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])
  heatmap1_axis <- input[[paste(type_Tool, "heatmap1_axis", sep = "_")]]
  drawFormatColumn <- input[[paste(type_Tool, "heatmap1_drawFormat", sep = "_")]]

  if (heatmap1_axis == paste0(uiTermKeyword, "-Genes")) {
    yAxisColumn <- drawFormatColumn
    xAxisColumn <- "Positive Hits"
  } else {
    yAxisColumn <- "Positive Hits"
    xAxisColumn <- drawFormatColumn
  }

  entriesCount <- length(unique(heatmapTable[[yAxisColumn]]))
  height <- calculatePlotHeight(entriesCount)

  # Get color from source selection
  source <- input[[paste(type_Tool, "heatmap1_sourceSelect", sep = "_")]]
  color <- if (!is.null(source)) DATASOURCE_COLORS[[source]] else "#4682B4"

  renderHeatmap(type_Tool, "heatmap1", heatmapTable, color,
                yAxisColumn, xAxisColumn,
                weightColumn = "GeneExists",
                height = height,
                showColorbar = FALSE)
}

# Render heatmap2 (Function vs Function) with enrichment data
renderHeatmap2WithData <- function(type_Tool, enrichmentData) {
  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  # Extract enrichment type and tool from type_Tool
  parts <- strsplit(type_Tool, "_")[[1]]
  enrichmentType <- parts[1]
  enrichmentTool <- parts[2]

  # Transform to edgelist format
  heatmapTable <- extractFunctionVsFunctionEdgelist(enrichmentType, enrichmentTool,
                                                    enrichmentData)
  if (is.null(heatmapTable) || nrow(heatmapTable) == 0) return()

  # Get UI settings
  drawFormatColumn <- switch(
    input[[paste(type_Tool, "heatmap2_drawFormat", sep = "_")]],
    "Term_ID" = "Id",
    "Function" = "Name"
  )
  yAxisColumn <- paste0("Source ", drawFormatColumn)
  xAxisColumn <- paste0("Target ", drawFormatColumn)
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)

  # Get color from source selection
  source <- input[[paste(type_Tool, "heatmap2_sourceSelect", sep = "_")]]
  color <- if (!is.null(source)) DATASOURCE_COLORS[[source]] else "#4682B4"

  renderHeatmap(type_Tool, "heatmap2", heatmapTable, color,
                yAxisColumn = yAxisColumn,
                xAxisColumn = xAxisColumn,
                weightColumn = "Similarity Score %",
                height = height,
                colorbarTitle = "Similarity %")
}

# Render heatmap3 (Gene vs Gene) with enrichment data
renderHeatmap3WithData <- function(type_Tool, enrichmentData) {
  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  # Transform to edgelist format
  heatmapTable <- extractGeneVsGeneEdgelist(enrichmentData)
  if (is.null(heatmapTable) || nrow(heatmapTable) == 0) return()

  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)

  # Get color from source selection
  source <- input[[paste(type_Tool, "heatmap3_sourceSelect", sep = "_")]]
  color <- if (!is.null(source)) DATASOURCE_COLORS[[source]] else "#4682B4"

  renderHeatmap(type_Tool, "heatmap3", heatmapTable, color,
                yAxisColumn = "Source Name",
                xAxisColumn = "Target Name",
                weightColumn = "Common Functions",
                height = height,
                colorbarTitle = "Common Functions")
}

# Table filter handler

# Handle table filter changes - update plot with filtered data
# DT uses client-side filtering, so we render from filtered indices
# without modifying currentView (keeps state in sync with DT's underlying data).
handleTableFilterChange <- function(enrichmentType, toolName, plotId) {
  tryCatch({
    type_Tool <- paste(enrichmentType, toolName, sep = "_")

    # Skip if last update was from plot (prevents feedback loop within 500ms)
    if (!shouldProcessUpdate(type_Tool, plotId, "table")) {
      return()
    }

    tableId <- paste(type_Tool, plotId, "table", sep = "_")

    # Get current view (what the DT was rendered with)
    currentData <- getCurrentView(type_Tool, plotId)
    if (is.null(currentData) || nrow(currentData) == 0) return()

    # Get filtered row indices from DT
    filteredRowIndices <- input[[paste0(tableId, "_rows_all")]]
    if (is.null(filteredRowIndices) || length(filteredRowIndices) == 0) return()

    # Safety check: indices must be valid for DT's data (currentView)
    if (max(filteredRowIndices) > nrow(currentData)) return()

    # Check if this is from a programmatic table update (e.g., after zoom/click)
    # If so, skip plot re-render - the plot is already in the correct state
    if (isProgrammaticTableUpdate(tableId)) {
      return()
    }

    # If all rows are visible, user cleared a filter - restore plot to currentView
    if (length(filteredRowIndices) == nrow(currentData)) {
      setUpdateSource(type_Tool, plotId, "table_filter")
      renderPlotFromCurrentView(type_Tool, plotId)
      return()
    }

    # Filter is active - render plot with visible terms only (no state update)
    filteredData <- currentData[filteredRowIndices, , drop = FALSE]

    if (nrow(filteredData) == 0) return()

    setUpdateSource(type_Tool, plotId, "table_filter")
    renderPlotWithData(type_Tool, plotId, filteredData)
  }, error = function(e) {
    message("Table filter error: ", e$message)
  })
}

# Order data for barchart by specified column
orderForBarchartByColumn <- function(data, column, drawFormatColumn) {
  data[[drawFormatColumn]] <-
    factor(
      data[[drawFormatColumn]],
      levels = unique(data[[drawFormatColumn]])[order(
        data[[column]], decreasing = FALSE)])
  return(data)
}

# Order data for dot plot by mode
orderForDotPlot <- function(data, mode, drawFormatColumn) {
  if (mode == "Enrichment Score") {
    data <- data[order(-data$`Enrichment Score %`), ]
  } else if (mode == "Gene Ratio") {
    data <- data[order(-data$`Gene Ratio`), ]
  }
  # else already sorted by -log10Pvalue

  data[[drawFormatColumn]] <-
    factor(
      data[[drawFormatColumn]],
      levels = unique(data[[drawFormatColumn]])[order(
        data$`-log10Pvalue`, decreasing = FALSE)])
  return(data)
}

# Handle Reset View button - restore ORIGINAL data to both plot and table
handleResetView <- function(enrichmentType, toolName, plotId) {
  tryCatch({
    type_Tool <- paste(enrichmentType, toolName, sep = "_")

    # Get ORIGINAL data (from Generate button, immutable)
    originalData <- getOriginalData(type_Tool, plotId)
    if (is.null(originalData) || nrow(originalData) == 0) {
      showNotification("No data available. Please generate the plot first.",
                       type = "warning", duration = 3)
      return()
    }

    # Restore original data to current view with "reset" source tracking
    updateCurrentView(type_Tool, plotId, originalData, "reset")

    # Re-render BOTH table and plot from current view
    renderBothFromCurrentView(type_Tool, plotId)
  }, error = function(e) {
    message("Reset view error: ", e$message)
    showNotification("Could not reset view.", type = "error", duration = 3)
  })
}
