# Plot-Table Synchronization State Management
#
# Maintains separate original/current data stores with source-aware update
# tracking to prevent feedback loops between plots and tables.

initializePlotTableState <- function() {
  # Original data set on Generate, used for Reset View
  plotOriginalData <<- reactiveValues()

  # Current view data that both plot and table render from
  plotCurrentView <<- reactiveValues()

  # Data currently rendered on the plot (may differ from currentView after filtering)
  # Used for highlighting calculations to match actual plot state
  plotRenderedData <<- reactiveValues()

  # Tracks update source and timing to prevent feedback loops
  plotUpdateTracker <<- reactiveValues()

  # Current network view for restoring tables on deselect
  currentNetworkView <<- reactiveValues()

  # Expected row counts for cascade detection (simple list, not reactive)
  expectedRowCounts <<- list()

  # Per-plot selection state for click-based term selection
  selectedTermIds <<- reactiveValues()

  # Pair-level selection for heatmap2/heatmap3 (Functions vs Functions, Genes vs Genes)
  # Stores data.frames with columns: source, target
  selectedPairs <<- reactiveValues()

  # Heatmap cell selection for visual highlighting
  # Stores data.frames with columns: x, y (cell coordinates)
  selectedHeatmapCells <<- reactiveValues()
}

# Timestamps for tracking programmatic table updates (not reactiveValues)
# Stores the time of last programmatic update per table
# Events within 200ms of a programmatic update are blocked as cascades
programmaticTableUpdateTimes <- list()

getStateKey <- function(type_Tool, plotId) {
  paste(type_Tool, plotId, sep = "_")
}


# Original data functions (immutable after Generate)

setOriginalData <- function(type_Tool, plotId, data) {
  key <- getStateKey(type_Tool, plotId)
  plotOriginalData[[key]] <<- data
  plotCurrentView[[key]] <<- data
  plotRenderedData[[key]] <<- data  # Initially, rendered data = original data
  plotUpdateTracker[[key]] <<- list(counter = 0, source = "generate", time = Sys.time())
  # Clear any existing selection when new data is generated
  selectedTermIds[[key]] <<- NULL
}

getOriginalData <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  return(plotOriginalData[[key]])
}

# Rendered data functions - tracks what's actually displayed on the plot
# This may differ from currentView after table filtering

setRenderedData <- function(type_Tool, plotId, data) {
  key <- getStateKey(type_Tool, plotId)
  plotRenderedData[[key]] <<- data
}

getRenderedData <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  data <- plotRenderedData[[key]]
  # Fallback to originalData if not set (shouldn't happen normally)
  if (is.null(data)) {
    return(getOriginalData(type_Tool, plotId))
  }
  return(data)
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


# Prevents feedback loops between plot and table updates.
# Zoom is visual-only; click/selection events use "selection" source.
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

  # Block table filter events triggered by our own selection updates
  if (expectedSource == "table" && lastSource == "selection") {
    return(FALSE)
  }

  # Block plot re-renders from table filter
  if (expectedSource == "plot" && lastSource %in% c("table_filter", "selection", "reset")) {
    return(FALSE)
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
  plotRenderedData[[key]] <<- NULL
  plotUpdateTracker[[key]] <<- NULL
  selectedTermIds[[key]] <<- NULL
  selectedPairs[[key]] <<- NULL
  selectedHeatmapCells[[key]] <<- NULL
}


# Selection state functions (click-based term selection)

getSelectedTerms <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  terms <- selectedTermIds[[key]]
  if (is.null(terms)) return(character(0))
  return(terms)
}

# Adds term if not selected, removes if already selected
toggleTermSelection <- function(type_Tool, plotId, termId) {
  key <- getStateKey(type_Tool, plotId)
  current <- getSelectedTerms(type_Tool, plotId)

  if (termId %in% current) {
    selectedTermIds[[key]] <<- setdiff(current, termId)
  } else {
    selectedTermIds[[key]] <<- union(current, termId)
  }
}

addTermsToSelection <- function(type_Tool, plotId, termIds) {
  key <- getStateKey(type_Tool, plotId)
  current <- getSelectedTerms(type_Tool, plotId)
  selectedTermIds[[key]] <<- union(current, termIds)
}

clearSelection <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  selectedTermIds[[key]] <<- character(0)
}

# Pair-level selection functions for heatmaps
# Tracks selected pairs (order-independent) instead of individual terms

getSelectedPairs <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  pairs <- selectedPairs[[key]]
  if (is.null(pairs)) return(data.frame(source = character(0), target = character(0)))
  return(pairs)
}

# Check if a pair exists (order-independent)
pairExists <- function(pairs, sourceId, targetId) {
  if (nrow(pairs) == 0) return(FALSE)
  any(
    (pairs$source == sourceId & pairs$target == targetId) |
    (pairs$source == targetId & pairs$target == sourceId)
  )
}

# Adds pair if not selected, removes if already selected (order-independent)
togglePairSelection <- function(type_Tool, plotId, sourceId, targetId) {
  key <- getStateKey(type_Tool, plotId)
  pairs <- getSelectedPairs(type_Tool, plotId)

  if (pairExists(pairs, sourceId, targetId)) {
    selectedPairs[[key]] <<- pairs[!(
      (pairs$source == sourceId & pairs$target == targetId) |
      (pairs$source == targetId & pairs$target == sourceId)
    ), , drop = FALSE]
  } else {
    selectedPairs[[key]] <<- rbind(pairs, data.frame(source = sourceId, target = targetId))
  }
}

# Get unique terms from all selected pairs
getTermsFromSelectedPairs <- function(type_Tool, plotId) {
  pairs <- getSelectedPairs(type_Tool, plotId)
  if (nrow(pairs) == 0) return(character(0))
  unique(c(pairs$source, pairs$target))
}

clearPairSelection <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  selectedPairs[[key]] <<- data.frame(source = character(0), target = character(0))
}

# Heatmap cell selection functions for visual highlighting
# Tracks selected cells (x, y coordinates) for rectangle shapes

getSelectedHeatmapCells <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  cells <- selectedHeatmapCells[[key]]
  if (is.null(cells)) return(data.frame(x = character(0), y = character(0)))
  return(cells)
}

# Check if a cell exists (exact match)
cellExists <- function(cells, x, y) {
  if (nrow(cells) == 0) return(FALSE)
  any(cells$x == x & cells$y == y)
}

# Adds cell if not selected, removes if already selected
toggleHeatmapCellSelection <- function(type_Tool, plotId, x, y) {
  key <- getStateKey(type_Tool, plotId)
  cells <- getSelectedHeatmapCells(type_Tool, plotId)

  if (cellExists(cells, x, y)) {
    selectedHeatmapCells[[key]] <<- cells[!(cells$x == x & cells$y == y), , drop = FALSE]
  } else {
    selectedHeatmapCells[[key]] <<- rbind(cells, data.frame(x = as.character(x), y = as.character(y)))
  }
}

clearHeatmapCellSelection <- function(type_Tool, plotId) {
  key <- getStateKey(type_Tool, plotId)
  selectedHeatmapCells[[key]] <<- data.frame(x = character(0), y = character(0))
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

# Clear all plot state for a given run (fullRunKey)
# Used when closing a run tab to clean up all associated plot state
clearPlotStateForRun <- function(fullRunKey) {
  # Clear standard plot states
  for (plotId in c("barchart", "scatterPlot", "dotPlot")) {
    clearPlotState(fullRunKey, plotId)
  }

  # Clear heatmap states
  for (heatmapId in c("heatmap1", "heatmap2", "heatmap3")) {
    clearPlotState(fullRunKey, heatmapId)
  }

  # Clear network states (4 components each)
  for (networkId in c("network1", "network2", "network3")) {
    clearNetworkState(fullRunKey, networkId)
  }

  # Clear current network view
  for (networkId in c("network1", "network2", "network3")) {
    enrichKey <- paste(fullRunKey, networkId, "enrichment", sep = "_")
    edgelistKey <- paste(fullRunKey, networkId, "edgelist", sep = "_")
    currentNetworkView[[enrichKey]] <<- NULL
    currentNetworkView[[edgelistKey]] <<- NULL
  }
}


# Programmatic table update tracking
# Uses a simple list (not reactiveValues) to avoid reactivity complications.
# When we programmatically update a table, we mark it as pending.
# When handleTableFilterChange fires, it checks and clears this flag.

markTableUpdatePending <- function(tableId) {
  programmaticTableUpdateTimes[[tableId]] <<- Sys.time()
}

isProgrammaticTableUpdate <- function(tableId) {
  lastUpdate <- programmaticTableUpdateTimes[[tableId]]
  if (is.null(lastUpdate)) {
    return(FALSE)
  }
  # Block events within 200ms of a programmatic update (cascade events)
  timeSince <- as.numeric(Sys.time() - lastUpdate)
  if (timeSince < 0.2) {
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

isSourceNotNull <- function(sourceSelect) {
  isNotNull <- FALSE
  if (!is.null(sourceSelect)){
    isNotNull <- TRUE
  } else
    renderWarning("Select at least one datasource.")
  return(isNotNull)
}

filterAndPrintTable <- function(session, outputId, sourceSelect, mode, slider,
                                filter = 'top') {
  # session is now an EnrichmentSession object
  runKey <- session$id
  enrichmentFilteredData <-
    filterTopData(session, sourceSelect, mode, slider)

  # Convert Source to factor for dropdown filtering (like Results tab)
  enrichmentFilteredData$Source <- as.factor(enrichmentFilteredData$Source)

  # Extract type_Tool and plotId from outputId
  # outputId format: "{runKey}_{plotId}" e.g., "functional_gProfiler_1_barchart"
  type_Tool <- runKey
  plotId <- gsub(paste0(type_Tool, "_"), "", outputId)

  # Store as ORIGINAL data (this is called from "Generate" button)
  # This sets both original and current view to the same data
  setOriginalData(type_Tool, plotId, enrichmentFilteredData)

  # Mark table update as programmatic to prevent cascade re-render
  markTableUpdatePending(paste0(outputId, "_table"))

  renderEnrichmentTable(
    shinyOutputId = paste0(outputId, "_table"),
    enrichmentFilteredData,
    caption = "Enrichment Results",
    fileName = paste(outputId, paste(sourceSelect, collapse = "_"), sep = "_"),
    mode = "Positive Hits",
    hiddenColumns = c(10, 11),
    expandableColumn = 10,
    filter = filter
  )
  return(enrichmentFilteredData)
}

filterTopData <- function(session, sourceSelect, mode, slider) {
  # Get results from session (not global enrichmentResults)
  enrichmentResult <- session$getResults()
  if (is.null(enrichmentResult)) return(NULL)
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

extractFunctionVsFunctionEdgelist <- function(session,
                                              enrichmentData,
                                              thresholdSlider = NULL,
                                              simplifyForNetwork = FALSE) {
  # session is now an EnrichmentSession object
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
  functionsEdgelist <- appendSourceDatabasesAndIds(session, functionsEdgelist)
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

appendSourceDatabasesAndIds <- function(session, functionsEdgelist) {
  # Get results from session (not global enrichmentResults)
  enrichedNetworkData <- session$getResults()
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

# Visual Highlighting Functions (plotlyProxy-based in-place updates)

# Updates plot highlighting via plotlyProxy (no re-render)
# Adds black borders to selected items using marker.line styling
updatePlotHighlighting <- function(type_Tool, plotId, session) {
  if (is.null(session)) return()

  tryCatch({
    shinyOutputId <- paste(type_Tool, plotId, sep = "_")
    selectedTerms <- getSelectedTerms(type_Tool, plotId)

    # Use rendered data (what's displayed, may differ from original after filtering)
    renderedData <- getRenderedData(type_Tool, plotId)
    if (is.null(renderedData) || nrow(renderedData) == 0) return()

    proxy <- plotlyProxy(shinyOutputId, session = session)

    # DotPlot is single-trace (continuous color), others are multi-trace
    if (plotId == "dotPlot") {
      updateSingleTraceHighlighting(proxy, renderedData, selectedTerms)
    } else {
      updateMultiTraceHighlighting(proxy, renderedData, selectedTerms)
    }
  }, error = function(e) {
    # Silently handle errors in highlighting
  })
}

# Single-trace highlighting (DotPlot): sets per-point border styling
updateSingleTraceHighlighting <- function(proxy, originalData, selectedTerms) {
  n <- nrow(originalData)

  if (length(selectedTerms) == 0) {
    lineColors <- rep("rgba(0,0,0,0.3)", n)
    lineWidths <- rep(1, n)
  } else {
    isSelected <- originalData$Term_ID_noLinks %in% selectedTerms
    lineColors <- ifelse(isSelected, "black", "rgba(0,0,0,0.3)")
    lineWidths <- ifelse(isSelected, 2, 1)
  }

  plotlyProxyInvoke(proxy, "restyle", "marker.line.color", list(lineColors), 0)
  plotlyProxyInvoke(proxy, "restyle", "marker.line.width", list(lineWidths), 0)
}

# Multi-trace highlighting (Barchart, Scatter): one trace per Source category
updateMultiTraceHighlighting <- function(proxy, originalData, selectedTerms) {
  # Plotly creates traces in FACTOR LEVEL ORDER (alphabetical),
  # NOT in row-encounter order. So we must iterate in the same order.
  if (is.factor(originalData$Source)) {
    sources <- levels(originalData$Source)
    # Only keep sources that actually exist in the data
    sources <- sources[sources %in% unique(originalData$Source)]
  } else {
    sources <- sort(unique(as.character(originalData$Source)))
  }

  for (i in seq_along(sources)) {
    traceData <- originalData[originalData$Source == sources[i], ]
    n <- nrow(traceData)
    traceIndex <- i - 1  # 0-indexed for plotly.js

    if (length(selectedTerms) == 0) {
      lineColors <- rep("rgba(0,0,0,0.3)", n)
      lineWidths <- rep(1, n)
    } else {
      isSelected <- traceData$Term_ID_noLinks %in% selectedTerms
      lineColors <- ifelse(isSelected, "black", "rgba(0,0,0,0.3)")
      lineWidths <- ifelse(isSelected, 2, 1)
    }

    plotlyProxyInvoke(proxy, "restyle", "marker.line.color", list(lineColors), traceIndex)
    plotlyProxyInvoke(proxy, "restyle", "marker.line.width", list(lineWidths), traceIndex)
  }
}

# Heatmap highlighting: draws rectangle shapes around selected cells
# Heatmaps don't support marker.line, uses layout shapes instead
updateHeatmapHighlighting <- function(type_Tool, plotId, session) {
  if (is.null(session)) return()

  tryCatch({
    shinyOutputId <- paste(type_Tool, plotId, sep = "_")
    selectedCells <- getSelectedHeatmapCells(type_Tool, plotId)

    # Use rendered data (what's displayed, may differ from original after filtering)
    renderedData <- getRenderedData(type_Tool, plotId)
    if (is.null(renderedData) || nrow(renderedData) == 0) return()

    proxy <- plotlyProxy(shinyOutputId, session = session)
    shapes <- list()

    if (nrow(selectedCells) > 0) {
      # Get category arrays matching renderHeatmap order
      categoryInfo <- getHeatmapCategoryArrays(type_Tool, plotId, renderedData)
      if (is.null(categoryInfo)) return()

      xCategories <- categoryInfo$x
      yCategories <- categoryInfo$y  # Already reversed in getHeatmapCategoryArrays

      for (i in seq_len(nrow(selectedCells))) {
        cellX <- selectedCells$x[i]
        cellY <- selectedCells$y[i]

        # Convert to 0-based indices for plotly.js
        xIndex <- match(cellX, xCategories) - 1
        yIndex <- match(cellY, yCategories) - 1

        if (!is.na(xIndex) && !is.na(yIndex)) {
          # Rectangle spans (index - 0.5) to (index + 0.5) around the cell center
          shapes[[length(shapes) + 1]] <- list(
            type = "rect",
            xref = "x",
            yref = "y",
            x0 = xIndex - 0.5,
            x1 = xIndex + 0.5,
            y0 = yIndex - 0.5,
            y1 = yIndex + 0.5,
            line = list(color = "black", width = 2),
            fillcolor = "rgba(0,0,0,0)"
          )
        }
      }
    }

    # Empty list clears shapes, populated list adds them
    plotlyProxyInvoke(proxy, "relayout", list(shapes = shapes))

  }, error = function(e) {
    # Silently ignore highlight errors
  })
}

# Get heatmap category arrays for proper shape positioning
# Returns list(x = xCategories, y = yCategories) matching renderHeatmap order
getHeatmapCategoryArrays <- function(type_Tool, plotId, enrichmentData) {
  tryCatch({
    if (plotId == "heatmap1") {
      # Heatmap1: Function vs Gene
      # Transform to heatmap format
      heatmapTable <- separateRows(enrichmentData)

      # Get axis setting
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

      # Match renderHeatmap order: y is reversed
      yCategories <- rev(unique(heatmapTable[[yAxisColumn]]))
      xCategories <- unique(heatmapTable[[xAxisColumn]])

    } else if (plotId == "heatmap2") {
      # Heatmap2: Function vs Function
      # Get session from registry (type_Tool is the full run key)
      session <- enrichmentSessionRegistry$get(type_Tool)
      if (is.null(session)) return(NULL)
      heatmapTable <- extractFunctionVsFunctionEdgelist(session, enrichmentData)
      if (is.null(heatmapTable) || nrow(heatmapTable) == 0) return(NULL)

      # Get draw format setting
      drawFormatColumn <- switch(
        input[[paste(type_Tool, "heatmap2_drawFormat", sep = "_")]],
        "Term_ID" = "Id",
        "Function" = "Name"
      )
      yAxisColumn <- paste0("Source ", drawFormatColumn)
      xAxisColumn <- paste0("Target ", drawFormatColumn)

      # Match renderHeatmap order: y is reversed
      yCategories <- rev(unique(heatmapTable[[yAxisColumn]]))
      xCategories <- unique(heatmapTable[[xAxisColumn]])

    } else if (plotId == "heatmap3") {
      # Heatmap3: Gene vs Gene
      # Transform to edgelist format
      heatmapTable <- extractGeneVsGeneEdgelist(enrichmentData)
      if (is.null(heatmapTable) || nrow(heatmapTable) == 0) return(NULL)

      # Match renderHeatmap order: y is reversed
      yCategories <- rev(unique(heatmapTable$`Source Name`))
      xCategories <- unique(heatmapTable$`Target Name`)

    } else {
      return(NULL)
    }

    return(list(x = xCategories, y = yCategories))

  }, error = function(e) {
    return(NULL)
  })
}

# Plot Click and Selection Handlers

# Derives enrichment type from current sidebar selection
# Using input$sideBarId instead of currentEnrichmentType global
# because the global can become stale between runs,
# while sidebar accurately reflects what the user is currently viewing.
deriveEnrichmentTypeFromSidebar <- function() {
  sidebarTab <- input$sideBarId
  if (is.null(sidebarTab)) return(NULL)

  if (sidebarTab == "functional_enrichment") {
    return("functional")
  }

  return(NULL)
}

# Handles plot click events: toggles term selection and updates table
# Plot is NOT re-rendered to preserve zoom state
# Updated to use Run object for validation and state management
handlePlotClick <- function(plotId, plotSource, session = NULL) {
  tryCatch({
    # Derive enrichment type from sidebar - this is the source of truth for which
    # enrichment section the user is currently viewing
    enrichmentType <- deriveEnrichmentTypeFromSidebar()
    if (is.null(enrichmentType)) return()

    # Read the correct tab panel input
    # This ensures we get the tab that's actually visible, not a stale cached value
    selectedTool <- input$toolTabsPanel
    if (is.null(selectedTool) || selectedTool == "" || selectedTool == "Combination") return()

    # Get session from registry for validation and consistent ID usage
    fullRunKey <- paste(enrichmentType, selectedTool, sep = "_")
    run <- enrichmentSessionRegistry$get(fullRunKey)
    if (is.null(run)) return()

    # Use run$id for state management (equivalent to fullRunKey)
    type_Tool <- run$id

    clickData <- event_data("plotly_click", source = plotSource)
    if (is.null(clickData)) return()

    # Simple plots: term-level selection
    if (plotId %in% c("barchart", "scatterPlot", "dotPlot")) {
      clickedTermId <- extractTermIdFromClick(clickData)
      if (is.null(clickedTermId)) return()

      termId <- resolveToTermIdNoLinks(clickedTermId, type_Tool, plotId)
      if (!is.null(termId)) {
        toggleTermSelection(type_Tool, plotId, termId)
        renderTableFromSelection(type_Tool, plotId)
      }

    } else if (plotId == "heatmap1") {
      # Heatmap1: pair-level selection (Function, Gene)
      cellX <- as.character(clickData$x)
      cellY <- as.character(clickData$y)

      # Axis orientation depends on user setting - use run$enrichmentType directly
      uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[run$enrichmentType]])
      heatmap1_axis <- input[[run$getInputId("heatmap1_axis")]]

      if (heatmap1_axis == paste0(uiTermKeyword, "-Genes")) {
        clickedTermId <- cellY
        clickedGene <- cellX
      } else {
        clickedTermId <- cellX
        clickedGene <- cellY
      }

      termId <- resolveToTermIdNoLinks(clickedTermId, type_Tool, plotId)

      if (!is.null(termId) && !is.null(clickedGene)) {
        togglePairSelection(type_Tool, plotId, termId, clickedGene)
        renderTableFromHeatmap1PairSelection(type_Tool, plotId)
        toggleHeatmapCellSelection(type_Tool, plotId, cellX, cellY)
      }

    } else if (plotId == "heatmap2") {
      # Heatmap2: pair-level selection (Function, Function)
      termIds <- extractTermsFromHeatmap2Click(clickData)

      if (length(termIds) == 2) {
        sourceId <- resolveToTermIdNoLinks(termIds[1], type_Tool, plotId)
        targetId <- resolveToTermIdNoLinks(termIds[2], type_Tool, plotId)

        if (!is.null(sourceId) && !is.null(targetId)) {
          togglePairSelection(type_Tool, plotId, sourceId, targetId)
          renderTableFromPairSelection(type_Tool, plotId)

          cellX <- as.character(clickData$x)
          cellY <- as.character(clickData$y)
          toggleHeatmapCellSelection(type_Tool, plotId, cellX, cellY)
        }
      }

    } else if (plotId == "heatmap3") {
      # Heatmap3: pair-level selection (Gene, Gene)
      geneX <- clickData$x
      geneY <- clickData$y

      if (!is.null(geneX) && !is.null(geneY)) {
        togglePairSelection(type_Tool, plotId, geneX, geneY)
        renderTableFromGenePairSelection(type_Tool, plotId)

        cellX <- as.character(geneX)
        cellY <- as.character(geneY)
        toggleHeatmapCellSelection(type_Tool, plotId, cellX, cellY)
      }
    }

    # Update visual highlighting
    if (!is.null(session)) {
      if (plotId %in% c("barchart", "scatterPlot", "dotPlot")) {
        updatePlotHighlighting(type_Tool, plotId, session)
      } else if (plotId %in% c("heatmap1", "heatmap2", "heatmap3")) {
        updateHeatmapHighlighting(type_Tool, plotId, session)
      }
    }

  }, error = function(e) {
    # Silently handle errors - plot click failures shouldn't disrupt the user
  })
}

# Handle plot zoom events - zoom is purely visual, no data/selection changes
# Use Reset View button to clear selection and restore original data
handlePlotZoom <- function(plotId, plotSource) {
  # Zoom events (including autoscale) are visual only - no action needed
  # The plotly chart handles zoom/pan/autoscale internally
  # Selection is managed separately via clicks and Reset View button
}

# Handle plot selection events (lasso/box) - add to selection (cumulative)
# Note: Plot is NOT re-rendered to preserve zoom state
# Updated to use Run object for validation and state management
handlePlotSelection <- function(plotId, plotSource, session = NULL) {
  tryCatch({
    # Derive enrichment type from sidebar - source of truth for current view
    enrichmentType <- deriveEnrichmentTypeFromSidebar()
    if (is.null(enrichmentType)) return()

    # Read the correct tab panel input
    selectedTool <- input$toolTabsPanel
    if (is.null(selectedTool) || selectedTool == "" || selectedTool == "Combination") return()

    # Get session from registry for validation and consistent ID usage
    fullRunKey <- paste(enrichmentType, selectedTool, sep = "_")
    run <- enrichmentSessionRegistry$get(fullRunKey)
    if (is.null(run)) return()

    # Use run$id for state management (equivalent to fullRunKey)
    type_Tool <- run$id

    # Heatmaps don't support lasso well (use click for cells)
    if (plotId %in% c("heatmap1", "heatmap2", "heatmap3")) return()

    selectionData <- event_data("plotly_selected", source = plotSource)
    if (is.null(selectionData) || nrow(selectionData) == 0) return()

    # Get selected term IDs from lasso/box
    selectedValues <- selectionData$customdata
    if (is.null(selectedValues) || length(selectedValues) == 0) {
      selectedValues <- selectionData$y
    }
    if (is.null(selectedValues) || length(selectedValues) == 0) return()

    # Resolve all selected values to Term_ID_noLinks and add to selection
    resolvedTermIds <- character(0)
    for (val in selectedValues) {
      termId <- resolveToTermIdNoLinks(val, type_Tool, plotId)
      if (!is.null(termId)) {
        resolvedTermIds <- c(resolvedTermIds, termId)
      }
    }

    if (length(resolvedTermIds) > 0) {
      # Add to existing selection (cumulative), update table only
      addTermsToSelection(type_Tool, plotId, unique(resolvedTermIds))
      renderTableFromSelection(type_Tool, plotId)

      # Update visual highlighting (black borders on selected items)
      if (!is.null(session)) {
        updatePlotHighlighting(type_Tool, plotId, session)
      }
    }
  }, error = function(e) {
    # Silently ignore selection errors
  })
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
    hiddenColumns = c(10, 11),
    expandableColumn = 10,
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

    # Track rendered data AFTER sorting for correct highlighting
    setRenderedData(type_Tool, plotId, data)
    renderBarchart(plotOutputId, data, column, drawFormatColumn, height)

  } else if (plotId == "scatterPlot") {
    # Add jitter for scatter plot
    data <- addJitter(data)

    # Track rendered data AFTER jittering for correct highlighting
    setRenderedData(type_Tool, plotId, data)
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

    # Track rendered data AFTER sorting for correct highlighting
    setRenderedData(type_Tool, plotId, data)
    renderDotPlot(plotOutputId, data, drawFormatColumn, height)

  } else if (plotId == "heatmap1") {
    setRenderedData(type_Tool, plotId, data)
    renderHeatmap1WithData(type_Tool, data)

  } else if (plotId == "heatmap2") {
    setRenderedData(type_Tool, plotId, data)
    renderHeatmap2WithData(type_Tool, data)

  } else if (plotId == "heatmap3") {
    setRenderedData(type_Tool, plotId, data)
    renderHeatmap3WithData(type_Tool, data)
  }
}

# Render BOTH plot and table from current view (used for reset/autoscale)
renderBothFromCurrentView <- function(type_Tool, plotId) {
  renderTableFromCurrentView(type_Tool, plotId)
  renderPlotFromCurrentView(type_Tool, plotId)
}

# Render plot with specific data (not from currentView)
# Used for DT filter display where we don't want to update currentView state
renderPlotWithData <- function(type_Tool, plotId, data) {
  if (is.null(data) || nrow(data) == 0) return()

  plotOutputId <- paste(type_Tool, plotId, sep = "_")

  if (plotId == "barchart") {
    mode <- input[[paste(type_Tool, "barchart_mode", sep = "_")]]
    column <- switch(mode, "Enrichment Score" = "Enrichment Score %", "-log10Pvalue")
    drawFormatColumn <- input[[paste(type_Tool, "barchart_drawFormat", sep = "_")]]
    data <- orderForBarchartByColumn(data, column, drawFormatColumn)
    height <- calculatePlotHeight(nrow(data))

    # Track rendered data AFTER sorting for correct highlighting
    setRenderedData(type_Tool, plotId, data)
    renderBarchart(plotOutputId, data, column, drawFormatColumn, height)

  } else if (plotId == "scatterPlot") {
    data <- addJitter(data)

    # Track rendered data AFTER jittering for correct highlighting
    setRenderedData(type_Tool, plotId, data)
    renderScatterPlot(plotOutputId, data)

  } else if (plotId == "dotPlot") {
    if (!"Gene Ratio" %in% names(data)) {
      data$`Gene Ratio` <- data$`Intersection Size` / data$`Query size`
    }
    mode <- input[[paste(type_Tool, "dotPlot_mode", sep = "_")]]
    drawFormatColumn <- input[[paste(type_Tool, "dotPlot_drawFormat", sep = "_")]]
    data <- orderForDotPlot(data, mode, drawFormatColumn)
    height <- nrow(data) * DOTPLOT_ENTRY_HEIGHT_PX + MIN_BAR_HEIGHT_PX

    # Track rendered data AFTER sorting for correct highlighting
    setRenderedData(type_Tool, plotId, data)
    renderDotPlot(plotOutputId, data, drawFormatColumn, height)

  } else if (plotId == "heatmap1") {
    setRenderedData(type_Tool, plotId, data)
    renderHeatmap1WithData(type_Tool, data)

  } else if (plotId == "heatmap2") {
    setRenderedData(type_Tool, plotId, data)
    renderHeatmap2WithData(type_Tool, data)

  } else if (plotId == "heatmap3") {
    setRenderedData(type_Tool, plotId, data)
    renderHeatmap3WithData(type_Tool, data)
  }
}

# Selection-aware render functions
# Shows selected terms in table, or all data if nothing selected
renderTableFromSelection <- function(type_Tool, plotId) {
  originalData <- getOriginalData(type_Tool, plotId)
  if (is.null(originalData) || nrow(originalData) == 0) return()

  selectedTerms <- getSelectedTerms(type_Tool, plotId)

  if (length(selectedTerms) == 0) {
    tableData <- originalData
  } else {
    if (!"Term_ID_noLinks" %in% names(originalData)) {
      tableData <- originalData
    } else {
      tableData <- originalData[originalData$Term_ID_noLinks %in% selectedTerms, , drop = FALSE]
    }
  }

  if (nrow(tableData) > 0) {
    updateCurrentView(type_Tool, plotId, tableData, "selection")
    markTableUpdatePending(paste(type_Tool, plotId, "table", sep = "_"))
    renderTableFromCurrentView(type_Tool, plotId)
  }
}

# Heatmap1 pair selection: shows functions from selected (function, gene) pairs
renderTableFromHeatmap1PairSelection <- function(type_Tool, plotId) {
  originalData <- getOriginalData(type_Tool, plotId)
  if (is.null(originalData) || nrow(originalData) == 0) return()

  pairs <- getSelectedPairs(type_Tool, plotId)

  if (nrow(pairs) == 0) {
    tableData <- originalData
  } else {
    # Only show functions (source), not genes (target)
    selectedTerms <- unique(pairs$source)
    tableData <- originalData[originalData$Term_ID_noLinks %in% selectedTerms, , drop = FALSE]
  }

  if (nrow(tableData) > 0) {
    updateCurrentView(type_Tool, plotId, tableData, "selection")
    markTableUpdatePending(paste(type_Tool, plotId, "table", sep = "_"))
    renderTableFromCurrentView(type_Tool, plotId)
  }
}

# Heatmap2 pair selection: shows unique terms from selected (function, function) pairs
renderTableFromPairSelection <- function(type_Tool, plotId) {
  originalData <- getOriginalData(type_Tool, plotId)
  if (is.null(originalData) || nrow(originalData) == 0) return()

  selectedTerms <- getTermsFromSelectedPairs(type_Tool, plotId)

  if (length(selectedTerms) == 0) {
    tableData <- originalData
  } else {
    tableData <- originalData[originalData$Term_ID_noLinks %in% selectedTerms, , drop = FALSE]
  }

  if (nrow(tableData) > 0) {
    updateCurrentView(type_Tool, plotId, tableData, "selection")
    markTableUpdatePending(paste(type_Tool, plotId, "table", sep = "_"))
    renderTableFromCurrentView(type_Tool, plotId)
  }
}

# Heatmap3 gene pair selection: shows terms containing both genes in each pair
renderTableFromGenePairSelection <- function(type_Tool, plotId) {
  originalData <- getOriginalData(type_Tool, plotId)
  if (is.null(originalData) || nrow(originalData) == 0) return()

  genePairs <- getSelectedPairs(type_Tool, plotId)

  if (nrow(genePairs) == 0) {
    tableData <- originalData
  } else {
    # Union of terms containing both genes for each selected pair
    allTermIds <- character(0)
    for (i in seq_len(nrow(genePairs))) {
      geneX <- genePairs$source[i]
      geneY <- genePairs$target[i]

      if (geneX == geneY) {
        # Diagonal: terms containing the single gene
        matchingTerms <- originalData$Term_ID_noLinks[
          sapply(originalData$`Positive Hits`, function(hits) {
            genes <- unlist(strsplit(as.character(hits), ",\\s*"))
            geneX %in% genes
          })
        ]
      } else {
        # Off-diagonal: terms containing both genes
        matchingTerms <- originalData$Term_ID_noLinks[
          sapply(originalData$`Positive Hits`, function(hits) {
            genes <- unlist(strsplit(as.character(hits), ",\\s*"))
            geneX %in% genes && geneY %in% genes
          })
        ]
      }
      allTermIds <- union(allTermIds, matchingTerms)
    }

    tableData <- originalData[originalData$Term_ID_noLinks %in% allTermIds, , drop = FALSE]
  }

  if (nrow(tableData) > 0) {
    updateCurrentView(type_Tool, plotId, tableData, "selection")
    markTableUpdatePending(paste(type_Tool, plotId, "table", sep = "_"))
    renderTableFromCurrentView(type_Tool, plotId)
  }
}

# Extract term ID from click on simple plots (barchart, scatter, dot)
extractTermIdFromClick <- function(clickData) {
  termId <- clickData$customdata
  if (is.null(termId) || length(termId) == 0) {
    termId <- clickData$y
  }
  return(termId)
}

# Resolve a clicked value to Term_ID_noLinks
# The clicked value might be Function name, Term_ID, or Term_ID_noLinks
resolveToTermIdNoLinks <- function(clickedValue, type_Tool, plotId) {
  if (is.null(clickedValue)) return(NULL)

  originalData <- getOriginalData(type_Tool, plotId)
  if (is.null(originalData) || nrow(originalData) == 0) return(NULL)

  # Try direct match on Term_ID_noLinks
  if ("Term_ID_noLinks" %in% names(originalData)) {
    if (clickedValue %in% originalData$Term_ID_noLinks) {
      return(clickedValue)
    }
  }

  # Try matching by Function name
  if ("Function" %in% names(originalData)) {
    match_row <- which(originalData$Function == clickedValue)
    if (length(match_row) > 0) {
      return(originalData$Term_ID_noLinks[match_row[1]])
    }
  }

  # Try matching by Term_ID (with HTML links)
  if ("Term_ID" %in% names(originalData)) {
    match_row <- which(originalData$Term_ID == clickedValue)
    if (length(match_row) > 0) {
      return(originalData$Term_ID_noLinks[match_row[1]])
    }
  }

  return(NULL)
}

# Extract both term IDs from heatmap2 click (both axes are terms)
extractTermsFromHeatmap2Click <- function(clickData) {
  return(c(clickData$x, clickData$y))
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

  # Get session from registry (type_Tool is the full run key)
  session <- enrichmentSessionRegistry$get(type_Tool)
  if (is.null(session)) return()
  heatmapTable <- extractFunctionVsFunctionEdgelist(session, enrichmentData)
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

# Handles table filter changes: updates plot with filtered data
# DT uses client-side filtering, so we render from filtered row indices
# Updated to accept Run object directly
handleTableFilterChange <- function(run, plotId) {
  tryCatch({
    type_Tool <- run$id

    if (!shouldProcessUpdate(type_Tool, plotId, "table")) return()

    tableId <- paste(type_Tool, plotId, "table", sep = "_")
    currentData <- getCurrentView(type_Tool, plotId)
    if (is.null(currentData) || nrow(currentData) == 0) return()

    filteredRowIndices <- input[[paste0(tableId, "_rows_all")]]
    if (is.null(filteredRowIndices) || length(filteredRowIndices) == 0) return()
    if (max(filteredRowIndices) > nrow(currentData)) return()

    # Skip programmatic updates (within 200ms of markTableUpdatePending)
    if (isProgrammaticTableUpdate(tableId)) return()

    # All rows visible = filter cleared, restore plot from currentView
    if (length(filteredRowIndices) == nrow(currentData)) {
      setUpdateSource(type_Tool, plotId, "table_filter")
      renderPlotFromCurrentView(type_Tool, plotId)
      return()
    }

    # Filter active: render plot with filtered subset
    # currentView stays unchanged so filter clear can restore properly
    filteredData <- currentData[filteredRowIndices, , drop = FALSE]
    if (nrow(filteredData) == 0) return()

    # Clear selections (may reference terms no longer visible after filter)
    clearSelection(type_Tool, plotId)
    clearPairSelection(type_Tool, plotId)
    clearHeatmapCellSelection(type_Tool, plotId)

    setUpdateSource(type_Tool, plotId, "table_filter")
    renderPlotWithData(type_Tool, plotId, filteredData)

  }, error = function(e) {
    # Silently ignore filter errors
  })
}

# Note: orderForBarchartByColumn is defined in barchart.R
# Note: orderForDotPlot is defined in dotplot.R

# Clears all selections and restores original data from Generate button
# Updated to accept Run object directly
handleResetView <- function(run, plotId) {
  tryCatch({
    type_Tool <- run$id

    # Clear all selection state
    clearSelection(type_Tool, plotId)
    clearPairSelection(type_Tool, plotId)
    clearHeatmapCellSelection(type_Tool, plotId)

    originalData <- getOriginalData(type_Tool, plotId)
    if (is.null(originalData) || nrow(originalData) == 0) {
      showNotification("No data available. Please generate the plot first.",
                       type = "warning", duration = 3)
      return()
    }

    updateCurrentView(type_Tool, plotId, originalData, "reset")
    renderBothFromCurrentView(type_Tool, plotId)

  }, error = function(e) {
    showNotification("Could not reset view.", type = "error", duration = 3)
  })
}
