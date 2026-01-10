handleEnrichmentNetwork <- function(run, networkId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Network.</p>")
    if (run$hasResults()) {
      handleNetworkCallbackFunction <- switch(
        networkId,
        "network1" = handleFunctionVsGeneNetwork,
        "network2" = handleFunctionVsFunctionNetwork,
        "network3" = handleGeneVsGeneNetwork
      )
      handleNetworkCallbackFunction(run)
    }
  }, error = function(e) {
    renderWarning("Cannot create network with the chosen settings.
                  Please adjust the data sources or filter values.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneNetwork <- function(run) {
  source <- input[[run$getInputId("network1_sourceSelect")]]

  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      run,
      outputId = run$getInputId("network1"),
      sourceSelect = source,
      mode = input[[run$getInputId("network1_mode")]],
      slider = input[[run$getInputId("network1_slider")]])

    networkEdgelist <- separateRows(enrichmentFilteredData)
    networkEdgelist <- keepEdgelistColumns(networkEdgelist)
    run$setArenaEdgelist("network1", networkEdgelist)
    renderShinyDataTable(
      shinyOutputId = run$getInputId("network1_edgelist"),
      networkEdgelist,
      caption = "Edgelist",
      fileName = paste(run$id, paste(source, collapse = "_"),
                       "network1_edgelist", sep = "_"),
      filter = "top"  # Enable filtering for sync
    )

    networkData <- constructVisNetwork(
      run$id,
      networkId = "network1",
      networkEdgelist,
      sourceColumnName = "Source Id",
      targetColumnName = "Target Gene",
      weightColumn = NULL
    )

    # Store state for plot-table synchronization
    setNetworkOriginalData(
      run$id, "network1",
      enrichmentFilteredData, networkEdgelist,
      networkData$nodes, networkData$edges
    )

    # Initialize current network view (for deselect restore)
    setCurrentNetworkView(run$id, "network1", enrichmentFilteredData, networkEdgelist)
  }
}

keepEdgelistColumns <- function(gprofilerNetworkData) {
  gprofilerNetworkData <- gprofilerNetworkData[, c(
    "Source", "Term_ID_noLinks", "Function", "Positive Hits")]
  colnames(gprofilerNetworkData) <-
    c("Source Database", "Source Id", "Source Name", "Target Gene")
  return(gprofilerNetworkData)
}

constructVisNetwork <- function(type_Tool, networkId, networkEdgelist,
                                sourceColumnName, targetColumnName,
                                weightColumn) {
  graph <- createGraph(networkEdgelist, sourceColumnName,
                       targetColumnName, weightColumn)
  data <- toVisNetworkData(graph)
  nodes <- data$nodes
  row.names(nodes) <- NULL
  nodes$font.size <- 24
  nodes <- appendGroupsAndTitles(type_Tool, networkId, nodes, networkEdgelist)
  edges <- data$edges
  edges$id <- seq_len(nrow(edges))  # Required for edge click events
  edges <- appendWidth(edges)
  edges$color <- "black"
  layout <- names(LAYOUT_CHOICES)[match(
    input[[paste(type_Tool, networkId, "layout", sep = "_")]], LAYOUT_CHOICES)]
  renderShinyVisNetwork(paste(type_Tool, networkId, sep = "_"), nodes, edges, layout)

  # Return nodes and edges for state storage
  return(list(nodes = nodes, edges = edges))
}

createGraph <- function(networkEdgelist, sourceColumnName,
                        targetColumnName, weightColumn) {
  graph <- igraph::graph_from_edgelist(
    as.matrix(networkEdgelist[, c(sourceColumnName, targetColumnName)]),
    directed = FALSE
  )
  if (!is.null(weightColumn))
    E(graph)$weight <- networkEdgelist[[weightColumn]]
  else
    E(graph)$weight <- 1
  return(graph)
}

appendGroupsAndTitles <- function(type_Tool, networkId, nodes, networkEdgelist) {
  nodes$group <-
    networkEdgelist$`Source Database`[match(
      nodes$label, networkEdgelist$`Source Id`)]
  nodes$title <- networkEdgelist$`Source Name`[match(
    nodes$label, networkEdgelist$`Source Id`)]
  if (networkId == "network1") {
    nodes$group[is.na(nodes$group)] <- "Gene"
    if (input[[paste(type_Tool, networkId, "drawFormat", sep = "_")]] == "Function") {
      # Use which() to avoid NA issues in subscripts
      nonGeneIdx <- which(nodes$group != "Gene")
      if (length(nonGeneIdx) > 0) {
        temp <- nodes$label[nonGeneIdx]
        nodes$label[nonGeneIdx] <- nodes$title[nonGeneIdx]
        nodes$title[nonGeneIdx] <- temp
      }
    }
  } else if (networkId == "network2") {
    nodes$group[is.na(nodes$group)] <-
      networkEdgelist$`Target Database`[match(
        nodes$label[is.na(nodes$group)], networkEdgelist$`Target Id`)]
    nodes$title[is.na(nodes$title)] <- networkEdgelist$`Target Name`[match(
      nodes$label[is.na(nodes$title)], networkEdgelist$`Target Id`)]
    if (input[[paste(type_Tool, networkId, "drawFormat", sep = "_")]] == "Function") {
      temp <- nodes$label
      nodes$label <- nodes$title
      nodes$title <- temp
    }
  } else if (networkId == "network3") {
    nodes$group <- "Gene"
  }
  return(nodes)
}

appendWidth <- function(edges) {
  edges$width <- mapRange(edges$weight,
                          newMin = EDGE_WIDTH_MIN, newMax = EDGE_WIDTH_MAX)
  edges$title <- edges$weight
  return(edges)
}

handleFunctionVsFunctionNetwork <- function(run) {
  source <- input[[run$getInputId("network2_sourceSelect")]]

  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      run,
      outputId = run$getInputId("network2"),
      sourceSelect = source,
      mode = input[[run$getInputId("network2_mode")]],
      slider = input[[run$getInputId("network2_slider")]])

    networkEdgelist <-
      extractFunctionVsFunctionEdgelist(
        run, enrichmentFilteredData,
        input[[run$getInputId("network2_thresholdSlider")]],
        simplifyForNetwork = TRUE
      )
    if (existEnoughEdges(run$id, "network2", networkEdgelist)) {
      run$setArenaEdgelist("network2", networkEdgelist)
      renderShinyDataTable(
        shinyOutputId = run$getInputId("network2_edgelist"),
        networkEdgelist,
        caption = "Edgelist",
        fileName = paste(run$id, paste(source, collapse = "_"),
                         "network2_edgelist", sep = "_"),
        filter = "top"  # Enable filtering for sync
      )

      networkData <- constructVisNetwork(
        run$id,
        networkId = "network2",
        networkEdgelist,
        sourceColumnName = "Source Id",
        targetColumnName = "Target Id",
        weightColumn = "Similarity Score %"
      )

      # Store state for plot-table synchronization
      setNetworkOriginalData(
        run$id, "network2",
        enrichmentFilteredData, networkEdgelist,
        networkData$nodes, networkData$edges
      )

      # Initialize current network view (for deselect restore)
      setCurrentNetworkView(run$id, "network2", enrichmentFilteredData, networkEdgelist)
    }
  }
}

existEnoughEdges <- function(runKey, networkId, networkEdgelist) {
  exist <- FALSE
  if (nrow(networkEdgelist) > 0){
    exist <- TRUE
  } else {
    renderWarning("The current chosen filters cannot produce enough edges to form a network.
                  Please adjust the data sources or filter values.")
    resetEdgelist_ViewAndArenaObjects(runKey, networkId)
  }
  return(exist)
}

handleGeneVsGeneNetwork <- function(run) {
  source <- input[[run$getInputId("network3_sourceSelect")]]

  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      run,
      outputId = run$getInputId("network3"),
      sourceSelect = source,
      mode = input[[run$getInputId("network3_mode")]],
      slider = input[[run$getInputId("network3_slider")]])

    networkEdgelist <-
      extractGeneVsGeneEdgelist(
        enrichmentFilteredData,
        input[[run$getInputId("network3_thresholdSlider")]],
        simplifyForNetwork = TRUE
      )
    if (existEnoughEdges(run$id, "network3", networkEdgelist)) {
      run$setArenaEdgelist("network3", networkEdgelist)
      renderShinyDataTable(
        shinyOutputId = run$getInputId("network3_edgelist"),
        networkEdgelist,
        caption = "Edgelist",
        fileName = paste(run$id, paste(source, collapse = "_"),
                         "network3_edgelist", sep = "_"),
        filter = "top"  # Enable filtering for sync
      )

      networkData <- constructVisNetwork(
        run$id,
        networkId = "network3",
        networkEdgelist,
        sourceColumnName = "Source Name",
        targetColumnName = "Target Name",
        weightColumn = "Common Functions"
      )

      # Store state for plot-table synchronization
      setNetworkOriginalData(
        run$id, "network3",
        enrichmentFilteredData, networkEdgelist,
        networkData$nodes, networkData$edges
      )

      # Initialize current network view (for deselect restore)
      setCurrentNetworkView(run$id, "network3", enrichmentFilteredData, networkEdgelist)
    }
  }
}


# Network-Table Synchronization Functions for visNetwork plots
#
# State is only set when user clicks Generate (above functions).
# Filter and click handlers render filtered views without modifying state,
# since DT uses client-side filtering with indices relative to rendered data.

handleNetworkNodeClick <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id
    networkInputId <- paste(type_Tool, networkId, sep = "_")

    clickEvent <- input[[paste(networkInputId, "click", sep = "_")]]

    # No nodes clicked - check if it's an edge click or empty space
    if (is.null(clickEvent) || is.null(clickEvent$nodes) ||
        length(clickEvent$nodes) == 0) {
      # Only trigger deselection if no edges were clicked either (true empty space)
      if (is.null(clickEvent$edges) || length(clickEvent$edges) == 0) {
        handleNetworkDeselection(run, networkId)
      }
      return()
    }

    if (!shouldProcessNetworkUpdate(type_Tool, networkId, "network")) return()

    nodeId <- clickEvent$nodes[[1]]
    setNetworkUpdateSource(type_Tool, networkId, "network_click")

    if (networkId == "network1") {
      renderNetwork1NodeClickTables(type_Tool, networkId, nodeId)
    } else if (networkId == "network2") {
      renderNetwork2NodeClickTables(type_Tool, networkId, nodeId)
    } else if (networkId == "network3") {
      renderNetwork3NodeClickTables(type_Tool, networkId, nodeId)
    }
  }, error = function(e) {
    # Silently ignore click errors
  })
}

# Network1: Function vs Gene
renderNetwork1NodeClickTables <- function(type_Tool, networkId, nodeId) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  isTerm <- nodeId %in% enrichmentData$Term_ID_noLinks ||
            nodeId %in% enrichmentData$Function

  if (isTerm) {
    filteredEnrichment <- enrichmentData[
      enrichmentData$Term_ID_noLinks == nodeId |
      enrichmentData$Function == nodeId, , drop = FALSE
    ]
    filteredEdgelist <- edgelistData[
      edgelistData$`Source Id` == nodeId |
      edgelistData$`Source Name` == nodeId, , drop = FALSE
    ]
  } else {
    filteredEnrichment <- enrichmentData[
      grepl(nodeId, enrichmentData$`Positive Hits`, fixed = TRUE), , drop = FALSE
    ]
    filteredEdgelist <- edgelistData[
      edgelistData$`Target Gene` == nodeId, , drop = FALSE
    ]
  }

  if (nrow(filteredEnrichment) > 0) {
    enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
    edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
    setExpectedRowCount(enrichmentTableId, nrow(filteredEnrichment))
    setExpectedRowCount(edgelistTableId, nrow(filteredEdgelist))

    renderEnrichmentTableWithData(type_Tool, networkId, filteredEnrichment)
    renderEdgelistTableWithData(type_Tool, networkId, filteredEdgelist)
  }
}

# Network2: Function vs Function
renderNetwork2NodeClickTables <- function(type_Tool, networkId, nodeId) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  # Get all edges connected to the clicked term
  filteredEdgelist <- edgelistData[
    edgelistData$`Source Id` == nodeId |
    edgelistData$`Source Name` == nodeId |
    edgelistData$`Target Id` == nodeId |
    edgelistData$`Target Name` == nodeId, , drop = FALSE
  ]

  # Collect all connected term IDs (clicked term + all terms it connects to)
  connectedTermIds <- unique(c(
    nodeId,
    filteredEdgelist$`Source Id`,
    filteredEdgelist$`Target Id`
  ))

  # Filter enrichment to show all connected terms
  filteredEnrichment <- enrichmentData[
    enrichmentData$Term_ID_noLinks %in% connectedTermIds |
    enrichmentData$Function %in% connectedTermIds, , drop = FALSE
  ]

  if (nrow(filteredEdgelist) > 0) {
    enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
    edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
    setExpectedRowCount(enrichmentTableId, nrow(filteredEnrichment))
    setExpectedRowCount(edgelistTableId, nrow(filteredEdgelist))

    renderEnrichmentTableWithData(type_Tool, networkId, filteredEnrichment)
    renderEdgelistTableWithData(type_Tool, networkId, filteredEdgelist)
  }
}

# Network3: Gene vs Gene
renderNetwork3NodeClickTables <- function(type_Tool, networkId, nodeId) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  filteredEnrichment <- enrichmentData[
    grepl(nodeId, enrichmentData$`Positive Hits`, fixed = TRUE), , drop = FALSE
  ]

  filteredEdgelist <- edgelistData[
    edgelistData$`Source Name` == nodeId |
    edgelistData$`Target Name` == nodeId, , drop = FALSE
  ]

  if (nrow(filteredEnrichment) > 0) {
    enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
    edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
    setExpectedRowCount(enrichmentTableId, nrow(filteredEnrichment))
    setExpectedRowCount(edgelistTableId, nrow(filteredEdgelist))

    renderEnrichmentTableWithData(type_Tool, networkId, filteredEnrichment)
    renderEdgelistTableWithData(type_Tool, networkId, filteredEdgelist)
  }
}

handleNetworkEdgeClick <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id
    networkInputId <- paste(type_Tool, networkId, sep = "_")

    # Edge clicks come through the click event (not selectEdge, which causes zoom)
    clickEvent <- input[[paste(networkInputId, "click", sep = "_")]]
    if (is.null(clickEvent) || is.null(clickEvent$edges) ||
        length(clickEvent$edges) == 0) return()

    # If nodes were clicked, let the node click handler handle it
    if (!is.null(clickEvent$nodes) && length(clickEvent$nodes) > 0) return()

    if (!shouldProcessNetworkUpdate(type_Tool, networkId, "network")) return()

    edgeId <- clickEvent$edges[[1]]

    edges <- getNetworkEdges(type_Tool, networkId)
    if (is.null(edges) || nrow(edges) == 0) return()

    edgeRow <- edges[edges$id == edgeId, , drop = FALSE]
    if (nrow(edgeRow) == 0) return()

    fromNode <- edgeRow$from[1]
    toNode <- edgeRow$to[1]

    edgelistData <- getNetworkEdgelistData(type_Tool, networkId)
    if (is.null(edgelistData) || nrow(edgelistData) == 0) return()

    enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)

    if (networkId == "network1") {
      # Term-Gene edge: filter edgelist to this edge, enrichment to the term
      filteredEdgelist <- edgelistData[
        (edgelistData$`Source Id` == fromNode & edgelistData$`Target Gene` == toNode) |
        (edgelistData$`Source Id` == toNode & edgelistData$`Target Gene` == fromNode) |
        (edgelistData$`Source Name` == fromNode & edgelistData$`Target Gene` == toNode) |
        (edgelistData$`Source Name` == toNode & edgelistData$`Target Gene` == fromNode),
        , drop = FALSE
      ]
      # The term is whichever node matches the enrichment data
      termNodes <- c(fromNode, toNode)
      filteredEnrichment <- enrichmentData[
        enrichmentData$Term_ID_noLinks %in% termNodes |
        enrichmentData$Function %in% termNodes, , drop = FALSE
      ]
    } else if (networkId == "network2") {
      # Term-Term edge: filter edgelist to this edge, enrichment to both terms
      filteredEdgelist <- edgelistData[
        (edgelistData$`Source Id` == fromNode & edgelistData$`Target Id` == toNode) |
        (edgelistData$`Source Id` == toNode & edgelistData$`Target Id` == fromNode) |
        (edgelistData$`Source Name` == fromNode & edgelistData$`Target Name` == toNode) |
        (edgelistData$`Source Name` == toNode & edgelistData$`Target Name` == fromNode),
        , drop = FALSE
      ]
      termNodes <- c(fromNode, toNode)
      filteredEnrichment <- enrichmentData[
        enrichmentData$Term_ID_noLinks %in% termNodes |
        enrichmentData$Function %in% termNodes, , drop = FALSE
      ]
    } else {
      # Gene-Gene edge: filter edgelist to this edge, enrichment to terms with both genes
      filteredEdgelist <- edgelistData[
        (edgelistData$`Source Name` == fromNode & edgelistData$`Target Name` == toNode) |
        (edgelistData$`Source Name` == toNode & edgelistData$`Target Name` == fromNode),
        , drop = FALSE
      ]
      # Filter to terms that contain both genes in their Positive Hits
      filteredEnrichment <- enrichmentData[
        grepl(fromNode, enrichmentData$`Positive Hits`, fixed = TRUE) &
        grepl(toNode, enrichmentData$`Positive Hits`, fixed = TRUE), , drop = FALSE
      ]
    }

    if (nrow(filteredEdgelist) > 0) {
      enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
      edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
      setExpectedRowCount(enrichmentTableId, nrow(filteredEnrichment))
      setExpectedRowCount(edgelistTableId, nrow(filteredEdgelist))
      setNetworkUpdateSource(type_Tool, networkId, "network_click")
      renderEnrichmentTableWithData(type_Tool, networkId, filteredEnrichment)
      renderEdgelistTableWithData(type_Tool, networkId, filteredEdgelist)
    }
  }, error = function(e) {
    # Silently ignore edge click errors
  })
}

handleNetworkSelection <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id
    networkInputId <- paste(type_Tool, networkId, sep = "_")

    selectedNodes <- input[[paste(networkInputId, "selected", sep = "_")]]
    if (is.null(selectedNodes) || length(selectedNodes) == 0) return()

    # Single node clicks are handled by handleNetworkNodeClick - only handle multi-select
    if (length(selectedNodes) == 1) return()

    if (!shouldProcessNetworkUpdate(type_Tool, networkId, "network")) return()

    setNetworkUpdateSource(type_Tool, networkId, "network_select")
    renderTablesForSelectedNodes(type_Tool, networkId, selectedNodes)
  }, error = function(e) {
    # Silently ignore selection errors
  })
}

renderTablesForSelectedNodes <- function(type_Tool, networkId, selectedNodes) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

  if (networkId == "network1") {
    termNodes <- selectedNodes[selectedNodes %in% enrichmentData$Term_ID_noLinks |
                               selectedNodes %in% enrichmentData$Function]
    geneNodes <- setdiff(selectedNodes, termNodes)

    termMatch <- enrichmentData$Term_ID_noLinks %in% termNodes |
                 enrichmentData$Function %in% termNodes
    geneMatch <- sapply(enrichmentData$`Positive Hits`, function(hits) {
      genes <- unlist(strsplit(as.character(hits), ",\\s*"))
      any(genes %in% geneNodes)
    })
    filteredEnrichment <- enrichmentData[termMatch | geneMatch, , drop = FALSE]

    filteredEdgelist <- edgelistData[
      edgelistData$`Source Id` %in% selectedNodes |
      edgelistData$`Source Name` %in% selectedNodes |
      edgelistData$`Target Gene` %in% selectedNodes, , drop = FALSE
    ]

  } else if (networkId == "network2") {
    filteredEnrichment <- enrichmentData[
      enrichmentData$Term_ID_noLinks %in% selectedNodes |
      enrichmentData$Function %in% selectedNodes, , drop = FALSE
    ]

    filteredEdgelist <- edgelistData[
      edgelistData$`Source Id` %in% selectedNodes |
      edgelistData$`Source Name` %in% selectedNodes |
      edgelistData$`Target Id` %in% selectedNodes |
      edgelistData$`Target Name` %in% selectedNodes, , drop = FALSE
    ]

  } else {
    filteredEnrichment <- enrichmentData[
      sapply(enrichmentData$`Positive Hits`, function(hits) {
        genes <- unlist(strsplit(as.character(hits), ",\\s*"))
        any(genes %in% selectedNodes)
      }), , drop = FALSE
    ]

    filteredEdgelist <- edgelistData[
      edgelistData$`Source Name` %in% selectedNodes |
      edgelistData$`Target Name` %in% selectedNodes, , drop = FALSE
    ]
  }

  if (nrow(filteredEnrichment) > 0) {
    enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
    edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
    setExpectedRowCount(enrichmentTableId, nrow(filteredEnrichment))
    setExpectedRowCount(edgelistTableId, nrow(filteredEdgelist))

    renderEnrichmentTableWithData(type_Tool, networkId, filteredEnrichment)
    renderEdgelistTableWithData(type_Tool, networkId, filteredEdgelist)
  }
}

# Restore tables to current network view when clicking empty space
handleNetworkDeselection <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id

    enrichmentData <- getCurrentNetworkViewEnrichment(type_Tool, networkId)
    edgelistData <- getCurrentNetworkViewEdgelist(type_Tool, networkId)

    # Fall back to full state if no current view is set
    if (is.null(enrichmentData)) {
      enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
      edgelistData <- getNetworkEdgelistData(type_Tool, networkId)
    }

    if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

    enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
    edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
    setExpectedRowCount(enrichmentTableId, nrow(enrichmentData))
    if (!is.null(edgelistData) && nrow(edgelistData) > 0) {
      setExpectedRowCount(edgelistTableId, nrow(edgelistData))
    }

    renderEnrichmentTableWithData(type_Tool, networkId, enrichmentData)
    if (!is.null(edgelistData) && nrow(edgelistData) > 0) {
      renderEdgelistTableWithData(type_Tool, networkId, edgelistData)
    }
  }, error = function(e) {
    # Silently ignore deselection errors
  })
}


# Table filter handlers update only the network to avoid cross-table cascades
handleNetworkEnrichmentTableFilter <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id
    tableId <- paste(type_Tool, networkId, "table", sep = "_")

    filteredRows <- input[[paste0(tableId, "_rows_all")]]
    if (is.null(filteredRows) || length(filteredRows) == 0) return()
    if (isReRenderCascade(tableId, filteredRows)) return()

    enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
    if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

    if (max(filteredRows) > nrow(enrichmentData)) return()

    # All rows visible means full state
    if (length(filteredRows) == nrow(enrichmentData)) {
      edgelistData <- getNetworkEdgelistData(type_Tool, networkId)
      setCurrentNetworkView(type_Tool, networkId, enrichmentData, edgelistData)
      renderNetworkFromState(type_Tool, networkId)
      return()
    }

    filteredEnrichment <- enrichmentData[filteredRows, , drop = FALSE]
    filteredEdgelist <- getFilteredEdgelistFromEnrichment(
      type_Tool, networkId, filteredEnrichment)

    setCurrentNetworkView(type_Tool, networkId, filteredEnrichment, filteredEdgelist)

    # All networks use the same approach: filter existing edges to visible nodes
    renderNetworkFromFilteredEnrichment(type_Tool, networkId, filteredEnrichment)

    # Sync edgelist table with cascade prevention
    edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")
    setExpectedRowCount(edgelistTableId, nrow(filteredEdgelist))
    renderEdgelistFromFilteredEnrichment(type_Tool, networkId, filteredEnrichment)
  }, error = function(e) {
    # Silently ignore filter errors
  })
}

handleNetworkEdgelistTableFilter <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id
    tableId <- paste(type_Tool, networkId, "edgelist", sep = "_")

    filteredRows <- input[[paste0(tableId, "_rows_all")]]
    if (is.null(filteredRows) || length(filteredRows) == 0) return()
    if (isReRenderCascade(tableId, filteredRows)) return()

    edgelistData <- getNetworkEdgelistData(type_Tool, networkId)
    if (is.null(edgelistData) || nrow(edgelistData) == 0) return()

    if (max(filteredRows) > nrow(edgelistData)) return()

    # All rows visible means full state
    if (length(filteredRows) == nrow(edgelistData)) {
      enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
      setCurrentNetworkView(type_Tool, networkId, enrichmentData, edgelistData)
      renderNetworkFromState(type_Tool, networkId)
      return()
    }

    filteredEdgelist <- edgelistData[filteredRows, , drop = FALSE]
    filteredEnrichment <- getFilteredEnrichmentFromEdgelist(
      type_Tool, networkId, filteredEdgelist)

    setCurrentNetworkView(type_Tool, networkId, filteredEnrichment, filteredEdgelist)
    renderNetworkFromFilteredEdgelist(type_Tool, networkId, filteredEdgelist)

    # Sync enrichment table with cascade prevention
    enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
    setExpectedRowCount(enrichmentTableId, nrow(filteredEnrichment))
    renderEnrichmentFromFilteredEdgelist(type_Tool, networkId, filteredEdgelist)
  }, error = function(e) {
    # Silently ignore filter errors
  })
}


# Rendering functions that filter data for display without modifying state

renderNetworkFromFilteredEnrichment <- function(type_Tool, networkId, filteredEnrichment) {
  nodes <- getNetworkNodes(type_Tool, networkId)
  edges <- getNetworkEdges(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  if (is.null(nodes) || nrow(nodes) == 0) return()

  visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
  visibleTermNames <- unique(filteredEnrichment$Function)

  if (networkId == "network1") {
    filteredEdgelist <- edgelistData[
      edgelistData$`Source Id` %in% visibleTermIds |
      edgelistData$`Source Name` %in% visibleTermNames, , drop = FALSE
    ]
    visibleGenes <- unique(filteredEdgelist$`Target Gene`)

    filteredNodes <- nodes[
      nodes$id %in% visibleTermIds |
      nodes$id %in% visibleTermNames |
      nodes$label %in% visibleTermIds |
      nodes$label %in% visibleTermNames |
      nodes$id %in% visibleGenes |
      nodes$label %in% visibleGenes, , drop = FALSE
    ]

  } else if (networkId == "network2") {
    filteredNodes <- nodes[
      nodes$id %in% visibleTermIds |
      nodes$id %in% visibleTermNames |
      nodes$label %in% visibleTermIds |
      nodes$label %in% visibleTermNames, , drop = FALSE
    ]

  } else {
    # Network3: get genes from filtered enrichment's Positive Hits
    visibleGenes <- unique(unlist(strsplit(
      as.character(filteredEnrichment$`Positive Hits`), ",\\s*"
    )))
    filteredNodes <- nodes[
      nodes$id %in% visibleGenes |
      nodes$label %in% visibleGenes, , drop = FALSE
    ]
  }

  filteredEdges <- edges[
    edges$from %in% filteredNodes$id & edges$to %in% filteredNodes$id, , drop = FALSE
  ]

  layout <- names(LAYOUT_CHOICES)[match(
    input[[paste(type_Tool, networkId, "layout", sep = "_")]], LAYOUT_CHOICES)]
  renderShinyVisNetwork(paste(type_Tool, networkId, sep = "_"), filteredNodes, filteredEdges, layout)
}

# Network3 requires edge recomputation from filtered enrichment
renderNetwork3FromFilteredEnrichment <- function(type_Tool, networkId, filteredEnrichment) {
  threshold <- input[[paste(type_Tool, "network3_thresholdSlider", sep = "_")]]

  newEdgelist <- extractGeneVsGeneEdgelist(
    filteredEnrichment,
    thresholdSlider = threshold,
    simplifyForNetwork = TRUE
  )

  if (is.null(newEdgelist) || nrow(newEdgelist) == 0) {
    showNotification("No gene connections remain with current filter.",
                     type = "warning", duration = 3)
    return()
  }

  graph <- igraph::graph_from_data_frame(
    newEdgelist[, c("Source Name", "Target Name")],
    directed = FALSE
  )
  igraph::E(graph)$weight <- newEdgelist$`Common Functions`

  data <- visNetwork::toVisNetworkData(graph)
  nodes <- data$nodes
  nodes$group <- "Gene"
  nodes$font.size <- 24
  nodes$label <- nodes$id
  nodes$title <- nodes$id

  edges <- data$edges
  edges$id <- seq_len(nrow(edges))  # Required for edge click events
  if (nrow(edges) > 0 && "weight" %in% names(edges)) {
    edges$width <- mapRange(edges$weight, newMin = EDGE_WIDTH_MIN, newMax = EDGE_WIDTH_MAX)
    edges$title <- as.character(edges$weight)
  }
  edges$color <- "black"

  layout <- names(LAYOUT_CHOICES)[match(
    input[[paste(type_Tool, networkId, "layout", sep = "_")]], LAYOUT_CHOICES)]
  renderShinyVisNetwork(paste(type_Tool, networkId, sep = "_"), nodes, edges, layout)
}

renderEdgelistFromFilteredEnrichment <- function(type_Tool, networkId, filteredEnrichment) {
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
  visibleTermNames <- unique(filteredEnrichment$Function)

  if (networkId == "network1") {
    filteredEdgelist <- edgelistData[
      edgelistData$`Source Id` %in% visibleTermIds |
      edgelistData$`Source Name` %in% visibleTermNames, , drop = FALSE
    ]
  } else if (networkId == "network2") {
    filteredEdgelist <- edgelistData[
      (edgelistData$`Source Id` %in% visibleTermIds | edgelistData$`Source Name` %in% visibleTermNames) &
      (edgelistData$`Target Id` %in% visibleTermIds | edgelistData$`Target Name` %in% visibleTermNames),
      , drop = FALSE
    ]
  } else {
    allGenes <- unique(unlist(strsplit(as.character(filteredEnrichment$`Positive Hits`), ",\\s*")))
    filteredEdgelist <- edgelistData[
      edgelistData$`Source Name` %in% allGenes &
      edgelistData$`Target Name` %in% allGenes, , drop = FALSE
    ]
  }

  renderEdgelistTableWithData(type_Tool, networkId, filteredEdgelist)
}

renderNetworkFromFilteredEdgelist <- function(type_Tool, networkId, filteredEdgelist) {
  nodes <- getNetworkNodes(type_Tool, networkId)
  edges <- getNetworkEdges(type_Tool, networkId)

  if (is.null(nodes) || nrow(nodes) == 0) return()

  # Build set of valid (from, to) pairs from the filtered edgelist
  if (networkId == "network1") {
    referencedNodes <- unique(c(
      filteredEdgelist$`Source Id`,
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Gene`
    ))
    # Create edge pairs: source (term) -> target (gene)
    validEdgePairs <- unique(paste(
      ifelse(!is.na(filteredEdgelist$`Source Id`),
             filteredEdgelist$`Source Id`, filteredEdgelist$`Source Name`),
      filteredEdgelist$`Target Gene`, sep = "|||"
    ))
  } else if (networkId == "network2") {
    referencedNodes <- unique(c(
      filteredEdgelist$`Source Id`,
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Id`,
      filteredEdgelist$`Target Name`
    ))
    # Create edge pairs: source (term) -> target (term)
    validEdgePairs <- unique(paste(
      ifelse(!is.na(filteredEdgelist$`Source Id`),
             filteredEdgelist$`Source Id`, filteredEdgelist$`Source Name`),
      ifelse(!is.na(filteredEdgelist$`Target Id`),
             filteredEdgelist$`Target Id`, filteredEdgelist$`Target Name`),
      sep = "|||"
    ))
  } else {
    referencedNodes <- unique(c(
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Name`
    ))
    # Create edge pairs: source (gene) -> target (gene)
    validEdgePairs <- unique(paste(
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Name`, sep = "|||"
    ))
  }

  filteredNodes <- nodes[
    nodes$id %in% referencedNodes |
    nodes$label %in% referencedNodes, , drop = FALSE
  ]

  # Filter edges to only those in the filtered edgelist (check both directions)
  edgePairsForward <- paste(edges$from, edges$to, sep = "|||")
  edgePairsReverse <- paste(edges$to, edges$from, sep = "|||")
  filteredEdges <- edges[
    edgePairsForward %in% validEdgePairs | edgePairsReverse %in% validEdgePairs,
    , drop = FALSE
  ]

  layout <- names(LAYOUT_CHOICES)[match(
    input[[paste(type_Tool, networkId, "layout", sep = "_")]], LAYOUT_CHOICES)]
  renderShinyVisNetwork(paste(type_Tool, networkId, sep = "_"), filteredNodes, filteredEdges, layout)
}

renderEnrichmentFromFilteredEdgelist <- function(type_Tool, networkId, filteredEdgelist) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)

  if (networkId == "network1") {
    referencedTermIds <- unique(c(
      filteredEdgelist$`Source Id`,
      filteredEdgelist$`Source Name`
    ))
    filteredEnrichment <- enrichmentData[
      enrichmentData$Term_ID_noLinks %in% referencedTermIds |
      enrichmentData$Function %in% referencedTermIds, , drop = FALSE
    ]
  } else if (networkId == "network2") {
    referencedTermIds <- unique(c(
      filteredEdgelist$`Source Id`,
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Id`,
      filteredEdgelist$`Target Name`
    ))
    filteredEnrichment <- enrichmentData[
      enrichmentData$Term_ID_noLinks %in% referencedTermIds |
      enrichmentData$Function %in% referencedTermIds, , drop = FALSE
    ]
  } else {
    referencedGenes <- unique(c(
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Name`
    ))
    filteredEnrichment <- enrichmentData[
      sapply(enrichmentData$`Positive Hits`, function(hits) {
        genes <- unlist(strsplit(as.character(hits), ",\\s*"))
        any(genes %in% referencedGenes)
      }), , drop = FALSE
    ]
  }

  renderEnrichmentTableWithData(type_Tool, networkId, filteredEnrichment)
}

handleNetworkResetView <- function(run, networkId) {
  tryCatch({
    type_Tool <- run$id

    enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
    edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

    if (is.null(enrichmentData) || nrow(enrichmentData) == 0) {
      showNotification("No data available. Generate the network first.",
                       type = "warning", duration = 3)
      return()
    }

    setCurrentNetworkView(type_Tool, networkId, enrichmentData, edgelistData)
    renderNetworkFromState(type_Tool, networkId)
    renderNetworkTablesFromStateWithTracking(type_Tool, networkId)
  }, error = function(e) {
    showNotification("Could not reset view.", type = "error", duration = 3)
  })
}


# Helper functions for rendering from state

renderNetworkFromState <- function(type_Tool, networkId) {
  nodes <- getNetworkNodes(type_Tool, networkId)
  edges <- getNetworkEdges(type_Tool, networkId)

  if (is.null(nodes) || nrow(nodes) == 0) return()

  layout <- names(LAYOUT_CHOICES)[match(
    input[[paste(type_Tool, networkId, "layout", sep = "_")]], LAYOUT_CHOICES)]

  renderShinyVisNetwork(paste(type_Tool, networkId, sep = "_"), nodes, edges, layout)
}

# Uses expected row count tracking to prevent cascade when tables fire _rows_all after re-render
renderNetworkTablesFromStateWithTracking <- function(type_Tool, networkId) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  enrichmentTableId <- paste(type_Tool, networkId, "table", sep = "_")
  edgelistTableId <- paste(type_Tool, networkId, "edgelist", sep = "_")

  if (!is.null(enrichmentData) && nrow(enrichmentData) > 0) {
    setExpectedRowCount(enrichmentTableId, nrow(enrichmentData))
    renderEnrichmentTableWithData(type_Tool, networkId, enrichmentData)
  }
  if (!is.null(edgelistData) && nrow(edgelistData) > 0) {
    setExpectedRowCount(edgelistTableId, nrow(edgelistData))
    renderEdgelistTableWithData(type_Tool, networkId, edgelistData)
  }
}

renderNetworkTablesFromState <- function(type_Tool, networkId) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)

  if (!is.null(enrichmentData) && nrow(enrichmentData) > 0) {
    renderEnrichmentTableWithData(type_Tool, networkId, enrichmentData)
  }
  if (!is.null(edgelistData) && nrow(edgelistData) > 0) {
    renderEdgelistTableWithData(type_Tool, networkId, edgelistData)
  }
}


# Table rendering with explicit data

renderEnrichmentTableWithData <- function(type_Tool, networkId, data) {
  if (is.null(data) || nrow(data) == 0) return()

  tableOutputId <- paste(type_Tool, networkId, "table", sep = "_")
  data$Source <- as.factor(data$Source)

  renderEnrichmentTable(
    shinyOutputId = tableOutputId,
    input_table = data,
    caption = "Enrichment Results",
    fileName = paste(type_Tool, networkId, sep = "_"),
    mode = "Positive Hits",
    hiddenColumns = c(10, 11),
    expandableColumn = 10,
    filter = "top"
  )
}

renderEdgelistTableWithData <- function(type_Tool, networkId, data) {
  if (is.null(data) || nrow(data) == 0) return()

  tableOutputId <- paste(type_Tool, networkId, "edgelist", sep = "_")
  source <- input[[paste(type_Tool, paste0(networkId, "_sourceSelect"), sep = "_")]]

  renderShinyDataTable(
    shinyOutputId = tableOutputId,
    outputData = data,
    caption = "Edgelist",
    fileName = paste(type_Tool, paste(source, collapse = "_"),
                     paste0(networkId, "_edgelist"), sep = "_"),
    filter = "top"
  )
}


# Cross-table filtering helpers that return filtered data without rendering

getFilteredEdgelistFromEnrichment <- function(type_Tool, networkId, filteredEnrichment) {
  edgelistData <- getNetworkEdgelistData(type_Tool, networkId)
  if (is.null(edgelistData) || nrow(edgelistData) == 0) return(NULL)

  visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
  visibleTermNames <- unique(filteredEnrichment$Function)

  if (networkId == "network1") {
    filteredEdgelist <- edgelistData[
      edgelistData$`Source Id` %in% visibleTermIds |
      edgelistData$`Source Name` %in% visibleTermNames, , drop = FALSE
    ]
  } else if (networkId == "network2") {
    filteredEdgelist <- edgelistData[
      (edgelistData$`Source Id` %in% visibleTermIds | edgelistData$`Source Name` %in% visibleTermNames) &
      (edgelistData$`Target Id` %in% visibleTermIds | edgelistData$`Target Name` %in% visibleTermNames),
      , drop = FALSE
    ]
  } else {
    allGenes <- unique(unlist(strsplit(as.character(filteredEnrichment$`Positive Hits`), ",\\s*")))
    filteredEdgelist <- edgelistData[
      edgelistData$`Source Name` %in% allGenes &
      edgelistData$`Target Name` %in% allGenes, , drop = FALSE
    ]
  }

  return(filteredEdgelist)
}

getFilteredEnrichmentFromEdgelist <- function(type_Tool, networkId, filteredEdgelist) {
  enrichmentData <- getNetworkEnrichmentData(type_Tool, networkId)
  if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return(NULL)

  if (networkId == "network1") {
    referencedTermIds <- unique(c(
      filteredEdgelist$`Source Id`,
      filteredEdgelist$`Source Name`
    ))
    filteredEnrichment <- enrichmentData[
      enrichmentData$Term_ID_noLinks %in% referencedTermIds |
      enrichmentData$Function %in% referencedTermIds, , drop = FALSE
    ]
  } else if (networkId == "network2") {
    referencedTermIds <- unique(c(
      filteredEdgelist$`Source Id`,
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Id`,
      filteredEdgelist$`Target Name`
    ))
    filteredEnrichment <- enrichmentData[
      enrichmentData$Term_ID_noLinks %in% referencedTermIds |
      enrichmentData$Function %in% referencedTermIds, , drop = FALSE
    ]
  } else {
    referencedGenes <- unique(c(
      filteredEdgelist$`Source Name`,
      filteredEdgelist$`Target Name`
    ))
    filteredEnrichment <- enrichmentData[
      sapply(enrichmentData$`Positive Hits`, function(hits) {
        genes <- unlist(strsplit(as.character(hits), ",\\s*"))
        any(genes %in% referencedGenes)
      }), , drop = FALSE
    ]
  }

  return(filteredEnrichment)
}
