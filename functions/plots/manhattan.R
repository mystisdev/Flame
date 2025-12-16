isEventFromManhattan <- function(triggeredEvent) {
  isFromManhattan <- T
  if (is.null(triggeredEvent$key)) # key only in Manhattan
    isFromManhattan <- F
  return(isFromManhattan)
}

handleManhattanPlot <- function(fullRunKey = NULL) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Manhattan Plot.</p>")
    # Use fullRunKey if provided, otherwise fall back to currentType_Tool
    type_Tool <- if (!is.null(fullRunKey)) fullRunKey else currentType_Tool

    enrichmentResult <- enrichmentResults[[type_Tool]]
    if (!is.null(enrichmentResult) && nrow(enrichmentResult) > 0) {
      # Build output IDs: dynamic for multi-run, hardcoded for legacy tab
      if (!is.null(fullRunKey)) {
        plotOutputId <- paste(fullRunKey, "manhattan", sep = "_")
        tableOutputId <- paste(fullRunKey, "manhattan_table", sep = "_")
      } else {
        plotOutputId <- "manhattan"
        tableOutputId <- "manhattan_table"
      }

      # Store table ID for click handlers to use
      currentManhattanTableId <<- tableOutputId

      if (isGprofilerResultValid()) {
        renderManhattanPlot(plotOutputId, type_Tool)
        # Show full enrichment table immediately
        renderManhattanEnrichmentTable(tableOutputId, enrichmentResult)
      } else {
        renderWarning("The Manhattan plot is currently available
                      only for the GPROFILER analysis pipeline.")
      }
    } else {
      renderWarning("Execute functional enrichment analysis with gProfiler first.")
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not draw Manhattan plot.")
  }, finally = {
    removeModal()
  })
}

handleManhattanClick <- function(currentTermID) {
  tryCatch({
    currentTermID <- mapGProfilerIDs(currentTermID)
    manhattanTable <- enrichmentResults[[currentType_Tool]][match(
      currentTermID, enrichmentResults[[currentType_Tool]]$Term_ID_noLinks), ]
    renderManhattanEnrichmentTable(currentManhattanTableId, manhattanTable)
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not print selected entries.")
  })
}

handleManhattanSelect <- function(currentTermIDs) {
  tryCatch({
    currentTermIDs <- mapGProfilerIDs(currentTermIDs)
    manhattanTable <-
      enrichmentResults[[currentType_Tool]][which(
        enrichmentResults[[currentType_Tool]]$Term_ID_noLinks %in% currentTermIDs
      ), ]
    renderManhattanEnrichmentTable(currentManhattanTableId, manhattanTable)
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not print selected entries.")
  })
}

mapGProfilerIDs <- function(currentTermIDs) {
  currentTermIDs[grep("KEGG:", currentTermIDs)] <- 
    paste0("map", gsub("[^0-9.-]", "", currentTermIDs[grep("KEGG:", currentTermIDs)]))
  currentTermIDs[grep("REAC:", currentTermIDs)] <-
    gsub("REAC:", "", currentTermIDs[grep("REAC:", currentTermIDs)])
  currentTermIDs[grep("WP:", currentTermIDs)] <-
    gsub("WP:", "", currentTermIDs[grep("WP:", currentTermIDs)])
  return(currentTermIDs)
}
