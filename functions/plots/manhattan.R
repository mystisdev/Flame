isEventFromManhattan <- function(triggeredEvent) {
  isFromManhattan <- T
  if (is.null(triggeredEvent$key)) # key only in Manhattan
    isFromManhattan <- F
  return(isFromManhattan)
}

handleManhattanPlot <- function(run) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Manhattan Plot.</p>")

    if (run$hasResults()) {
      plotOutputId <- run$getInputId("manhattan")
      tableOutputId <- run$getInputId("manhattan_table")

      # Removed global currentManhattanTableId assignment
      # Click handlers now derive table ID from run object

      # Manhattan plot is only available for gProfiler
      if (run$toolName == "gProfiler" && isGprofilerResultValid(run$id)) {
        renderManhattanPlot(plotOutputId, run$id)
        # Show full enrichment table immediately
        renderManhattanEnrichmentTable(tableOutputId, run$getResults())
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

# Updated to derive run context from current state instead of reading globals
# Updated to use run object to derive table ID instead of global currentManhattanTableId
handleManhattanClick <- function(currentTermID) {
  tryCatch({
    # Derive run context (same pattern as handlePlotClick)
    selectedTool <- currentSelectedToolTab
    if (is.null(selectedTool) || selectedTool == "") return()

    enrichmentType <- deriveEnrichmentTypeFromSidebar()
    if (is.null(enrichmentType)) return()

    fullRunKey <- paste(enrichmentType, selectedTool, sep = "_")
    run <- activeRuns[[fullRunKey]]
    if (is.null(run)) return()

    currentTermID <- mapGProfilerIDs(currentTermID)
    results <- run$getResults()
    manhattanTable <- results[match(currentTermID, results$Term_ID_noLinks), ]
    # Updated to derive table ID from run instead of global
    tableOutputId <- run$getInputId("manhattan_table")
    renderManhattanEnrichmentTable(tableOutputId, manhattanTable)
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Could not print selected entries.")
  })
}

# Updated to derive run context from current state instead of reading globals
# Updated to use run object to derive table ID instead of global currentManhattanTableId
handleManhattanSelect <- function(currentTermIDs) {
  tryCatch({
    # Derive run context (same pattern as handlePlotClick)
    selectedTool <- currentSelectedToolTab
    if (is.null(selectedTool) || selectedTool == "") return()

    enrichmentType <- deriveEnrichmentTypeFromSidebar()
    if (is.null(enrichmentType)) return()

    fullRunKey <- paste(enrichmentType, selectedTool, sep = "_")
    run <- activeRuns[[fullRunKey]]
    if (is.null(run)) return()

    currentTermIDs <- mapGProfilerIDs(currentTermIDs)
    results <- run$getResults()
    manhattanTable <- results[which(results$Term_ID_noLinks %in% currentTermIDs), ]
    # Updated to derive table ID from run instead of global
    tableOutputId <- run$getInputId("manhattan_table")
    renderManhattanEnrichmentTable(tableOutputId, manhattanTable)
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
