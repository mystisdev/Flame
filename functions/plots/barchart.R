handleBarchart <- function(run) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Barchart.</p>")
    if (run$hasResults()) {
      sourceSelect <- input[[run$getInputId("barchart_sourceSelect")]]
      mode <- input[[run$getInputId("barchart_mode")]]

      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          run$enrichmentType, run$id,
          outputId = run$getInputId("barchart"),
          sourceSelect = sourceSelect,
          mode = mode,
          slider = input[[run$getInputId("barchart_slider")]])
        constructBarchart(run$id, enrichmentFilteredData, mode)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create barchart with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructBarchart <- function(type_Tool, enrichmentFilteredData, mode) {
  column <- switch(
    mode,
    "Enrichment Score" = "Enrichment Score %",
    "-log10Pvalue"
  )
  drawFormatColumun <- input[[paste(type_Tool, "barchart_drawFormat", sep = "_")]]

  enrichmentFilteredData <- orderForBarchartByColumn(enrichmentFilteredData,
                                                     column, drawFormatColumun)

  height <- calculatePlotHeight(nrow(enrichmentFilteredData))

  # Track rendered data AFTER sorting for correct highlighting
  setRenderedData(type_Tool, "barchart", enrichmentFilteredData)

  renderBarchart(paste(type_Tool, "barchart", sep = "_"),
                 enrichmentFilteredData, column, drawFormatColumun, height)
}

orderForBarchartByColumn <- function(enrichmentFilteredData, column,
                                     drawFormatColumun) {
  # Set factor levels ordered by the column value (ascending = bottom-to-top on Y-axis)
  enrichmentFilteredData[[drawFormatColumun]] <- factor(
    enrichmentFilteredData[[drawFormatColumun]],
    levels = unique(enrichmentFilteredData[[drawFormatColumun]])[
      order(enrichmentFilteredData[[column]], decreasing = FALSE)])

  return(enrichmentFilteredData)
}
