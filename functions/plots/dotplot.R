handleDotPlot <- function(run) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Dot Plot.</p>")
    if (run$hasResults()) {
      sourceSelect <- input[[run$getInputId("dotPlot_sourceSelect")]]

      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          run$enrichmentType, run$id,
          outputId = run$getInputId("dotPlot"),
          sourceSelect = sourceSelect,
          mode = input[[run$getInputId("dotPlot_mode")]],
          slider = input[[run$getInputId("dotPlot_slider")]])

        constructDotPlot(run$id, enrichmentFilteredData)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create dot plot with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructDotPlot <- function(type_Tool, dotPlotData) {
  # Compute Gene Ratio: Intersection Size / Query Size
  dotPlotData$`Gene Ratio` <-
    dotPlotData$`Intersection Size` / dotPlotData$`Query size`

  # Get user preferences
  mode <- input[[paste(type_Tool, "dotPlot_mode", sep = "_")]]
  drawFormatColumn <- input[[paste(type_Tool, "dotPlot_drawFormat", sep = "_")]]

  # Order Y-axis based on selected mode
  dotPlotData <- orderForDotPlot(dotPlotData, mode, drawFormatColumn)

  # Calculate dynamic height
  height <- nrow(dotPlotData) * DOTPLOT_ENTRY_HEIGHT_PX + MIN_BAR_HEIGHT_PX

  # Track rendered data AFTER sorting for correct highlighting
  setRenderedData(type_Tool, "dotPlot", dotPlotData)
  renderDotPlot(paste(type_Tool, "dotPlot", sep = "_"),
                dotPlotData, drawFormatColumn, height)
}

orderForDotPlot <- function(data, mode, drawFormatColumn) {
  orderColumn <- switch(
    mode,
    "Enrichment Score" = "Enrichment Score %",
    "Gene Ratio" = "Gene Ratio",
    "-log10Pvalue"  # default
  )

  # Set factor levels ordered by the column value (ascending = bottom-to-top on Y-axis)
  data[[drawFormatColumn]] <- factor(
    data[[drawFormatColumn]],
    levels = unique(data[[drawFormatColumn]])[
      order(data[[orderColumn]], decreasing = FALSE)])

  return(data)
}
