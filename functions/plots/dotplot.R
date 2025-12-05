handleDotPlot <- function(enrichmentType, enrichmentTool) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Dot Plot.</p>")
    if (existEnrichmentResults(enrichmentType, enrichmentTool)) {
      type_Tool <- paste(enrichmentType, enrichmentTool, sep = "_")
      sourceSelect <- input[[paste(type_Tool, "dotPlot_sourceSelect", sep = "_")]]

      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          enrichmentType, enrichmentTool,
          outputId = paste(type_Tool, "dotPlot", sep = "_"),
          sourceSelect = sourceSelect,
          mode = input[[paste(type_Tool, "dotPlot_mode", sep = "_")]],
          slider = input[[paste(type_Tool, "dotPlot_slider", sep = "_")]])

        constructDotPlot(type_Tool, enrichmentFilteredData)
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

  # Factor levels in reverse order so highest values appear at top
  data[[drawFormatColumn]] <- factor(
    data[[drawFormatColumn]],
    levels = unique(data[[drawFormatColumn]])[order(data[[orderColumn]], decreasing = FALSE)]
  )
  return(data)
}
