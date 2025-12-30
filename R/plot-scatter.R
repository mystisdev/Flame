handleScatterPlot <- function(run) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Scatter Plot.</p>")
    if (run$hasResults()) {
      sourceSelect <- input[[run$getInputId("scatterPlot_sourceSelect")]]

      if (isSourceNotNull(sourceSelect)) {
        enrichmentFilteredData <- filterAndPrintTable(
          run$enrichmentType, run$id,
          outputId = run$getInputId("scatterPlot"),
          sourceSelect = sourceSelect,
          mode = input[[run$getInputId("scatterPlot_mode")]],
          slider = input[[run$getInputId("scatterPlot_slider")]])

        constructScatterPlot(run$id, enrichmentFilteredData)
      }
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create scatterplot with these inputs.")
  }, finally = {
    removeModal()
  })
}

constructScatterPlot <- function(type_Tool, scatterData) {
  scatterData <- addJitter(scatterData)

  # Track rendered data AFTER jittering for correct highlighting
  setRenderedData(type_Tool, "scatterPlot", scatterData)
  renderScatterPlot(paste(type_Tool, "scatterPlot", sep = "_"), scatterData)
}

addJitter <- function(scatterData) {
  size <- nrow(scatterData)
  scatterData$`Enrichment Score %_jittered` <-
    scatterData$`Enrichment Score %` + runif(size, min = -0.5, max = 0.5)
  scatterData$`-log10Pvalue_jittered` <-
    scatterData$`-log10Pvalue` + runif(size, min = -0.005, max = 0.005)
  return(scatterData)
}
