handleHeatmap <- function(run, heatmapId) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Rendering Heatmap.</p>")
    if (run$hasResults()) {
      handleHeatmapCallbackFunction <- switch(
        heatmapId,
        "heatmap1" = handleFunctionVsGeneHeatmap,
        "heatmap2" = handleFunctionVsFunctionHeatmap,
        "heatmap3" = handleGeneVsGeneHeatmap
      )
      handleHeatmapCallbackFunction(run)
    }
  }, error = function(e) {
    cat(paste0("Error: ", e))
    renderWarning("Cannot create heatmap with these inputs.")
  }, finally = {
    removeModal()
  })
}

handleFunctionVsGeneHeatmap <- function(run) {
  source <- input[[run$getInputId("heatmap1_sourceSelect")]]

  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      run$enrichmentType, run$id,
      outputId = run$getInputId("heatmap1"),
      sourceSelect = source,
      mode = input[[run$getInputId("heatmap1_mode")]],
      slider = input[[run$getInputId("heatmap1_slider")]])

    constructFunctionsVsGeneHeatmap(run$enrichmentType,
                                    run$id, enrichmentFilteredData)
  }
}

constructFunctionsVsGeneHeatmap <- function(enrichmentType,
                                            type_Tool, enrichmentFilteredData) {
  heatmapTable <- separateRows(enrichmentFilteredData)
  heatmapTable$GeneExists <- 1
  
  uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])
  heatmap1_axis <- input[[paste(type_Tool, "heatmap1_axis", sep = "_")]]
  drawFormatColumun <- input[[paste(type_Tool, "heatmap1_drawFormat", sep = "_")]]
  if (heatmap1_axis == paste0(uiTermKeyword, "-Genes")) {
    yAxisColumn <- drawFormatColumun
    xAxisColumn <- "Positive Hits"
  } else {
    yAxisColumn <- "Positive Hits"
    xAxisColumn <- drawFormatColumun
  }
  entriesCount <- length(unique(heatmapTable[[yAxisColumn]]))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap1", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap1_sourceSelect", sep = "_")]]]],
                yAxisColumn, xAxisColumn,
                weightColumn = "GeneExists",
                height = height,
                showColorbar = FALSE)
}

handleFunctionVsFunctionHeatmap <- function(run) {
  source <- input[[run$getInputId("heatmap2_sourceSelect")]]

  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      run$enrichmentType, run$id,
      outputId = run$getInputId("heatmap2"),
      sourceSelect = source,
      mode = input[[run$getInputId("heatmap2_mode")]],
      slider = input[[run$getInputId("heatmap2_slider")]])

    constructFunctionsVsFunctionHeatmap(run$id, enrichmentFilteredData)
  }
}

# Updated to accept runKey (full key like "functional_gProfiler_1")
constructFunctionsVsFunctionHeatmap <- function(type_Tool, enrichmentFilteredData) {
  heatmapTable <- extractFunctionVsFunctionEdgelist(type_Tool, enrichmentFilteredData)
  
  drawFormatColumun <- switch(
    input[[paste(type_Tool, "heatmap2_drawFormat", sep = "_")]],
    "Term_ID" = "Id",
    "Function" = "Name"
  )
  yAxisColumn <- paste0("Source ", drawFormatColumun)
  xAxisColumn <- paste0("Target ", drawFormatColumun)
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap2", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap2_sourceSelect", sep = "_")]]]],
                yAxisColumn = yAxisColumn,
                xAxisColumn = xAxisColumn,
                weightColumn = "Similarity Score %",
                height = height,
                colorbarTitle = "Similarity %")
}

handleGeneVsGeneHeatmap <- function(run) {
  source <- input[[run$getInputId("heatmap3_sourceSelect")]]

  if (isSourceNotNull(source)) {
    enrichmentFilteredData <- filterAndPrintTable(
      run$enrichmentType, run$id,
      outputId = run$getInputId("heatmap3"),
      sourceSelect = source,
      mode = input[[run$getInputId("heatmap3_mode")]],
      slider = input[[run$getInputId("heatmap3_slider")]])

    constructGeneVsGeneHeatmap(run$id, enrichmentFilteredData)
  }
}

constructGeneVsGeneHeatmap <- function(type_Tool, enrichmentFilteredData) {
  heatmapTable <- extractGeneVsGeneEdgelist(enrichmentFilteredData)
  
  entriesCount <- length(unique(heatmapTable$`Source Name`))
  height <- calculatePlotHeight(entriesCount)
  renderHeatmap(type_Tool, "heatmap3", heatmapTable,
                color = DATASOURCE_COLORS[[input[[paste(
                  type_Tool, "heatmap3_sourceSelect", sep = "_")]]]],
                yAxisColumn = "Source Name",
                xAxisColumn = "Target Name",
                weightColumn = "Common Functions",
                height = height,
                colorbarTitle = "Common Functions")
}
