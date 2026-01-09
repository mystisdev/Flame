renderError <- function(prompt) {
  if (exists("session"))
    shinyalert::shinyalert("Error!", prompt, type = "error")
}

renderWarning <- function(prompt) {
  if (exists("session"))
    shinyalert::shinyalert("Warning!", prompt, type = "warning")
}

renderModal <- function(prompt) {
  showModal(modalDialog(HTML(prompt), footer = NULL))
}

renderShinyText <- function(shinyOutputId, prompt) {
  output[[shinyOutputId]] <- renderText(prompt)
}

# Helper function to create export buttons excluding specified columns
createExportButtons <- function(fileName, excludeColumns = c()) {
  if (length(excludeColumns) > 0) {
    columnSelector <- JS(sprintf(
      "function(idx, data, node) { return [%s].indexOf(idx) === -1; }",
      paste(excludeColumns, collapse = ",")
    ))
    exportOpts <- list(columns = columnSelector)
  } else {
    exportOpts <- list()
  }

  list(
    list(extend = 'excel', filename = fileName, exportOptions = exportOpts),
    list(extend = 'csv', filename = fileName, exportOptions = exportOpts),
    list(extend = 'copy', exportOptions = exportOpts),
    list(extend = 'pdf', filename = fileName,
         exportOptions = c(exportOpts, list(orthogonal = "export")),
         orientation = "landscape"),
    list(extend = 'print', exportOptions = exportOpts)
  )
}

renderShinyDataTable <- function(shinyOutputId, outputData,
                                 caption = NULL, fileName = "",
                                 scrollY = NULL, hiddenColumns = c(),
                                 filter = "none") {
  output[[shinyOutputId]] <- DT::renderDataTable(
    outputData,
    server = F,
    selection = 'none',
    extensions = 'Buttons',
    caption = caption,
    options = list(
      scrollY = scrollY,
      scrollX = TRUE,
      scroller = T,
      "dom" = 'Blfiprt',
      buttons = createExportButtons(fileName, hiddenColumns),
      columnDefs = list(
        list(visible = F, targets = hiddenColumns)
      )
    ),
    filter = filter,
    rownames = F,
    escape = F
  )
}

# Render UpSet plot for the COMBINATION tab (enrichment results comparison)
# NOTE: This is NOT for the Input UpSet plot which compares input gene lists.
#       The Input UpSet is managed by AnalyteListSetOperationsSession in
#       listmgmt-session-setops.R with its own inline rendering.
# Used by: enrich-combination.R to compare enriched terms across multiple runs
renderUpset <- function(shinyOutputId, upsetList, upsetModeFunction, modeLabel) {
  output[[shinyOutputId]] <- upsetjs::renderUpsetjs({
    upsetjs::upsetjs() %>%
      upsetjs::fromList(upsetList) %>%
      upsetjs::interactiveChart() %>%
      upsetModeFunction() %>%
      upsetjs::chartLabels(combination.name = paste0(modeLabel, " Size")) %>%
      upsetjs::chartFontSizes(
        font.family = "Segoe UI",
        chart.label = "18px",
        set.label = "10px") %>%
      upsetjs::chartTheme(color = "#383f4f") %>%
      upsetjs::chartLayout(width.ratios = c(0.2, 0.1, 0.7), bar.padding = 0.3)
  })
}

# renderVolcano() removed - now handled by VolcanoInputSession$renderVolcanoPlot()
# renderLine() removed - was only used by renderVolcano()
# renderReduction() removed - now handled by ReductionInputSession$renderReduction()

renderEnrichmentTable <- function(shinyOutputId, input_table,
                                  caption, fileName, mode,
                                  hiddenColumns, expandableColumn, filter = 'none',
                                  exportExcludeColumns = c(0, 11)){
  output[[shinyOutputId]] <- DT::renderDataTable({
    tableData <- cbind(' ' = '&oplus;', input_table)

    dt <- DT::datatable(
      tableData,
      escape = F,
      rownames = FALSE,
      selection = 'none',
      filter = filter,
      extensions = c('Buttons'),
      caption = caption,
      options = list(
        scrollX = TRUE,
        "dom" = 'T<"clear">lBfrtip',
        buttons = createExportButtons(fileName, exportExcludeColumns),
        columnDefs = list(
          list(visible = F, targets = hiddenColumns),
          list(orderable = F, searchable = F, className = 'details-control', targets = 0)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).find('thead tr:eq(1) td:eq(0)').find('input,select').hide();",
          "}"
        )
      ),
      callback = JS(paste0(
        "table.column(0).nodes().to$().css({cursor: 'pointer'});
        let format = function(d) {
          return '<div style=\"background-color:#eee; padding: .5em;\"> <b>", mode, ":</b> ' +
                  d[", expandableColumn, "] + '</div>';
        };
        table.on('click', 'td.details-control', function() {
          let td = $(this), row = table.row(td.closest('tr'));
          if (row.child.isShown()) {
            row.child.hide();
            td.html('&oplus;');
          } else {
            row.child(format(row.data())).show();
            td.html('&CircleMinus;');
          }
        });"
      ))
    )

    # Format P-value column to show 3 significant figures (keeps numeric for slider filter)
    dt <- DT::formatSignif(dt, columns = 'P-value', digits = 3)
    dt
  }, server = F)
}

renderManhattanEnrichmentTable <- function(shinyOutputId, manhattanTable) {
  # With rownames=FALSE: 0=⊕, ..., 10=Positive Hits, 11=Term_ID_noLinks
  renderEnrichmentTable(shinyOutputId,
                        manhattanTable,
                        caption = "Selected Terms",
                        fileName = "gprofiler_manhattan_selected",
                        mode = "Positive Hits",
                        hiddenColumns = c(10, 11),
                        expandableColumn = 10)
}

renderCombinationTable <- function(shinyOutputId, input_table, caption, fileName,
                                   hiddenColumns, filter = 'none',
                                   exportExcludeColumns = c(0, 4)) {
  # Column indices after cbind (0-indexed, rownames=FALSE):
  # 0=⊕, 1=Source, 2=Term ID, 3=Function, 4=Term_ID_noLinks, 5=Tools,
  # 6=X², 7=Comb. P-value, 8=Intersection_Hits, 9=Union_Hits, 10=Hit_Summary, 11=Rank
  output[[shinyOutputId]] <- DT::renderDataTable({
    tableData <- cbind(' ' = '&oplus;', input_table)

    dt <- DT::datatable(
      tableData,
      escape = F,
      rownames = FALSE,
      selection = 'none',
      filter = filter,
      extensions = c('Buttons'),
      caption = caption,
      options = list(
        scrollX = TRUE,
        "dom" = 'T<"clear">lBfrtip',
        buttons = createExportButtons(fileName, exportExcludeColumns),
        columnDefs = list(
          list(visible = F, targets = hiddenColumns),
          list(orderable = F, searchable = F, className = 'details-control', targets = 0)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).find('thead tr:eq(1) td:eq(0)').find('input,select').hide();",
          "}"
        )
      ),
      callback = JS(
        "table.column(0).nodes().to$().css({cursor: 'pointer'});
        let format = function(d) {
          return '<div style=\"background-color:#eee; padding: .5em;\">' +
                 '<b>Hit Summary:</b> ' + d[10] + '<br/><br/>' +
                 '<b>Intersection Hits (found by all tools):</b><br/>' + d[8] + '<br/><br/>' +
                 '<b>Union Hits (found by any tool):</b><br/>' + d[9] + '</div>';
        };
        table.on('click', 'td.details-control', function() {
          let td = $(this), row = table.row(td.closest('tr'));
          if (row.child.isShown()) {
            row.child.hide();
            td.html('&oplus;');
          } else {
            row.child(format(row.data())).show();
            td.html('&CircleMinus;');
          }
        });"
      )
    )

    # Format numeric columns to show 3 significant figures (keeps numeric for slider filter)
    dt <- DT::formatSignif(dt, columns = c('X<sup>2</sup>', 'Comb. P-value'), digits = 3)
    dt
  }, server = F)
}

renderShinyVisNetwork <- function(networkId, nodes, edges, layout) {
  shinyjs::show(networkId)
  output[[networkId]] <- renderVisNetwork({
    set.seed(123)
    visNetwork(nodes = nodes, edges = edges, background = "white") %>%
      visGroups(groupname = "GO:MF", color = DATASOURCE_COLORS["GO:MF"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "GO:BP", color = DATASOURCE_COLORS["GO:BP"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "GO:CC", color = DATASOURCE_COLORS["GO:CC"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "UNIPROT", color = DATASOURCE_COLORS["UNIPROT"][[1]], shape = "hexagon") %>%
      visGroups(groupname = "KEGG", color = DATASOURCE_COLORS["KEGG"][[1]], shape = "diamond") %>%
      visGroups(groupname = "REAC", color = DATASOURCE_COLORS["REAC"][[1]], shape = "diamond") %>%
      visGroups(groupname = "WP", color = DATASOURCE_COLORS["WP"][[1]], shape = "diamond") %>%
      visGroups(groupname = "PANTHER Pathways", color = DATASOURCE_COLORS["PANTHER Pathways"][[1]], shape = "diamond") %>%
      visGroups(groupname = "DO", color = DATASOURCE_COLORS["DO"][[1]], shape = "triangleDown") %>%
      visGroups(groupname = "DISGENET", color = DATASOURCE_COLORS["DISGENET"][[1]], shape = "triangleDown") %>%
      visGroups(groupname = "OMIM", color = DATASOURCE_COLORS["OMIM"][[1]], shape = "triangleDown") %>%
      visGroups(groupname = "GLAD4U_DISEASE", color = DATASOURCE_COLORS["GLAD4U_DISEASE"][[1]], shape = "triangleDown") %>%
      visGroups(groupname = "ORPHA", color = DATASOURCE_COLORS["ORPHA"][[1]], shape = "triangleDown") %>%
      visGroups(groupname = "DRUGBANK", color = DATASOURCE_COLORS["DRUGBANK"][[1]], shape = "star") %>%
      visGroups(groupname = "GLAD4U_DRUG", color = DATASOURCE_COLORS["GLAD4U_DRUG"][[1]], shape = "star") %>%
      visGroups(groupname = "INTERPRO", color = DATASOURCE_COLORS["INTERPRO"][[1]], shape = "star") %>%
      visGroups(groupname = "PFAM", color = DATASOURCE_COLORS["PFAM"][[1]], shape = "star") %>%
      visGroups(groupname = "BTO", color = DATASOURCE_COLORS["BTO"][[1]], shape = "triangle") %>%
      visGroups(groupname = "WBBT", color = DATASOURCE_COLORS["WBBT"][[1]], shape = "triangle") %>%
      visGroups(groupname = "TF", color = DATASOURCE_COLORS["TF"][[1]], shape = "triangle") %>%
      visGroups(groupname = "CollecTRI", color = DATASOURCE_COLORS["CollecTRI"][[1]], shape = "triangle") %>%
      visGroups(groupname = "MIRNA", color = DATASOURCE_COLORS["MIRNA"][[1]], shape = "triangle") %>%
      visGroups(groupname = "CORUM", color = DATASOURCE_COLORS["CORUM"][[1]], shape = "triangle") %>%
      visGroups(groupname = "HPA", color = DATASOURCE_COLORS["HPA"][[1]], shape = "square") %>%
      visGroups(groupname = "HP", color = DATASOURCE_COLORS["HP"][[1]], shape = "square") %>%
      visGroups(groupname = "WBP", color = DATASOURCE_COLORS["WBP"][[1]], shape = "square") %>%
      visGroups(groupname = "MGI", color = DATASOURCE_COLORS["MGI"][[1]], shape = "square") %>%
      visGroups(groupname = "Gene", color = GENE_NODE_COLOR, shape = "square") %>%
      visGroups(groupname = "PUBMED", color = LITERATURE_NODE_COLOR, shape = "square") %>%
      visEdges(color = "black") %>%
      visIgraphLayout(layout = layout) %>%
      visInteraction(navigationButtons = T, hover = T, multiselect = T) %>%
      # Enable event callbacks for plot-table synchronization
      # Must use sprintf() to inject networkId - this.id doesn't work in vis.js context
      visEvents(
        click = sprintf("function(params) {
          setTimeout(function() {
            Shiny.setInputValue('%s_click', params, {priority: 'event'});
          }, 0);
        }", networkId),
        select = sprintf("function(params) {
          setTimeout(function() {
            Shiny.setInputValue('%s_selected', params.nodes, {priority: 'event'});
          }, 0);
        }", networkId),
        deselectNode = sprintf("function(params) {
          setTimeout(function() {
            Shiny.setInputValue('%s_deselect', params, {priority: 'event'});
          }, 0);
        }", networkId)
      )
  })
}

renderHeatmap <- function(type_Tool, shinyOutputId, heatmapTable, color,
                          yAxisColumn, xAxisColumn, weightColumn, height,
                          showColorbar = TRUE, colorbarTitle = NULL,
                          doubleClickReset = TRUE) {
  # Create unique source ID for plotly events (e.g., "Heatmap1", "Heatmap2", "Heatmap3")
  plotSource <- switch(
    shinyOutputId,
    "heatmap1" = "Heatmap1",
    "heatmap2" = "Heatmap2",
    "heatmap3" = "Heatmap3",
    "Heatmap"  # fallback
  )

  # Get unique y-axis and x-axis categories
  # Data comes in DESCENDING order (highest values first from filterTopData)
  # Plotly's y-axis has y=0 at bottom, so first category = bottom
  # To match barchart/dotplot (highest at TOP), we REVERSE the y-categories
  yCategories <- rev(unique(heatmapTable[[yAxisColumn]]))
  xCategories <- unique(heatmapTable[[xAxisColumn]])

  # Configure double-click behavior
  doubleClickBehavior <- if (doubleClickReset) "reset+autosize" else FALSE

  output[[paste(type_Tool, shinyOutputId, sep = "_")]] <- renderPlotly({
    plot_ly(
      data = heatmapTable,
      y = heatmapTable[[yAxisColumn]],
      x = heatmapTable[[xAxisColumn]],
      z = heatmapTable[[weightColumn]],
      type = 'heatmap',
      colors = colorRamp(c("#f5f6f7", color)),
      hoverinfo = "text",
      hovertext = generateHeatmapHoverText(shinyOutputId),
      height = height,
      source = plotSource,
      showscale = showColorbar,
      colorbar = list(title = colorbarTitle)
    ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          categoryorder = "array",
          categoryarray = xCategories
        ),
        yaxis = list(
          showgrid = FALSE,
          categoryorder = "array",
          categoryarray = yCategories
        )
      ) %>%
      plotly::config(doubleClick = doubleClickBehavior)
  })
}

generateHeatmapHoverText <- function(shinyOutputId) {
  hoverText <- switch(
    shinyOutputId,
    "heatmap1" = ~paste0("<b>Term ID</b>: ", Term_ID_noLinks,
                         "\n<b>Function</b>: ", Function,
                         "\n<b>Gene</b>: ", `Positive Hits`,
                         "\nEnrichment Score %: ", `Enrichment Score %`,
                         "\n-log10Pvalue: ", `-log10Pvalue`,
                         "\nIntersection Size: ", `Intersection Size`),
    "heatmap2" = ~paste0("<b>Function 1 ID</b>: ", `Source Id`,
                         "\n<b>Function 1 Name</b>: ", `Source Name`,
                         "\n<b>Function 2 ID</b>: ", `Target Id`,
                         "\n<b>Function 2 Name</b>: ", `Target Name`,
                         "\nCommon Genes: ", `Common Genes`,
                         "\nTotal Genes: ", `Total Genes`,
                         "\nSimilarity Score %: ", `Similarity Score %`),
    "heatmap3" = ~paste0("<b>Gene 1</b>: ", `Source Name`,
                         "\n<b>Gene 2</b>: ", `Target Name`,
                         "\nCommon Functions: ", `Common Functions`)
  )
  return(hoverText)
}

renderBarchart <- function(shinyOutputId, barchartData, column,
                           drawFormatColumun, height) {
  output[[shinyOutputId]] <- renderPlotly({
    plot_ly(
      data = barchartData,
      x = barchartData[[column]],
      y = barchartData[[drawFormatColumun]],
      type = 'bar',
      orientation = 'h',
      color = ~Source,
      colors = DATASOURCE_COLORS,
      marker = list(
        line = list(color = "rgba(0,0,0,0.3)", width = 1)  # Initial border for restyle
      ),
      text = sprintf("%s/%s",
                     barchartData[["Intersection Size"]],
                     barchartData[["Term Size"]]),
      textfont = list(color = '#000000', size = 16),
      textposition = 'outside',
      hoverinfo = "text",
      hovertext = ~paste0("<b>Term ID</b>: ", Term_ID_noLinks,
                          "\n<b>Function</b>: ", Function,
                          "\nEnrichment Score %: ", `Enrichment Score %`,
                          "\n-log10Pvalue: ", `-log10Pvalue`),
      height = height,
      source = "Barchart",
      customdata = ~Term_ID_noLinks,
      unselected = list(marker = list(opacity = 1))  # Disable auto-dimming on lasso
    ) %>%
      layout(legend = list(
        title = list(text = "Source"),
        itemclick = FALSE,
        itemdoubleclick = FALSE
      ))
  })
}

renderScatterPlot <- function(shinyOutputId, scatterData) {
  output[[shinyOutputId]] <- renderPlotly({
    plot_ly(
      data = scatterData,
      x = ~`-log10Pvalue_jittered`,
      y = ~`Enrichment Score %_jittered`,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 15,
        line = list(
          color = 'rgb(0, 0, 0)',
          width = 1
        )),
      color = ~Source,
      colors = DATASOURCE_COLORS,
      hoverinfo = "text",
      hovertext = ~paste0("<b>Term ID</b>: ", Term_ID_noLinks,
                          "\n<b>Function</b>: ", Function,
                          "\nEnrichment Score %: ", `Enrichment Score %`,
                          "\n-log10Pvalue: ", `-log10Pvalue`),
      source = "Scatter",
      customdata = ~Term_ID_noLinks,
      unselected = list(marker = list(opacity = 1))  # Disable auto-dimming on lasso
    ) %>%
      layout(
        xaxis = list(title = "-log10Pvalue"),
        yaxis = list(title = "Enrichment Score"),
        legend = list(
          title = list(text = "Source"),
          itemclick = FALSE,
          itemdoubleclick = FALSE
        )
      )
  })
}

renderDotPlot <- function(shinyOutputId, dotPlotData, drawFormatColumn, height) {
  # Square root scaling for dot sizes
  minIntersection <- min(dotPlotData$`Intersection Size`)
  maxIntersection <- max(dotPlotData$`Intersection Size`)

  if (maxIntersection == minIntersection) {
    dotPlotData$scaledSize <- (DOTPLOT_SIZE_MIN + DOTPLOT_SIZE_MAX) / 2
  } else {
    dotPlotData$scaledSize <- DOTPLOT_SIZE_MIN +
      (DOTPLOT_SIZE_MAX - DOTPLOT_SIZE_MIN) *
      (sqrt(dotPlotData$`Intersection Size`) - sqrt(minIntersection)) /
      (sqrt(maxIntersection) - sqrt(minIntersection))
  }

  output[[shinyOutputId]] <- renderPlotly({
    plot_ly(
      data = dotPlotData,
      x = ~`Gene Ratio`,
      y = dotPlotData[[drawFormatColumn]],
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = ~scaledSize,
        color = ~`-log10Pvalue`,
        colorscale = DOTPLOT_COLORSCALE,
        colorbar = list(
          title = "-log10(P-value)",
          tickformat = ".1f",
          len = 0.6,
          thickness = 15
        ),
        line = list(color = 'rgba(0,0,0,0.3)', width = 1)
      ),
      hoverinfo = "text",
      hovertext = ~paste0(
        "<b>Term ID</b>: ", Term_ID_noLinks,
        "\n<b>Function</b>: ", Function,
        "\nGene Ratio: ", round(`Gene Ratio`, 3),
        "\nIntersection Size: ", `Intersection Size`,
        "\nTerm Size: ", `Term Size`,
        "\nQuery Size: ", `Query size`,
        "\n-log10Pvalue: ", `-log10Pvalue`,
        "\nEnrichment Score %: ", `Enrichment Score %`
      ),
      height = height,
      source = "DotPlot",
      customdata = ~Term_ID_noLinks,
      unselected = list(marker = list(opacity = 1))  # Disable auto-dimming on lasso
    ) %>%
      layout(xaxis = list(title = "Gene Ratio"))
  })
}

renderManhattanPlot <- function(shinyOutputId, type_Tool) {
  # Use per-run gProfiler result for multi-run support
  result <- gprofilerResults[[type_Tool]]
  if (is.null(result)) result <- gprofilerResult  # fallback for backward compatibility

  output[[shinyOutputId]] <- renderPlotly({
    gprofiler2::gostplot(
      result,
      capped = T,
      interactive = T,
      pal = DATASOURCE_COLORS
    )
  })
}

