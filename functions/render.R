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

renderShinyDataTable <- function(shinyOutputId, outputData,
                                 caption = NULL, fileName = "",
                                 scrollY = NULL, hiddenColumns = c(),
                                 filter = "none") {
  output[[shinyOutputId]] <- DT::renderDataTable(
    outputData,
    server = F,
    extensions = 'Buttons',
    caption = caption,
    options = list(
      scrollY = scrollY,
      scrollX = TRUE,
      scroller = T,
      "dom" = 'Blfiprt',
      buttons = list(
        list(extend = 'excel', filename = fileName),
        list(extend = 'csv', filename = fileName),
        list(extend = 'copy', filename = fileName),
        list(extend = 'pdf', filename = fileName),
        list(extend = 'print', filename = fileName)
      ),
      columnDefs = list(
        list(visible = F, targets = hiddenColumns)
      )
    ),
    filter = filter,
    rownames = F,
    escape = F
  )
}

renderUpset <- function(shinyOutputId, upsetList, upsetModeFunction) {
  output[[shinyOutputId]] <- upsetjs::renderUpsetjs({
    upsetjs::upsetjs() %>%
      upsetjs::fromList(upsetList) %>%
      upsetjs::interactiveChart() %>%
      upsetModeFunction() %>%
      upsetjs::chartLabels(combination.name = paste0(currentUpsetMode, " Size")) %>% 
      upsetjs::chartFontSizes(
        font.family = "Segoe UI",
        chart.label = "18px",
        set.label = "10px") %>%
      upsetjs::chartTheme(color = "#383f4f") %>%
      upsetjs::chartLayout(width.ratios = c(0.2, 0.1, 0.7), bar.padding = 0.3)
  })
}

renderVolcano <- function() {
  minLog10PValue <- min(volcanoPlotData$`-log10Pvalue`)
  maxLog10PValue <- max(volcanoPlotData$`-log10Pvalue`)
  maxLog2FC <- max(volcanoPlotData$logfc)
  pvalueThreshold <- input$volcano_pvalue_slider
  logFCThreshold <- input$volcano_fc_slider

  output$volcanoPlot <- renderPlotly({
    plot_ly(
      data = volcanoPlotData,
      x = ~`logfc`,
      y = ~`-log10Pvalue`,
      customdata = ~symbol,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 6),
      color = ~expression,
      colors = VOLCANO_COLORS,
      hoverinfo = "text",
      hovertext = ~paste0("<b>Gene</b>: ", symbol,
                          "\nlogFC: ", logfc,
                          "\n-log10Pvalue: ", `-log10Pvalue`),
      source = "Volcano"
    ) %>%
      layout(
        xaxis = list(title = "log2FC"),
        yaxis = list(title = "-log10Pvalue"),
        showlegend = FALSE,
        dragmode = "lasso",
        shapes = list(
          renderLine(minLog10PValue, maxLog10PValue, logFCThreshold, logFCThreshold),
          renderLine(minLog10PValue, maxLog10PValue, -logFCThreshold, -logFCThreshold),
          renderLine(pvalueThreshold, pvalueThreshold, -maxLog2FC, maxLog2FC)
        )
      )
  })
}

renderLine <- function(y0, y1, x0, x1) {
  list(
    type = "line",
    y0 = y0,
    y1 = y1,
    x0 = x0,
    x1 = x1,
    line = list(color = "red")
  )
}

renderReduction <- function() {
  gene_col <- input$reduction_gene_col
  x_col <- input$reduction_x_axis
  y_col <- input$reduction_y_axis
  color_col <- input$reduction_color
  size_col <- input$reduction_size

  # Exclude genes with missing gene names or coordinates
  plot_data <- currentReduction[
    !is.na(currentReduction[[gene_col]]) & currentReduction[[gene_col]] != "" &
    !is.na(currentReduction[[x_col]]) &
    !is.na(currentReduction[[y_col]]), ]

  # Build hover text
  hover_text <- paste0("<b>", gene_col, "</b>: ", plot_data[[gene_col]])
  for (col_name in names(plot_data)) {
    if (col_name != gene_col) {
      col_data <- plot_data[[col_name]]
      formatted_data <- if (is.numeric(col_data)) {
        ifelse(is.na(col_data), "NA", round(col_data, 2))
      } else {
        ifelse(is.na(col_data), "NA", as.character(col_data))
      }
      hover_text <- paste0(hover_text, "\n", col_name, ": ", formatted_data)
    }
  }
  plot_data$hover_text <- hover_text

  use_color <- !is.null(color_col) && color_col != ""
  use_size <- !is.null(size_col) && size_col != ""

  # Determine color type and prepare palette
  is_categorical_color <- FALSE
  color_palette <- NULL
  if (use_color) {
    color_data <- plot_data[[color_col]]
    is_categorical_color <- is.character(color_data) || is.factor(color_data) ||
                           length(unique(color_data[!is.na(color_data)])) <= REDUCTION_CATEGORICAL_THRESHOLD

    if (is_categorical_color) {
      n_colors <- length(unique(color_data[!is.na(color_data)]))
      colors_set <- RColorBrewer::brewer.pal(min(max(3, n_colors), 8), "Set2")
      color_palette <- if (n_colors > 8) colorRampPalette(colors_set)(n_colors) else colors_set
    }
  }

  # Pre-transform size data
  if (use_size) {
    size_data_scaled <- log10(plot_data[[size_col]] + 1)
    size_range <- range(size_data_scaled, na.rm = TRUE)

    plot_data$size_scaled <- if (size_range[2] > size_range[1]) {
      REDUCTION_SIZE_RANGE[1] + (REDUCTION_SIZE_RANGE[2] - REDUCTION_SIZE_RANGE[1]) *
        (size_data_scaled - size_range[1]) / (size_range[2] - size_range[1])
    } else {
      rep(REDUCTION_DEFAULT_SIZE, length(size_data_scaled))
    }
    plot_data$size_scaled[is.na(plot_data[[size_col]])] <- REDUCTION_NA_SIZE
  }

  output$reductionPlot <- renderPlotly({
    p <- plot_ly(source = "ReductionPlot")

    # Categorical color - plot each category separately with legend
    if (use_color && is_categorical_color) {
      color_categories <- unique(plot_data[[color_col]])
      color_categories <- color_categories[!is.na(color_categories)]

      # Plot valid colors
      for (c_idx in seq_along(color_categories)) {
        subset_data <- plot_data[
          plot_data[[color_col]] == color_categories[c_idx] &
          !is.na(plot_data[[color_col]]), ]

        if (nrow(subset_data) > 0) {
          p <- p %>% add_trace(
            data = subset_data,
            x = ~get(x_col), y = ~get(y_col),
            type = "scatter", mode = "markers",
            marker = list(
              color = color_palette[c_idx],
              size = if (use_size) subset_data$size_scaled else REDUCTION_DEFAULT_SIZE,
              line = list(width = 0),
              opacity = REDUCTION_MARKER_OPACITY
            ),
            text = ~hover_text, hoverinfo = "text",
            customdata = ~get(gene_col),
            name = as.character(color_categories[c_idx]),
            showlegend = TRUE
          )
        }
      }

      # Plot NA color (use grey, keep default circle shape)
      subset_data <- plot_data[is.na(plot_data[[color_col]]), ]

      if (nrow(subset_data) > 0) {
        p <- p %>% add_trace(
          data = subset_data,
          x = ~get(x_col), y = ~get(y_col),
          type = "scatter", mode = "markers",
          marker = list(
            color = REDUCTION_NA_COLOR,
            size = if (use_size) subset_data$size_scaled else REDUCTION_DEFAULT_SIZE,
            line = list(width = 0),
            opacity = REDUCTION_MARKER_OPACITY
          ),
          text = ~hover_text, hoverinfo = "text",
          customdata = ~get(gene_col),
          name = "NA",
          showlegend = TRUE
        )
      }
    # Continuous color or no color mapping
    } else {
      if (use_color && !is_categorical_color) {
        # Plot points with valid color values
        subset_data <- plot_data[!is.na(plot_data[[color_col]]), ]

        if (nrow(subset_data) > 0) {
          marker_config <- list(
            color = subset_data[[color_col]],
            colorscale = REDUCTION_COLORSCALE_CONTINUOUS,
            colorbar = list(title = color_col),
            showscale = TRUE,
            size = if (use_size) subset_data$size_scaled else REDUCTION_DEFAULT_SIZE,
            line = list(width = 0),
            opacity = REDUCTION_MARKER_OPACITY
          )

          p <- p %>% add_trace(
            data = subset_data,
            x = ~get(x_col), y = ~get(y_col),
            type = "scatter", mode = "markers",
            marker = marker_config,
            text = ~hover_text, hoverinfo = "text",
            customdata = ~get(gene_col),
            showlegend = FALSE
          )
        }

        # Plot NA color values (use grey, keep default circle shape)
        subset_data <- plot_data[is.na(plot_data[[color_col]]), ]

        if (nrow(subset_data) > 0) {
          p <- p %>% add_trace(
            data = subset_data,
            x = ~get(x_col), y = ~get(y_col),
            type = "scatter", mode = "markers",
            marker = list(
              color = REDUCTION_NA_COLOR,
              size = if (use_size) subset_data$size_scaled else REDUCTION_DEFAULT_SIZE,
              line = list(width = 0),
              opacity = REDUCTION_MARKER_OPACITY
            ),
            text = ~hover_text, hoverinfo = "text",
            customdata = ~get(gene_col),
            name = paste0(color_col, ": NA"),
            showlegend = TRUE
          )
        }
      } else {
        # No color mapping - plot all points
        marker_config <- list(
          size = if (use_size) plot_data$size_scaled else REDUCTION_DEFAULT_SIZE,
          line = list(width = 0),
          opacity = REDUCTION_MARKER_OPACITY
        )

        p <- p %>% add_trace(
          data = plot_data,
          x = ~get(x_col), y = ~get(y_col),
          type = "scatter", mode = "markers",
          marker = marker_config,
          text = ~hover_text, hoverinfo = "text",
          customdata = ~get(gene_col),
          showlegend = FALSE
        )
      }
    }

    # Configure legend title
    legend_title <- if (use_color && is_categorical_color) {
      color_col
    }

    p %>% layout(
      xaxis = list(title = x_col),
      yaxis = list(title = y_col),
      dragmode = "lasso",
      hovermode = "closest",
      legend = if (!is.null(legend_title)) list(title = list(text = legend_title)) else list()
    )
  })
}

renderEnrichmentTable <- function(shinyOutputId, input_table,
                                  caption, fileName, mode,
                                  hiddenColumns, expandableColumn, filter = 'none'){
  output[[shinyOutputId]] <- DT::renderDataTable(
    server = F,
    cbind(' ' = '&oplus;', input_table),
    escape = F,
    filter = filter,  # Enable column-specific filtering controls (default: none)
    extensions = c('Buttons'),
    caption = caption,
    options = list(
      "dom" = 'T<"clear">lBfrtip',
      buttons = list(
        list(extend = 'excel', filename = fileName),
        list(extend = 'csv', filename = fileName),
        list(extend = 'copy', filename = fileName),
        list(extend = 'pdf', filename = fileName, exportOptions = list(orthogonal = "export"), orientation = "landscape"),
        list(extend = 'print', filename = fileName)
      ),
      columnDefs = list(
        list(visible = F, targets = hiddenColumns),
        list(orderable = F, className = 'details-control', targets = 1)
      )
    ),
    callback = JS(paste0(
      "table.column(1).nodes().to$().css({cursor: 'pointer'});
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
}

renderManhattanEnrichmentTable <- function(manhattanTable) {
  renderEnrichmentTable("manhattan_table",
                        manhattanTable,
                        caption = "Selected Terms",
                        fileName = "gprofiler_manhattan_selected",
                        mode = "Positive Hits",
                        hiddenColumns = c(0, 11, 12),
                        expandableColumn = 11)
}

renderCombinationTable <- function(shinyOutputId, input_table, caption, fileName,
                                   hiddenColumns, filter = 'none') {
  # Column indices after cbind (0-indexed):
  # 0=⊕, 1=Source, 2=Term ID, 3=Function, 4=Term_ID_noLinks, 5=Tools,
  # 6=X², 7=Comb. P-value, 8=Intersection_Hits, 9=Union_Hits, 10=Hit_Summary, 11=Rank
  # Note: With filter="top", DT requires +1 offset for hiddenColumns and JS indices
  output[[shinyOutputId]] <- DT::renderDataTable(
    server = F,
    cbind(' ' = '&oplus;', input_table),
    escape = F,
    filter = filter,
    extensions = c('Buttons'),
    caption = caption,
    options = list(
      scrollX = TRUE,
      "dom" = 'T<"clear">lBfrtip',
      buttons = list(
        list(extend = 'excel', filename = fileName),
        list(extend = 'csv', filename = fileName),
        list(extend = 'copy', filename = fileName),
        list(extend = 'pdf', filename = fileName, exportOptions = list(orthogonal = "export"), orientation = "landscape"),
        list(extend = 'print', filename = fileName)
      ),
      columnDefs = list(
        list(visible = F, targets = hiddenColumns),
        list(orderable = F, className = 'details-control', targets = 1)
      )
    ),
    callback = JS(
      "table.column(1).nodes().to$().css({cursor: 'pointer'});
      let format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\">' +
               '<b>Hit Summary:</b> ' + d[11] + '<br/><br/>' +
               '<b>Intersection Hits (found by all tools):</b><br/>' + d[9] + '<br/><br/>' +
               '<b>Union Hits (found by any tool):</b><br/>' + d[10] + '</div>';
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
      visInteraction(navigationButtons = T, hover = T)
  })
}

renderHeatmap <- function(type_Tool, shinyOutputId, heatmapTable, color,
                          yAxisColumn, xAxisColumn, weightColumn, height,
                          showColorbar = TRUE, colorbarTitle = NULL) {
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
      source = "Heatmap",
      showscale = showColorbar,
      colorbar = list(title = colorbarTitle)
    ) %>%
      layout(xaxis = list(showgrid = F),
             yaxis = list(showgrid = F))
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
      source = "Barchart"
    )
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
      source = "Scatter"
    ) %>%
      layout(
        xaxis = list(title = "-log10Pvalue"),
        yaxis = list(title = "Enrichment Score")
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
      source = "DotPlot"
    ) %>%
      layout(xaxis = list(title = "Gene Ratio"))
  })
}

renderManhattanPlot <- function() {
  output$manhattan <- renderPlotly({
    gprofiler2::gostplot(
      gprofilerResult,
      capped = T,
      interactive = T,
      pal = DATASOURCE_COLORS
    )
  })
}

