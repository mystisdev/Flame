# ============================================================================
# Dynamic Tab Generation Functions for Multi-Run Architecture
# ============================================================================
#
# These functions generate UI components for dynamically created enrichment run tabs.
# All functions are parameterized (no global state modification) to work safely
# when called from the server context during runtime.

# Main function to generate the full tab content for a run
generateToolPanelForRun <- function(enrichmentType, toolName, runNumber) {
  fullRunKey <- paste(enrichmentType, toolName, runNumber, sep = "_")

  return(
    tags$div(
      tags$br(),
      generateParametersBoxForRun(fullRunKey),
      tabsetPanel(
        generateResultsPanelForRun(enrichmentType, toolName, fullRunKey),
        generatePlotsPanelForRun(enrichmentType, toolName, fullRunKey)
      )
    )
  )
}

generateParametersBoxForRun <- function(fullRunKey) {
  box(
    title = "Parameters",
    width = NULL,
    status = "primary",
    solidHeader = T,
    collapsible = T,
    collapsed = T,
    verbatimTextOutput(
      outputId = paste(fullRunKey, "enrichment_parameters", sep = "_")
    )
  )
}

generateResultsPanelForRun <- function(enrichmentType, toolName, fullRunKey) {
  sourcesPanel <- do.call(
    tabsetPanel, c(
      id = paste(fullRunKey, "sources_panel", sep = "_"),
      lapply(TAB_NAMES_CODES, function(tabName) {
        generateEnrichmentTabForRun(fullRunKey, tabName)
      })
    )
  )

  return(
    tabPanel(
      title = "Results",
      icon = icon("table"),
      tags$div(
        id = paste(fullRunKey, "resultsDiv", sep = "_"),
        class = "enrichmentResultsDiv",
        tags$br(),
        sourcesPanel
      ),
      tags$br(),
      tags$div(
        id = paste(fullRunKey, "conversionBoxes", sep = "_"),
        style = "display: none;",
        box(
          title = "Conversion Table",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          tabsetPanel(
            tabPanel("Input List", DT::dataTableOutput(paste(fullRunKey, "conversionTable_input", sep = "_"))),
            tabPanel("Reference Background",
                     div(id = paste(fullRunKey, "conversionTable_genome_div", sep = "_"),
                         h3("No custom background was submitted by the user, the entire selected genome was used instead.")),
                     div(id = paste(fullRunKey, "conversionTable_reference_div", sep = "_"), style = "display:none",
                         DT::dataTableOutput(paste(fullRunKey, "conversionTable_reference", sep = "_")))
            )
          )
        ),
        box(
          title = "Unconverted Inputs",
          class = "conversionBox",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          verbatimTextOutput(paste(fullRunKey, "notConverted_input", sep = "_")),
          tags$hr(),
          div(id = paste(fullRunKey, "notConverted_reference_div", sep = "_"), style = "display:none",
              verbatimTextOutput(paste(fullRunKey, "notConverted_reference", sep = "_"))
          )
        )
      ),
      box(
        title = "No-hit Inputs",
        class = "conversionBox",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        verbatimTextOutput(paste(fullRunKey, "genesNotFound", sep = "_"))
      )
    )
  )
}

generateEnrichmentTabForRun <- function(fullRunKey, tabName) {
  tabPanel(
    title = names(TAB_NAMES[TAB_NAMES == tabName]),
    tags$br(),
    DT::dataTableOutput(paste(fullRunKey, "table", tabName, sep = "_"))
  )
}

# Plots panel generation for multi-run mode
generatePlotsPanelForRun <- function(enrichmentType, toolName, fullRunKey) {
  tabPanel(
    title = "Plots",
    icon = icon("chart-bar"),
    do.call(
      tabsetPanel,
      lapply(PLOT_TABNAMES, function(tabName) {
        generatePlotPanelForRun(tabName, enrichmentType, toolName, fullRunKey)
      })
    )
  )
}

generatePlotPanelForRun <- function(tabName, enrichmentType, toolName, fullRunKey) {
  plotPanelDiv <- switch(
    tabName,
    "Network" = do.call(
      tabsetPanel,
      lapply(NETWORK_IDS, function(plotId) {
        generatePlotPanelOrDivForRun(plotId, enrichmentType, toolName, fullRunKey)
      })
    ),
    "Heatmap" = do.call(
      tabsetPanel,
      lapply(HEATMAP_IDS, function(plotId) {
        generatePlotPanelOrDivForRun(plotId, enrichmentType, toolName, fullRunKey)
      })
    ),
    "Barchart" = generatePlotPanelOrDivForRun("barchart", enrichmentType, toolName, fullRunKey),
    "Scatter Plot" = generatePlotPanelOrDivForRun("scatterPlot", enrichmentType, toolName, fullRunKey),
    "Dot Plot" = generatePlotPanelOrDivForRun("dotPlot", enrichmentType, toolName, fullRunKey),
    "Manhattan" = {
      if (toolName == "gProfiler")
        generateManhattanPanelForRun(fullRunKey)
      else
        NULL
    }
  )

  if (!is.null(plotPanelDiv))
    return(
      tabPanel(
        title = tabName,
        tags$br(),
        plotPanelDiv
      )
    )
  else
    return(NULL)
}

generatePlotPanelOrDivForRun <- function(plotId, enrichmentType, toolName, fullRunKey) {
  # general values
  returnType <- "tabPanel"
  title <- NULL
  multipleDatasources <- T
  plotExtraFluidRow <- NULL
  actionButtonColumns <- 4
  plotDrawFormatControl <- column(
    4,
    radioButtons(
      inputId = paste(fullRunKey, plotId, "drawFormat", sep = "_"),
      label = paste0("Choose format of ",
                     UI_TERM_KEYWORD[[enrichmentType]], " to draw:"),
      choices = list("ID" = "Term_ID", "Name" = "Function"),
      inline = TRUE
    )
  )
  plotExtraControl <- NULL
  plotOutputDiv <- plotlyOutput(paste(fullRunKey, plotId, sep = "_"))
  plotExtraOutputDiv <- NULL

  uiTermKeyword <- stringr::str_to_title(UI_TERM_KEYWORD[[enrichmentType]])

  # conditions
  if (plotId %in% c("barchart", "scatterPlot", "dotPlot"))
    returnType <- "tags$div"
  if (plotId %in% c("network1", "heatmap1"))
    title <- paste0(uiTermKeyword, " Vs Genes")
  if (plotId %in% c("network2", "heatmap2"))
    title <- paste0(uiTermKeyword, " Vs ", uiTermKeyword)
  if (plotId %in% c("network3", "heatmap3"))
    title <- "Genes Vs Genes"
  if (plotId %in% c("heatmap1", "heatmap2", "heatmap3")) {
    multipleDatasources <- F
    plotOutputDiv <- tags$div(
      class = "heatmapOutput",
      plotlyOutput(paste(fullRunKey, plotId, sep = "_"))
    )
  }
  if (plotId %in% c("network1", "network2", "network3")) {
    plotExtraFluidRow <- generateNetworkExtraControlForRun(plotId, enrichmentType, fullRunKey)
    plotExtraOutputDiv <- tags$div(
      tags$br(),
      generateColorCodingLegendForRun(),
      DT::dataTableOutput(paste(fullRunKey, plotId, "edgelist", sep = "_"))
    )
    plotOutputDiv <- tags$div(
      class = "networkOutput",
      visNetworkOutput(paste(fullRunKey, plotId, sep = "_"), height = VIS_NET_HEIGHT)
    )
  }
  if (plotId == "heatmap1") {
    plotExtraControl <- column(
      4,
      tags$div(
        class = "drawUp",
        radioButtons(
          inputId = paste(fullRunKey, plotId, "axis", sep = "_"),
          label = "Axes",
          choices = c(paste0(uiTermKeyword, "-Genes"),
                      paste0("Genes-", uiTermKeyword)),
          inline = TRUE,
        )
      )
    )
  }
  if (plotId == "barchart")
    plotOutputDiv <- tags$div(
      class = "barchartOutput",
      plotlyOutput(paste(fullRunKey, "barchart", sep = "_"))
    )
  if (plotId == "dotPlot")
    plotOutputDiv <- tags$div(
      class = "dotPlotOutput",
      plotlyOutput(paste(fullRunKey, "dotPlot", sep = "_"))
    )
  if (plotId %in% c("network3", "heatmap3", "scatterPlot")) {
    actionButtonColumns <- 12
    plotDrawFormatControl <- NULL
  }

  # Mode choices: add "Gene Ratio" for dotPlot
  modeChoices <- c("-log10Pvalue", "Enrichment Score")
  if (plotId == "dotPlot")
    modeChoices <- c("-log10Pvalue", "Enrichment Score", "Gene Ratio")

  return(
    eval(parse(text = returnType))(
      title = title,
      tags$br(),
      fluidRow(
        column(
          4,
          pickerInput(
            inputId = paste(fullRunKey, plotId, "sourceSelect", sep = "_"),
            label = "Select term datasource(s):",
            choices = NULL, multiple = multipleDatasources,
            options = list('actions-box' = TRUE)
          )
        ),
        column(
          4,
          sliderInput(
            inputId = paste(fullRunKey, plotId, "slider", sep = "_"),
            label = paste0("Filter number of top ",
                           UI_TERM_KEYWORD[[enrichmentType]], ":"),
            min = 1, max = 10, value = 10, step = 1
          ) %>%
            bsplus::shinyInput_label_embed(
              bsplus::shiny_iconlink("circle-info") %>%
                bsplus::bs_embed_popover(
                  title = "Upper cap of 200 terms."
                )
            )
        ),
        column(
          4,
          radioButtons(
            inputId = paste(fullRunKey, plotId, "mode", sep = "_"),
            label = paste0("Order retrieved ",
                           UI_TERM_KEYWORD[[enrichmentType]], " by:"),
            choices = modeChoices,
            inline = TRUE
          )
        )
      ),
      plotExtraFluidRow,
      fluidRow(
        column(
          actionButtonColumns,
          actionButton(
            inputId = paste(fullRunKey, plotId, "button", sep = "_"),
            label = "Generate", icon("palette"), class = "submit_button"
          ),
          if (plotId %in% c("barchart", "scatterPlot", "dotPlot",
                            "heatmap1", "heatmap2", "heatmap3",
                            "network1", "network2", "network3")) {
            actionButton(
              inputId = paste(fullRunKey, plotId, "resetView", sep = "_"),
              label = "Reset View", icon("refresh"), class = "reset_button",
              style = "margin-left: 10px;"
            )
          }
        ),
        plotDrawFormatControl,
        plotExtraControl
      ),
      tags$hr(),
      plotOutputDiv,
      plotExtraOutputDiv,
      tags$br(),
      DT::dataTableOutput(paste(fullRunKey, plotId, "table", sep = "_"))
    )
  )
}

generateNetworkExtraControlForRun <- function(networkId, enrichmentType, fullRunKey) {
  exclusiveNetworkComponent <- switch(
    networkId,
    "network1" = NULL,
    "network2" = sliderInput(
      inputId = paste(fullRunKey, networkId, "thresholdSlider", sep = "_"),
      label = "Similarity score cut-off (%):",
      min = 1, max = 100, value = 10, step = 1
    ) %>%
      bsplus::shinyInput_label_embed(
        bsplus::shiny_iconlink("circle-info") %>%
          bsplus::bs_embed_popover(
            title = "Jaccard-like similarity: (Common Genes / Total Genes) x 100"
          )
      ),
    "network3" = sliderInput(
      inputId = paste(fullRunKey, networkId, "thresholdSlider", sep = "_"),
      label = paste0("Number of common ", UI_TERM_KEYWORD[[enrichmentType]], ":"),
      min = 1, max = 100, value = 10, step = 1
    )
  )

  return(
    fluidRow(
      column(
        4,
        actionButton(
          inputId = paste(fullRunKey, networkId, "arena", sep = "_"),
          label = "Visualize 3D",
          class = "arena_button"
        )
      ),
      column(
        4,
        selectInput(
          inputId = paste(fullRunKey, networkId, "layout", sep = "_"),
          label = "Choose layout algorithm:",
          choices = as.vector(unlist(LAYOUT_CHOICES))
        )
      ),
      column(
        4,
        tags$div(
          class = "drawUp",
          exclusiveNetworkComponent
        )
      )
    )
  )
}

generateManhattanPanelForRun <- function(fullRunKey) {
  return(
    tabPanel(
      title = "Manhattan",
      tags$br(),
      actionButton(inputId = paste(fullRunKey, "manhattan_button", sep = "_"),
                   label = "Generate", icon("palette"), class = "submit_button"),
      tags$hr(),
      plotlyOutput(paste(fullRunKey, "manhattan", sep = "_"), width = "100%", inline = FALSE),
      tags$br(),
      DT::dataTableOutput(paste(fullRunKey, "manhattan_table", sep = "_"))
    )
  )
}

# ============================================================================
# Literature Enrichment Tab Generation (Multi-Run Mode)
# ============================================================================

# Main function to generate the full tab content for a literature enrichment run
generateToolPanelForLiteratureRun <- function(toolName, uniqueId) {
  fullRunKey <- paste("literature", toolName, uniqueId, sep = "_")

  return(
    tags$div(
      tags$br(),
      generateParametersBoxForRun(fullRunKey),
      tabsetPanel(
        generateResultsPanelForLiteratureRun(fullRunKey),
        generatePlotsPanelForRun("literature", toolName, fullRunKey)
      )
    )
  )
}

# Results panel for literature enrichment (PUBMED datasource only)
generateResultsPanelForLiteratureRun <- function(fullRunKey) {
  # Literature enrichment only uses PUBMED datasource
  sourcesPanel <- tabsetPanel(
    id = paste(fullRunKey, "sources_panel", sep = "_"),
    tabPanel("PUBMED", tags$br(),
             DT::dataTableOutput(paste(fullRunKey, "table_pubmed", sep = "_")))
  )

  return(
    tabPanel(
      title = "Results",
      icon = icon("table"),
      tags$div(
        id = paste(fullRunKey, "resultsDiv", sep = "_"),
        class = "enrichmentResultsDiv",
        tags$br(),
        sourcesPanel
      ),
      tags$br(),
      tags$div(
        id = paste(fullRunKey, "conversionBoxes", sep = "_"),
        style = "display: none;",
        box(
          title = "Conversion Table",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          tabsetPanel(
            tabPanel("Input List", DT::dataTableOutput(paste(fullRunKey, "conversionTable_input", sep = "_"))),
            tabPanel("Reference Background",
                     div(id = paste(fullRunKey, "conversionTable_genome_div", sep = "_"),
                         h3("No custom background was submitted by the user, the entire selected genome was used instead.")),
                     div(id = paste(fullRunKey, "conversionTable_reference_div", sep = "_"), style = "display:none",
                         DT::dataTableOutput(paste(fullRunKey, "conversionTable_reference", sep = "_")))
            )
          )
        ),
        box(
          title = "Unconverted Inputs",
          class = "conversionBox",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          verbatimTextOutput(paste(fullRunKey, "notConverted_input", sep = "_")),
          tags$hr(),
          div(id = paste(fullRunKey, "notConverted_reference_div", sep = "_"), style = "display:none",
              verbatimTextOutput(paste(fullRunKey, "notConverted_reference", sep = "_"))
          )
        )
      ),
      box(
        title = "No-hit Inputs",
        class = "conversionBox",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        verbatimTextOutput(paste(fullRunKey, "genesNotFound", sep = "_"))
      )
    )
  )
}

# ============================================================================
# Shared Helper Functions
# ============================================================================

# Color coding legend for network plots (extracted to avoid global dependency)
generateColorCodingLegendForRun <- function() {
  fluidRow(
    do.call(
      box, c(
        class = "legend", title = "Legend", status = "primary",
        solidHeader = T, width = 12, collapsible = T, collapsed = T,
        lapply(LEGEND_ITEMS, function(legendList) {
          do.call(
            column, c(
              width = 2,
              lapply(legendList, function(source) {
                fontColor <- "white"
                if (source %in% c("GENE", "DO", "GO:BP", "UNIPROT",
                                  "DISGENET", "OMIM", "BTO", "WBP"))
                  fontColor <- "black"
                tags$div(
                  tags$p(source),
                  style = paste0(
                    "background-color: ", DATASOURCE_COLORS[source][[1]],
                    "; color: ", fontColor, ";"
                  )
                )
              })
            )
          )}
        )
      )
    )
  )
}

# ============================================================================
# Output Registration for Cleanup
# ============================================================================

# Register all outputs created for an enrichment run tab
# Called immediately after tab is generated in handleEnrichmentRun()
registerOutputsForRun <- function(fullRunKey) {
  runInfo <- parseFullRunKey(fullRunKey)
  enrichmentType <- runInfo$enrichmentType
  toolName <- runInfo$toolName

  # Parameters box
  outputRegistry$registerOutput(fullRunKey,
    paste(fullRunKey, "enrichment_parameters", sep = "_"), "text")

  # No-hit genes
  outputRegistry$registerOutput(fullRunKey,
    paste(fullRunKey, "genesNotFound", sep = "_"), "text")

  # Conversion boxes
  outputRegistry$registerOutputs(fullRunKey, c(
    paste(fullRunKey, "conversionTable_input", sep = "_"),
    paste(fullRunKey, "conversionTable_reference", sep = "_")
  ), "datatable")
  outputRegistry$registerOutputs(fullRunKey, c(
    paste(fullRunKey, "notConverted_input", sep = "_"),
    paste(fullRunKey, "notConverted_reference", sep = "_")
  ), "text")

  # Result tables (all datasources)
  if (enrichmentType == "functional") {
    for (tabCode in TAB_NAMES_CODES) {
      outputRegistry$registerOutput(fullRunKey,
        paste(fullRunKey, "table", tabCode, sep = "_"), "datatable")
    }
  } else {
    # Literature enrichment: PUBMED only
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, "table_pubmed", sep = "_"), "datatable")
  }

  # Simple plots (barchart, scatter, dot)
  for (plotId in c("barchart", "scatterPlot", "dotPlot")) {
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, plotId, sep = "_"), "plotly")
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, plotId, "table", sep = "_"), "datatable")
  }

  # Heatmaps
  for (heatmapId in HEATMAP_IDS) {
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, heatmapId, sep = "_"), "plotly")
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, heatmapId, "table", sep = "_"), "datatable")
  }

  # Networks
  for (networkId in NETWORK_IDS) {
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, networkId, sep = "_"), "visNetwork")
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, networkId, "table", sep = "_"), "datatable")
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, networkId, "edgelist", sep = "_"), "datatable")
  }

  # Manhattan (gProfiler only)
  if (toolName == "gProfiler") {
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, "manhattan", sep = "_"), "plotly")
    outputRegistry$registerOutput(fullRunKey,
      paste(fullRunKey, "manhattan_table", sep = "_"), "datatable")
  }
}

# Register combination tab outputs
# Called when combination tab is first shown
registerCombinationOutputs <- function() {
  key <- COMBINATION_REGISTRY_KEY
  outputRegistry$registerOutput(key, "combo_table", "datatable")
  outputRegistry$registerOutput(key, "combo_upsetClick_table", "datatable")
  outputRegistry$registerOutput(key, "combo_network_table", "datatable")
  outputRegistry$registerOutput(key, "combo_visNetwork", "visNetwork")
  outputRegistry$registerOutput(key, "upsetjsCombo", "upsetjs")
}
