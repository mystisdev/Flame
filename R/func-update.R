# =============================================================================
# UI UPDATE FUNCTIONS
# =============================================================================
# Functions for updating Shiny UI elements in response to user actions.
# =============================================================================

# updateVolcanoMetricsConversionText() removed - now in VolcanoInputSession$updateMetricsText()

updateAvailableTools <- function() {
  inputOrganism <- input$functional_enrichment_organism
  if (inputOrganism != "") {
    # always trigger a changed-tool event to correctly update namespaces
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = NULL, selected = character(0))
    taxid <- ORGANISMS[ORGANISMS$print_name == inputOrganism, ]$taxid
    organismShortName <- ORGANISMS[ORGANISMS$print_name == inputOrganism, ]$short_name
    choices <- names(which(sapply(TOOL_ORGANISMS, function(tool) taxid %in% tool)))
    selected <- choices[1]
    if (isSpecialOrganism(organismShortName))
      selected <- SPECIAL_PREFERRED_TOOL[[organismShortName]]
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = choices, selected = selected)
  }
}

isSpecialOrganism <- function(organismShortName) {
  return(organismShortName %in% SPECIAL_ORGANISMS)
}

updateAvailableDatasources <- function() {
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  choices <- getChoicesUnion(toolCapitalNames)
  updatePickerInput(session, "functional_enrichment_datasources",
                    choices = choices, selected = DATASOURCES_DEFAULT_SELECTED)
}

getChoicesUnion <- function(toolCapitalNames) {
  choices <- c()
  for (tool in toolCapitalNames) {
    prefix <- ""
    if (tool == "ENRICHR") {
      prefix <- getEnrichrVariablePrefix()
    }
    if (tool == "GENECODIS") {
      prefix <- getGeneCodisVariablePrefix()
    }
    choices <- c(choices, DATASOURCES[[paste0(prefix, tool)]])
  }
  choices <- filterDatasourcePrintChoices(unique(choices))
  return(choices)
}

filterDatasourcePrintChoices <- function(choices) {
  filtered_DATASOURCES_PRINT <- DATASOURCES_PRINT
  for (i in length(DATASOURCES_PRINT):1) {
    filtered_DATASOURCES_PRINT[[i]][!filtered_DATASOURCES_PRINT[[i]] %in% choices] <- NULL
    if (length(filtered_DATASOURCES_PRINT[[i]]) == 0)
      filtered_DATASOURCES_PRINT[[i]] <- NULL
  }
  return(filtered_DATASOURCES_PRINT)
}

updateAvailableNamespaces <- function() {
  organismShortName <-
    ORGANISMS[ORGANISMS$print_name == input$functional_enrichment_organism, ]$short_name
  if (!is.na(organismShortName)) {
    toolCapitalNames <- toupper(input$functional_enrichment_tool)
    choices <- getNamespaceChoices(toolCapitalNames)
    selected <- choices[1]
    if (isSpecialOrganism(organismShortName) &&
        all(toolCapitalNames == "ENRICHR"))
      selected <- SPECIAL_PREFERRED_NAMESPACE[[organismShortName]]
    updateSelectInput(session, "functional_enrichment_namespace",
                      choices = choices, selected = selected)
  } else {
    shinyjs::disable("functional_enrichment_namespace")
    updateSelectInput(session, "functional_enrichment_namespace",
                      choices = c("ENSEMBL Protein ID" = "ENSP"),
                      selected = "ENSP")
  }
}

getNamespaceChoices <- function(toolCapitalNames) {
  if (length(toolCapitalNames) == 1) {
    shinyjs::enable("functional_enrichment_namespace")
    prefix <- getNamespacePrefix(toolCapitalNames)
    choices <- NAMESPACES[[paste0(prefix, toolCapitalNames)]]

    # If tool doesn't have specific namespaces, use CORE namespaces
    if (is.null(choices)) {
      choices <- NAMESPACES[["CORE"]]
    }

    organismShortName <-
      ORGANISMS[ORGANISMS$print_name == input$functional_enrichment_organism, ]$short_name
    if (isSpecialOrganism(organismShortName) && toolCapitalNames == "GPROFILER")
      choices <- c(NAMESPACES[["SPECIAL"]][[organismShortName]], choices)
  } else {
    shinyjs::disable("functional_enrichment_namespace")
    choices <- DEFAULT_NAMESPACE_TEXT
  }
  return(choices)
}

getNamespacePrefix <- function(toolCapitalNames) {
  prefix <- ""
  if (toolCapitalNames == "ENRICHR") {
    prefix <- switch(
      ORGANISMS[ORGANISMS$print_name ==
                  input[["functional_enrichment_organism"]], ]$short_name,
      "dmelanogaster" = "FLY_",
      "scerevisiae" = "YEAST_"
    )
  }
  return(prefix)
}

updateAvailableSignificanceMetrics <- function() {
  toolCapitalNames <- toupper(input$functional_enrichment_tool)
  options <- getAvailableSignificanceMetrics(toolCapitalNames)
  updateSelectInput(session, "functional_enrichment_metric", choices = options[["choices"]], selected = options[["selected"]])
}

getAvailableSignificanceMetrics <- function(toolCapitalNames) {
  if (length(toolCapitalNames) == 1) {
    shinyjs::enable("functional_enrichment_metric")
    choices <- METRICS[[toolCapitalNames]]
    if(input$functional_enrichment_background_choice== "genome") {
      selected <- DEFAULT_METRICS_GENOME[[toolCapitalNames[1]]]}
    else {
      selected <-  DEFAULT_METRICS_USERBACKGROUND[[toolCapitalNames[1]]] }
  } else {
    shinyjs::disable("functional_enrichment_metric")
    choices <- DEFAULT_METRIC_TEXT
    selected <- DEFAULT_METRIC_TEXT
  }
  options <- list("choices" = choices, "selected" = selected)
  return(options)
}

updateShinySliderInput <- function(shinyOutputId, minSliderValue, maxSliderValue,
                                   value = DEFAULT_SLIDER_VALUE, step = 1) {
  updateSliderInput(
    session, shinyOutputId,
    min = minSliderValue, max = maxSliderValue,
    value = value, step = step
  )
}

# Parameterized versions of control update functions (explicit inputs, no globals)

updatePlotControlPanelsForTool <- function(enrichmentType, tool) {
  type_Tool <- paste(enrichmentType, tool, sep = "_")
  selectedDataSource <- updatePlotDataSourcesForTool(enrichmentType, type_Tool)
  updatePlotSliderInputsForTool(enrichmentType, type_Tool, selectedDataSource)
}

updatePlotDataSourcesForTool <- function(enrichmentType, type_Tool) {
  sources <- switch(
    enrichmentType,
    "functional" = ENRICHMENT_DATASOURCES[
      which(ENRICHMENT_DATASOURCES %in% unique(enrichmentResults[[type_Tool]]$Source))
    ],
    "literature" = "PUBMED"
  )
  selected <- sources[1]

  lapply(ALL_PLOT_IDS, function(plotId) {
    updatePickerInput(
      session, paste(type_Tool, plotId, "sourceSelect", sep = "_"),
      choices = sources, selected = selected
    )
  })
  return(selected)
}

updatePlotSliderInputsForTool <- function(enrichmentType, type_Tool, selectedDataSource) {
  maxSliderValue <- switch(
    enrichmentType,
    "functional" = nrow(enrichmentResults[[type_Tool]][grepl(
      selectedDataSource, enrichmentResults[[type_Tool]]$Source), ]),
    "literature" = nrow(enrichmentResults[[type_Tool]])
  )
  if (maxSliderValue > MAX_SLIDER_VALUE)
    maxSliderValue <- MAX_SLIDER_VALUE

  lapply(ALL_PLOT_IDS, function(plotId) {
    updateShinySliderInput(
      shinyOutputId = paste(type_Tool, plotId, "slider", sep = "_"),
      minSliderValue = 1, maxSliderValue)
  })
  updateShinySliderInput(
    shinyOutputId = paste(type_Tool, "network3_thresholdSlider", sep = "_"),
    minSliderValue = 1, maxSliderValue)
}

updateAvailableStringNamespaces <- function() {
  if (input$string_network_organism != "") {
    if (!is.na(
      ORGANISMS[ORGANISMS$print_name == input$string_network_organism, ]$short_name
    )) {
      shinyjs::enable("STRING_namespace")
      updateSelectInput(session, "STRING_namespace",
                        choices = NAMESPACES[["STRING"]],
                        selected = "ENSP")
    } else {
      shinyjs::disable("STRING_namespace")
      updateSelectInput(session, "STRING_namespace",
                        choices = c("User Input" = "USERINPUT"),
                        selected = "USERINPUT")
    }
  }
}


updateBackgroundMode <- function(choice, enrichmentType) {
  if (choice == "genome") {
    shinyjs::hide(paste0(enrichmentType, "_enrichment_background_container"))
    # Genome background: All enrichment tools are available
    # ENRICHMENT_TOOLS = ["gProfiler", "WebGestalt", "enrichR", "STRING", "PANTHER"]
    # this is only for enrichmentType = 'functional',
    # since 'literature' only has STRING anyway
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = ENRICHMENT_TOOLS, selected = DEFAULT_TOOL)
  }
  else {
    shinyjs::show(paste0(enrichmentType, "_enrichment_background_container"))
    # Custom background: Only tools that support user-provided background lists
    # enrichR is excluded because runEnrichr() does not accept user_reference parameter
    # Available tools: gProfiler, WebGestalt, STRING, PANTHER (all have user_reference parameter)
    # this is only for enrichmentType = 'functional',
    # since 'literature' only has STRING anyway
    updatePickerInput(session, "functional_enrichment_tool",
                      choices = c("gProfiler", "WebGestalt", "STRING", "PANTHER", "GeneCodis"), selected = DEFAULT_TOOL)
  }
  updateAvailableSignificanceMetrics()
}

# TODO: This function uses the old userInputLists global instead of analyteListRegistry.
#       Fix when refactoring the enrichment page - should use:
#         background_choices <- analyteListRegistry$getNames()
#       instead of:
#         background_choices <- names(userInputLists)
updateBackgroundListChoices <- function(enrichmentType) {
  # the code below makes sure that the input list is NOT a choice for the background
  # first, we get the user input value
  userInputVal <- input[[paste0(enrichmentType, "_enrichment_file")]]
  # then, we remove the user input from the list of choices (locally, not globally)
  background_choices <- names(userInputLists)
  background_choices <- background_choices[background_choices != userInputVal]
  updateSelectInput(session, paste0(enrichmentType, "_enrichment_background_list"), choices = background_choices)
}
