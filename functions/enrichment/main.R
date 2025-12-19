handleEnrichment <- function(enrichmentType) {
  tryCatch({
    if (existInputGeneLists()) {
      currentEnrichmentType <<- enrichmentType
      # Get config for this enrichment type
      config <- getEnrichmentConfig(enrichmentType)

      if (existInputOrganism()) {
        if (existEnrichmentTool()) {
          tools <- switch(
            currentEnrichmentType,
            "functional" = input$functional_enrichment_tool,
            "literature" = "STRING"
          )

          currentUserList <<-
            unlist(userInputLists[names(userInputLists) ==
                                    input[[paste0(currentEnrichmentType,
                                                  "_enrichment_file")]]][[1]])
          currentOrganism <<-
            ORGANISMS[ORGANISMS$print_name ==
                                  input[[paste0(currentEnrichmentType,
                                                "_enrichment_organism")]], ]$taxid

          if(input[[paste0(currentEnrichmentType, "_enrichment_background_choice")]] == "genome") {
            currentBackgroundList <<- c()
          }
          else {
            currentBackgroundList <<-  unlist(userInputLists[names(userInputLists) ==
                                    input[[paste0(currentEnrichmentType,
                                                  "_enrichment_background_list")]]][[1]])
          }

          lapply(tools, function(toolName) {
            currentEnrichmentTool <<- toolName
            currentSignificanceMetric <<- decideToolMetric()

            # Multi-run: Check for matching parameters (unified for all types)
            currentParams <- captureRunParameters()
            matchResult <- findMatchingRun(currentEnrichmentType, toolName, currentParams)

            if (!is.null(matchResult$matchType) && matchResult$matchType == "exact") {
              # EXACT MATCH: Just select tab and pulse it (no API call)
              runInfo <- activeRuns[[matchResult$fullRunKey]]
              updateTabsetPanel(session, config$tabsetPanelId, selected = runInfo$runId)
              session$sendCustomMessage("handler_pulseTab", runInfo$runId)
              return()
            }

            if (!is.null(matchResult$matchType) && matchResult$matchType == "datasources_differ") {
              # DATASOURCES DIFFER: Update existing tab with new results
              existingRunKey <- matchResult$fullRunKey
              runInfo <- activeRuns[[existingRunKey]]

              # Use existing run's identifiers (don't create new numbers)
              currentUniqueId <<- runInfo$uniqueId
              currentRunNumber <<- runInfo$runNumber
              currentFullRunKey <<- existingRunKey
              currentType_Tool <<- existingRunKey

              # Update stored parameters with new datasources
              updateRunParameters(existingRunKey, currentParams)
              clearRunResults(existingRunKey)

              # Run enrichment (will populate existing tab)
              handleEnrichmentRun(isUpdate = TRUE)
              return()
            }

            # NO MATCH: Create new run
            currentUniqueId <<- getNextUniqueId(toolName)
            currentRunNumber <<- getNextRunNumber(toolName)
            runId <- createRunId(toolName, currentUniqueId)
            currentFullRunKey <<- getFullRunKey(currentEnrichmentType, runId)
            currentType_Tool <<- currentFullRunKey

            # Register the run
            registerRun(currentEnrichmentType, toolName, currentUniqueId, currentRunNumber, currentParams)
            handleEnrichmentRun(isUpdate = FALSE)
          })

          # Update combination tab after UI is flushed to ensure DOM elements are ready
          if (config$supportsCombination) {
            session$onFlushed(function() {
              prepareCombinationTab()
            }, once = TRUE)
          }
        }
      }
    }
  }, error = function(e) {
    cat(paste("Enrichment analysis error: ", e))
    renderError(e$message)
  }, finally = {
    removeModal()
  })
}

existInputGeneLists <- function() {
  exist <- F
  if (length(userInputLists) > 0)
    exist <- T
  else
    renderWarning("Upload an input list first.")
  return(exist)
}

existInputOrganism <- function() {
  notEmpty <- F
  if (input[[paste0(currentEnrichmentType, "_enrichment_organism")]] != "")
    notEmpty <- T
  else
    renderWarning("Select an organism from the list.")
  return(notEmpty)
}

existEnrichmentTool <- function() {
  exist <- F
  if (!is.null(input[[paste0(currentEnrichmentType, "_enrichment_tool")]]))
    exist <- T
  else
    renderWarning("Select at least one enrichment tool.")
  return(exist)
}

decideToolMetric <- function() {
  if (input[[paste0(currentEnrichmentType, "_enrichment_metric")]] == DEFAULT_METRIC_TEXT) {
    if(input[[paste0(currentEnrichmentType, "_enrichment_background_choice")]] == "genome")
      currentSignificanceMetric <<- DEFAULT_METRICS_GENOME[[toupper(currentEnrichmentTool)]]
    else
      currentSignificanceMetric <<- DEFAULT_METRICS_USERBACKGROUND[[toupper(currentEnrichmentTool)]]
  }
  else
    currentSignificanceMetric <<-
      input[[paste0(currentEnrichmentType, "_enrichment_metric")]]
}

# Unified multi-run handler for all enrichment types (config-driven)
# Uses ENRICHMENT_TYPES_CONFIG to handle differences between functional/literature/future types
# isUpdate: TRUE when updating existing tab with new datasources, FALSE for new run
handleEnrichmentRun <- function(isUpdate = FALSE) {
  # Get config for current enrichment type
  config <- getEnrichmentConfig(currentEnrichmentType)

  actionText <- if (isUpdate) "Updating" else "Executing"
  renderModal(
    paste0(
      "<h2>Please wait.</h2><br />
      <p>", actionText, " ", config$label, " with ", currentEnrichmentTool,
      " (Run ", currentRunNumber, ").</p>"
    )
  )

  if (existDataSources()) {
    # Initialize empty result storage for this run
    enrichmentResults[[currentFullRunKey]] <<- data.frame()
    for (networkId in NETWORK_IDS) {
      arenaEdgelist[[paste(currentFullRunKey, networkId, sep = "_")]] <<- data.frame()
    }

    # Convert input gene symbols to tool-specific format
    inputGenesConversionTable <- geneConvert(currentUserList)
    if (validInputGenesConversionTable(inputGenesConversionTable)) {
      if(length(currentBackgroundList)==0)
        backgroundGenesConversionTable <- NULL
      else
        backgroundGenesConversionTable <- geneConvert(currentBackgroundList)

      # Run enrichment analysis
      runEnrichmentAnalysis(inputGenesConversionTable$target, backgroundGenesConversionTable$target)

      if (validEnrichmentResultMultiRun()) {
        if (!isUpdate) {
          # Create dynamic tab for this run (only for new runs)
          insertEnrichmentTab(config)
        }

        # Hide all source tabs initially (they'll be shown as results populate)
        hideAllSourceTabsForRun(currentFullRunKey)

        if (!isUpdate) {
          # Show the results panel if this is the first run
          if (getActiveRunCount(currentEnrichmentType) == 1) {
            shinyjs::show(config$resultsPanelId)
            shinyjs::show(config$clearButtonId)
            # For functional enrichment, ensure Combination tab stays hidden initially
            if (config$supportsCombination) {
              hideTab(inputId = config$tabsetPanelId, target = "Combination")
            }
          }

          # Register dynamic observers AFTER UI is flushed to the client
          local({
            runKeyForObservers <- currentFullRunKey
            session$onFlushed(function() {
              registerObserversForRun(runKeyForObservers)
            }, once = TRUE)
          })
        }

        noHitGenesCheckList <- executeNamespaceRollback(inputGenesConversionTable)
        printParametersMultiRun()

        # For new runs, defer shinyjs::show until UI is flushed (element must exist in DOM)
        if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          if (!isUpdate) {
            local({
              runKey <- currentFullRunKey
              inputTable <- inputGenesConversionTable
              bgTable <- backgroundGenesConversionTable
              session$onFlushed(function() {
                shinyjs::show(paste(runKey, "conversionBoxes", sep = "_"))
                printUnconvertedGenes(inputTable, bgTable, runKey = runKey)
                printConversionTable(inputTable, bgTable, runKey = runKey)
              }, once = TRUE)
            })
          } else {
            # Update mode: UI already exists
            shinyjs::show(paste(currentFullRunKey, "conversionBoxes", sep = "_"))
            printUnconvertedGenes(inputGenesConversionTable, backgroundGenesConversionTable, runKey = currentFullRunKey)
            printConversionTable(inputGenesConversionTable, backgroundGenesConversionTable, runKey = currentFullRunKey)
          }
        }

        findAndPrintNoHitGenes(noHitGenesCheckList, runKey = currentFullRunKey)
        printResultTables(runKey = currentFullRunKey)

        runId <- createRunId(currentEnrichmentTool, currentUniqueId)

        # Update plot control panels after UI is flushed to ensure DOM elements exist
        # This works for both new runs and datasource-differ updates
        local({
          runKeyForUpdate <- currentFullRunKey
          session$onFlushed(function() {
            updatePlotControlPanelsForRun(runKeyForUpdate)
          }, once = TRUE)
        })

        updateTabsetPanel(session, config$tabsetPanelId, selected = runId)

        # Pulse the tab to indicate results are ready
        session$sendCustomMessage("handler_pulseTab", runId)
      } else {
        # Enrichment failed
        if (!isUpdate) {
          unregisterRun(currentFullRunKey)
        }
      }
    } else {
      # Conversion failed
      if (!isUpdate) {
        unregisterRun(currentFullRunKey)
      }
    }
  } else {
    # No datasources
    if (!isUpdate) {
      unregisterRun(currentFullRunKey)
    }
  }
}

# Legacy alias for backward compatibility (remove after full migration)
handleEnrichmentWithToolMultiRun <- handleEnrichmentRun

# Unified tab insertion function for all enrichment types (config-driven)
insertEnrichmentTab <- function(config = NULL) {
  if (is.null(config)) {
    config <- getEnrichmentConfig(currentEnrichmentType)
  }

  # Use uniqueId for internal Shiny IDs (prevents caching bugs)
  runId <- createRunId(currentEnrichmentTool, currentUniqueId)

  # Use displayNumber (currentRunNumber) for user-visible tab title
  tabTitle <- paste0(currentEnrichmentTool, " (", currentRunNumber, ")")

  # Generate tab content using the appropriate generator from config
  tabContent <- if (config$generateTabContent == "generateToolPanelForRun") {
    generateToolPanelForRun("functional", currentEnrichmentTool, currentUniqueId)
  } else {
    generateToolPanelForLiteratureRun(currentEnrichmentTool, currentUniqueId)
  }

  # Create tab title with close button (using config for event name)
  tabTitleHtml <- tags$span(
    tabTitle,
    tags$button(
      class = "close-run-tab",
      type = "button",
      onclick = paste0("event.stopPropagation(); Shiny.setInputValue('",
                       config$closeEvent, "', '", runId, "', {priority: 'event'});"),
      icon("times")
    )
  )

  # Insert the tab (using config for panel ID)
  insertTab(
    inputId = config$tabsetPanelId,
    tab = tabPanel(
      title = tabTitleHtml,
      value = runId,
      tabContent
    ),
    select = TRUE
  )
}

# Legacy alias for backward compatibility
insertResultTabForRun <- function() {
  insertEnrichmentTab(getEnrichmentConfig("functional"))
}

# Legacy aliases for backward compatibility (use unified functions above)
handleEnrichmentWithToolLiteratureMultiRun <- handleEnrichmentRun
insertResultTabForLiteratureRun <- function() {
  insertEnrichmentTab(getEnrichmentConfig("literature"))
}
getActiveLiteratureRunCount <- function() {
  getActiveRunCount("literature")
}

existDataSources <- function() {
  exist <- F
  if (!is.null(input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]))
    exist <- T
  else
    renderWarning("Select at least one datasource.")
  return(exist)
}

geneConvert <- function(geneList) {
  # Convert gene symbols to tool-specific identifier format required by enrichment APIs
  # Returns data.frame with columns: input (original), target (converted), name (description)

  currentNamespace <<- input[[paste0(currentEnrichmentType, "_enrichment_namespace")]]
  if (currentNamespace == DEFAULT_NAMESPACE_TEXT)
    currentNamespace <<- getDefaultTargetNamespace()

  if (currentNamespace != "USERINPUT") {
    if (currentEnrichmentTool == "STRING") {
      # For STRING: Convert to STRING format via STRING's get_string_ids API
      # Input: ["RPL23", "TPR"] -> Output: ["9606.ENSP00000420311", "9606.ENSP00000360532"]
      inputGenesConversionTable <- stringPOSTConvertENSP(geneList, currentOrganism)
    } else if (currentEnrichmentTool == "PANTHER") {
      # For PANTHER: Use PANTHER's geneinfo API for gene mapping
      # Input: ["FSD1L", "LTA4H"] -> Output: ["HUMAN|HGNC=13753|UniProtKB=Q9BXM9", "HUMAN|HGNC=6710|UniProtKB=P09960"]
      inputGenesConversionTable <- pantherPOSTConvert(geneList, currentOrganism)
    } else if (currentEnrichmentTool == "GeneCodis") {
      # For GeneCodis: No conversion needed - accepts multiple ID formats and auto-recognizes them
      # Pass genes through as-is (like USERINPUT)
      inputGenesConversionTable <- data.frame(
        "input" = geneList,
        "target" = geneList,
        "name" = geneList
      )
    } else {
      # For gProfiler, WebGestalt, enrichR: Use g:Profiler conversion to target namespace
      # Examples: ENTREZGENE_ACC, ENSEMBL, etc. (depends on tool requirements)
      inputGenesConversionTable <- gProfilerConvert(geneList, currentNamespace)
    }
  } else {
    # USERINPUT: No conversion needed - pass genes through as-is
    inputGenesConversionTable <- data.frame(
      "input" = geneList,
      "target" = geneList,
      "name" = geneList
    )
  }

  return(inputGenesConversionTable)
}

getDefaultTargetNamespace <- function() {
  shortName <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name
  switch(
    currentEnrichmentTool,
    "STRING" = "ENSP",
    "gProfiler" = "USERINPUT",
    "WebGestalt" = "ENTREZGENE_ACC",
    "PANTHER" = "PANTHER_ACC",
    "GeneCodis" = "USERINPUT",
    "enrichR" = {
      if (shortName == "scerevisiae" || shortName == "dmelanogaster")
        "USERINPUT"
      else
        "ENTREZGENE"
    }
  )
}

stringPOSTConvertENSP <- function(userList, organism) {
  url <- "https://string-db.org/api/json/get_string_ids"
  params <- list(
    "identifiers" = paste0(userList, collapse = "%0d"),
    "species" = organism
  )
  request <- httr::POST(url, body = params)
  if (isPOSTResponseValid(request)) {
    inputGenesConversionTable <- jsonlite::fromJSON(
      rawToChar(httr::content(request, "raw")))
    inputGenesConversionTable <-
      inputGenesConversionTable[, c("queryItem", "stringId", "preferredName")]
    colnames(inputGenesConversionTable) <- c("input", "target", "name")
  } else
    inputGenesConversionTable <- NULL
  return(inputGenesConversionTable)
}

pantherPOSTConvert <- function(userList, organism) {
  url <- "https://pantherdb.org/services/oai/pantherdb/geneinfo"
  params <- list(
    "geneInputList" = paste0(userList, collapse = ","),
    "organism" = organism
  )
  request <- httr::POST(url, body = params, encode = "form")
  if (isPOSTResponseValid(request)) {
    pantherResponse <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))

    # Extract mapped genes from PANTHER response
    if ("search" %in% names(pantherResponse) && "mapped_genes" %in% names(pantherResponse$search)) {
      mappedGenes <- pantherResponse$search$mapped_genes$gene
      if (length(mappedGenes) > 0) {
        inputGenesConversionTable <- data.frame(
          input = mappedGenes$mapped_id_list,
          target = mappedGenes$accession,  # PANTHER accession like "HUMAN|HGNC=6710|UniProtKB=P09960"
          name = mappedGenes$sf_name,
          stringsAsFactors = FALSE
        )
      } else {
        inputGenesConversionTable <- data.frame(
          input = character(0), target = character(0), name = character(0)
        )
      }
    } else {
      inputGenesConversionTable <- data.frame(
        input = character(0), target = character(0), name = character(0)
      )
    }
  } else {
    inputGenesConversionTable <- NULL
  }
  return(inputGenesConversionTable)
}

gProfilerConvert <- function(geneList, target) {
  inputGenesConversionTable <- gprofiler2::gconvert(
    geneList,
    organism = ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name,
    target = target, mthreshold = 1, filter_na = T
  )
  inputGenesConversionTable <- inputGenesConversionTable[, c("input", "target", "name")]
  return(inputGenesConversionTable)
}

validInputGenesConversionTable <- function(inputGenesConversionTable) {
  valid <- F
  if (!is.null(inputGenesConversionTable))
    valid <- T
  else
    renderWarning("No valid genes for analysis found.")
  return(valid)
}

runEnrichmentAnalysis <- function(userInputList, user_reference = NULL) {
  tool <- toupper(currentEnrichmentTool)
  if (tool == "GPROFILER") {
    runGprofiler(userInputList, user_reference)
  } else if (tool == "WEBGESTALT") {
    runWebgestalt(userInputList, user_reference)
  } else if (tool == "ENRICHR") {
    runEnrichr(userInputList)
  } else if (tool == "STRING") {
    runString(userInputList, currentOrganism, user_reference)
  } else if (tool == "PANTHER") {
    runPanther(userInputList, currentOrganism, user_reference)
  } else if (tool == "GENECODIS") {
    runGeneCodis(userInputList, currentOrganism, user_reference)
  }
}

validEnrichmentResult <- function() {
  valid <- F
  enrichmentResult <- getGlobalEnrichmentResult(currentEnrichmentType,
                                                currentEnrichmentTool)
  if (nrow(enrichmentResult) > 0)
    valid <- T
  else {
    renderWarning(paste0(
      stringr::str_to_title(currentEnrichmentType), " enrichment with ",
      currentEnrichmentTool, " could not return any valid results."))
    hideTab(inputId = "toolTabsPanel", target = currentEnrichmentTool)
  }
  return(valid)
}

getGlobalEnrichmentResult <- function(enrichmentType, toolName) {
  return(enrichmentResults[[paste(enrichmentType, toolName, sep = "_")]])
}

executeNamespaceRollback <- function(inputGenesConversionTable) {
  # Determine whether to roll back converted IDs to original input names
  rollBackNamesFlag <- ifelse(input[[paste0(currentEnrichmentType,
                                            "_enrichment_inputConversion")]] ==
                                "Original input names", T, F)

  # Use appropriate key based on enrichment type
  resultKey <- if (currentEnrichmentType == "functional") currentFullRunKey else currentType_Tool

  if (rollBackNamesFlag) {
    # Convert Positive Hits back from tool-specific IDs to original gene symbols
    enrichmentResults[[resultKey]] <<-
      rollBackConvertedNames(enrichmentResults[[resultKey]],
                             inputGenesConversionTable)

    # For no-hit calculation: use original gene symbols that successfully converted
    noHitGenesCheckList <- inputGenesConversionTable$input
  } else {
    # For no-hit calculation: use tool-specific IDs (not rolled back)
    noHitGenesCheckList <- inputGenesConversionTable$target
  }

  return(noHitGenesCheckList)
}

rollBackConvertedNames <- function(enrichmentOutput, inputGenesConversionTable) {
  # Check if all Positive Hits are empty (safety check for tools without gene lists)
  all_empty <- all(enrichmentOutput$`Positive Hits` == "" | is.na(enrichmentOutput$`Positive Hits`))

  if (all_empty) {
    # For tools without gene lists, just return the original data unchanged
    return(enrichmentOutput)
  }

  enrichmentOutput <- tidyr::separate_rows(enrichmentOutput,
                                           `Positive Hits`, sep = ",\\s*")
  enrichmentOutput <- merge(enrichmentOutput, inputGenesConversionTable,
                            by.x = "Positive Hits", by.y = "target")
  enrichmentOutput <-
    enrichmentOutput[, !(names(enrichmentOutput) %in% c("Positive Hits", "name"))]
  colnames(enrichmentOutput)[match("input", colnames(enrichmentOutput))] <-
    "Positive Hits"
  enrichmentOutput <- enrichmentOutput %>% group_by(Term_ID) %>%
    mutate(`Positive Hits` = paste(`Positive Hits`, collapse = ","))
  if (currentEnrichmentTool == "WebGestalt") # Term_ID_noLinks already generated
    enrichmentOutput <-
    enrichmentOutput[, c("Source", "Term_ID", "Function",  "P-value",
                         "-log10Pvalue", "Term Size", "Query size",
                         "Intersection Size", "Enrichment Score %",
                         "Positive Hits", "Term_ID_noLinks")]
  return(as.data.frame(distinct(enrichmentOutput)))
}

printParameters <- function() {
  parametersOutput <- paste0(
    "File: ", input[[paste0(currentEnrichmentType, "_enrichment_file")]],
    "\nOrganism: ", ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$print_name,
    "\nBackground: ", input[[
      paste0(currentEnrichmentType, "_enrichment_background_choice")]],
    "\nBackground size (no. of genes): ", enrichmentBackgroundSizes[[toupper(currentType_Tool)]],
    "\nDatasources: ", decideToolSelectedDatasources(),
    "\nNamespace: ", currentNamespace,
    "\nSignificance metric: ", currentSignificanceMetric, 
    "\nSignificance threshold: ", input[[
      paste0(currentEnrichmentType, "_enrichment_threshold")]]
  )
  renderShinyText(paste(
    currentType_Tool, "enrichment_parameters", sep = "_"), parametersOutput)
}

decideToolSelectedDatasources <- function() {
  inputSelectedDatasources <-
    input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  prefix <- ""
  if (currentEnrichmentTool == "enrichR")
    prefix <- getEnrichrVariablePrefix()
  toolDatasources <- DATASOURCES[[paste0(prefix, toupper(currentEnrichmentTool))]]
  inputSelectedDatasources <-
    inputSelectedDatasources[inputSelectedDatasources %in% toolDatasources]
  inputSelectedDatasources <- paste(inputSelectedDatasources, collapse = ", ")
  return(inputSelectedDatasources)
}

# Helper function for rendering gene report text
# Reduces duplication across gene reporting functions
renderGeneReport <- function(outputId, genes, messageTemplate) {
  count <- length(genes)
  if (count > 0) {
    geneList <- paste(genes, collapse = ", ")
    message <- sprintf(messageTemplate, count, geneList)
    renderShinyText(outputId, message)
  } else {
    renderShinyText(outputId, "-")
  }
}

printUnconvertedGenes <- function(convertedInputs, convertedOutputs = NULL,
                                   runKey = currentType_Tool) {
  # Display genes that failed conversion to target namespace
  # Note: This is different from "No-hit Inputs" which are genes that
  # successfully converted but don't appear in any enriched term
  #
  # @param runKey The run key for element IDs (defaults to currentType_Tool for
  #               backwards compatibility, pass currentFullRunKey for multi-run)

  # Report unconverted input genes
  inputOutputId <- paste(runKey, "notConverted_input", sep = "_")
  unconvertedInputs <- currentUserList[!currentUserList %in% convertedInputs$input]
  renderGeneReport(
    outputId = inputOutputId,
    genes = unconvertedInputs,
    messageTemplate = "%d input item(s) could not be converted to the target namespace:\n%s"
  )

  # Report unconverted background genes (if provided)
  refOutputId <- paste(runKey, "notConverted_reference", sep = "_")
  refDivId <- paste(runKey, "notConverted_reference_div", sep = "_")

  if (!is.null(convertedOutputs)) {
    unconvertedBackground <- currentBackgroundList[!currentBackgroundList %in% convertedOutputs$input]
    renderGeneReport(
      outputId = refOutputId,
      genes = unconvertedBackground,
      messageTemplate = "%d reference background item(s) could not be converted to the target namespace:\n%s"
    )
    shinyjs::show(refDivId)
  } else {
    shinyjs::hide(refDivId)
  }
}

printConversionTable <- function(inputConversionTable, backgroundConversionTable = NULL,
                                  runKey = currentType_Tool) {
  # Display gene conversion table for input list and optional background list
  # @param runKey The run key for element IDs (defaults to currentType_Tool for
  #               backwards compatibility, pass currentFullRunKey for multi-run)

  # Render input list conversion table
  shinyOutputId <- paste(runKey, "conversionTable_input", sep = "_")
  fileName <- paste(runKey, "conversion_table", sep = "_")
  colnames(inputConversionTable) <- c("Input", "Target", "Name")
  renderShinyDataTable(shinyOutputId, inputConversionTable, fileName = fileName)

  # Handle reference background conversion table
  genomeDivId <- paste(runKey, "conversionTable_genome_div", sep = "_")
  refDivId <- paste(runKey, "conversionTable_reference_div", sep = "_")

  if (is.null(backgroundConversionTable)) {
    shinyjs::show(genomeDivId)
    shinyjs::hide(refDivId)
  } else {
    shinyjs::hide(genomeDivId)
    shinyOutputId <- paste(runKey, "conversionTable_reference", sep = "_")
    fileName <- paste(runKey, "conversion_table_reference", sep = "_")
    colnames(backgroundConversionTable) <- c("Input", "Target", "Name")
    renderShinyDataTable(shinyOutputId, backgroundConversionTable, fileName = fileName)
    shinyjs::show(refDivId)
  }
}

findAndPrintNoHitGenes <- function(convertedInputs, runKey = NULL) {
  # Find and print genes not found in any enriched term
  # @param runKey The run key for looking up results (NULL uses legacy lookup)
  noHitGenes <- findNoHitGenes(convertedInputs, runKey)
  printNoHitGenes(noHitGenes, runKey)
}

findNoHitGenes <- function(convertedInputs, runKey = NULL) {
  # Find genes that successfully converted but don't appear in any enriched term
  #
  # convertedInputs: List of genes that successfully converted (either original symbols
  #                  or tool-specific IDs, depending on rollback setting)
  # runKey: The run key for looking up enrichment results. If NULL, uses legacy
  #         lookup via getGlobalEnrichmentResult()
  #
  # Logic:
  # 1. Collect all genes appearing in Positive Hits across ALL enriched terms
  # 2. Compare convertedInputs against these "hit" genes
  # 3. Return genes that converted successfully but aren't in any term
  #
  # Note: This is different from "Unconverted Inputs" which are genes that
  # failed at the conversion step entirely

  if (is.null(runKey)) {
    # Legacy mode: use type_tool lookup
    enrichmentResult <- getGlobalEnrichmentResult(currentEnrichmentType, currentEnrichmentTool)
  } else {
    # Multi-run mode: use direct runKey lookup
    enrichmentResult <- enrichmentResults[[runKey]]
  }

  # Collect all genes from Positive Hits column across all enriched terms
  allHitGenes <- paste(enrichmentResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)

  # Find genes that converted but don't appear in any enriched term
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenes <- function(noHitGenes, runKey = currentType_Tool) {
  # Display genes not found in any enriched term
  # @param runKey The run key for element IDs (defaults to currentType_Tool for
  #               backwards compatibility, pass currentFullRunKey for multi-run)
  shinyOutputId <- paste(runKey, "genesNotFound", sep = "_")
  renderGeneReport(
    outputId = shinyOutputId,
    genes = noHitGenes,
    messageTemplate = "%d input item(s) not found in any result term:\n%s"
  )
}

printResultTables <- function(runKey = currentType_Tool) {
  # Print all result tables for an enrichment run
  # @param runKey The run key for element IDs and result lookup
  formatResultTable(runKey)
  switch(
    currentEnrichmentType,
    "functional" = printFunctionalResultTable(runKey),
    "literature" = printLiteratureResultTable(runKey)
  )
}

formatResultTable <- function(runKey = currentType_Tool) {
  # Format and sort enrichment results, add database links
  # @param runKey The run key for result lookup
  enrichmentResults[[runKey]] <<-
    enrichmentResults[[runKey]][order(
      -enrichmentResults[[runKey]]$`-log10Pvalue`), ]

  if (is.null(enrichmentResults[[runKey]]$Term_ID_noLinks)) {
    enrichmentResults[[runKey]]$Term_ID_noLinks <<-
      enrichmentResults[[runKey]]$Term_ID

    switch(
      currentEnrichmentType,
      "functional" = attachDBLinks(runKey),
      "literature" = attachLinks("PUBMED", "https://pubmed.ncbi.nlm.nih.gov/", gSub = "PMID:")
    )
  }
}

printFunctionalResultTable <- function(runKey = currentType_Tool) {
  # Print functional enrichment result tables for all datasources
  # @param runKey The run key for element IDs and result lookup
  shinyOutputId <- paste(runKey, "table_all", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "all", runKey)
  datasources <- input$functional_enrichment_datasources
  lapply(datasources, function(datasource) {
    partialId <- as.character(TAB_NAMES[datasource])
    shinyOutputId <- paste(runKey, "table", partialId, sep = "_")
    tabPosition <- match(datasource, ENRICHMENT_DATASOURCES)
    printResultTable(shinyOutputId, tabPosition, datasource, runKey)
  })
}

printResultTable <- function(shinyOutputId, tabPosition, datasource,
                              runKey = currentType_Tool) {
  # Print a single result table for a datasource
  # @param runKey The run key for result lookup
  if (datasource == "all" || datasource == "pubmed") {
    transformedResultPartial <- enrichmentResults[[runKey]]
  } else {
    pattern <- paste0("^", datasource, "$")
    matches <- grepl(pattern, enrichmentResults[[runKey]]$Source)
    transformedResultPartial <- enrichmentResults[[runKey]][matches, ]
  }

  if (nrow(transformedResultPartial) > 0) {
    transformedResultPartial$`Positive Hits` <-
      gsub(",", ", ", transformedResultPartial$`Positive Hits`)

    showSourceTabForRun(runKey, datasource)

    caption = "Enrichment Results"
    fileName <- paste(runKey, datasource, sep = "_")
    mode <- "Positive Hits"
    # With rownames=FALSE: 0=⊕, ..., 10=Positive Hits, 11=Term_ID_noLinks
    hiddenColumns <- c(10, 11)
    expandableColumn <- 10

    # Convert Source to factor for dropdown filtering (instead of text search)
    transformedResultPartial$Source <- as.factor(transformedResultPartial$Source)

    # Enable top-row filtering to allow users to filter by datasource, p-value range, etc,
    # but especially 'term size' for better control over too small and too large terms
    renderEnrichmentTable(shinyOutputId, transformedResultPartial,
                          caption, fileName, mode,
                          hiddenColumns, expandableColumn, filter = 'top')
  }
}

printLiteratureResultTable <- function(runKey = currentType_Tool) {
  # Print literature enrichment result table
  # @param runKey The run key for element IDs
  shinyOutputId <- paste(runKey, "table_pubmed", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "pubmed", runKey)
}

handleEnrichmentResultClear <- function(enrichmentType, toolName) {
  resetCombination()
  resetEnrichmentResults(enrichmentType, toolName)
  hideTab(inputId = "toolTabsPanel", target = toolName)
  prepareCombinationTab()
}

handleMultiClear <- function() {
  resetCombination()
  # Clear ALL active runs
  for (fullRunKey in names(activeRuns)) {
    if (startsWith(fullRunKey, "functional_")) {
      clearRunCompletely(fullRunKey)
    }
  }
  # Reset all run counters so numbering starts fresh
  initializeRunCounters()
  # Hide the results panel
  shinyjs::hide("functionalEnrichmentResultsPanel")
  shinyjs::hide("functional_enrichment_all_clear")
  prepareCombinationTab()
}

# Clear all literature enrichment runs
handleLiteratureMultiClear <- function() {
  # Clear ALL active literature runs
  for (fullRunKey in names(activeRuns)) {
    if (startsWith(fullRunKey, "literature_")) {
      clearLiteratureRunCompletely(fullRunKey)
    }
  }
  # Reset STRING run counter so numbering starts fresh
  resetRunCounterForTool("STRING")
  # Hide the results panel
  shinyjs::hide("literatureEnrichmentResultsPanel")
  shinyjs::hide("literature_enrichment_all_clear")
}

# Unified function to clear a single run completely (data, state, tab)
# Uses config to determine correct panel IDs
clearEnrichmentRun <- function(fullRunKey) {
  runInfo <- parseFullRunKey(fullRunKey)
  config <- getEnrichmentConfig(runInfo$enrichmentType)

  # Clear enrichment results
  enrichmentResults[[fullRunKey]] <<- NULL

  # Clear arena edgelists
  for (networkId in NETWORK_IDS) {
    arenaEdgelist[[paste(fullRunKey, networkId, sep = "_")]] <<- NULL
  }

  # Clear background sizes
  enrichmentBackgroundSizes[[toupper(fullRunKey)]] <<- NULL

  # Clear gProfiler results cache if applicable (only for functional)
  if (runInfo$toolName == "gProfiler") {
    gprofilerResults[[fullRunKey]] <<- NULL
  }

  # Clear plot state for this run
  clearPlotStateForRun(fullRunKey)

  # Remove the tab (using config for panel ID)
  removeTab(inputId = config$tabsetPanelId, target = runInfo$runId)

  # Unregister the run
  unregisterRun(fullRunKey)
}

# Legacy aliases for backward compatibility
clearRunCompletely <- clearEnrichmentRun
clearLiteratureRunCompletely <- clearEnrichmentRun

# Multi-Run Helper Functions

validEnrichmentResultMultiRun <- function() {
  valid <- FALSE
  enrichmentResult <- enrichmentResults[[currentFullRunKey]]
  if (!is.null(enrichmentResult) && nrow(enrichmentResult) > 0) {
    valid <- TRUE
  } else {
    renderWarning(paste0(
      "Functional enrichment with ", currentEnrichmentTool,
      " (Run ", currentRunNumber, ") could not return any valid results."))
  }
  return(valid)
}

printParametersMultiRun <- function() {
  parametersOutput <- paste0(
    "Run: ", currentEnrichmentTool, " (", currentRunNumber, ")",
    "\nFile: ", input[[paste0(currentEnrichmentType, "_enrichment_file")]],
    "\nOrganism: ", ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$print_name,
    "\nBackground: ", input[[
      paste0(currentEnrichmentType, "_enrichment_background_choice")]],
    "\nBackground size (no. of genes): ", enrichmentBackgroundSizes[[toupper(currentFullRunKey)]],
    "\nDatasources: ", decideToolSelectedDatasources(),
    "\nNamespace: ", currentNamespace,
    "\nSignificance metric: ", currentSignificanceMetric,
    "\nSignificance threshold: ", input[[
      paste0(currentEnrichmentType, "_enrichment_threshold")]]
  )
  renderShinyText(paste(currentFullRunKey, "enrichment_parameters", sep = "_"), parametersOutput)
}

# Helper functions for hiding/showing source tabs using Shiny's built-in functions
hideAllSourceTabsForRun <- function(fullRunKey) {
  sourcePanelId <- paste(fullRunKey, "sources_panel", sep = "_")

  # Determine which tabs to hide based on enrichment type
  runInfo <- parseFullRunKey(fullRunKey)
  if (runInfo$enrichmentType == "literature") {
    # Literature enrichment only has PUBMED tab
    hideTab(inputId = sourcePanelId, target = "PUBMED")
  } else {
    # Functional enrichment has multiple datasource tabs
    lapply(names(TAB_NAMES), function(tabTitle) {
      hideTab(inputId = sourcePanelId, target = tabTitle)
    })
  }
}

showSourceTabForRun <- function(fullRunKey, datasource) {
  sourcePanelId <- paste(fullRunKey, "sources_panel", sep = "_")
  # Map datasource to tab title
  tabTitle <- if (datasource == "all") "ALL" else if (datasource == "pubmed") "PUBMED" else datasource
  showTab(inputId = sourcePanelId, target = tabTitle)
}

# Clear all results for a run (but keep the tab and registration)
# Used when datasources change and we need to refresh results
clearRunResults <- function(fullRunKey) {

  # Clear enrichment results
  enrichmentResults[[fullRunKey]] <<- NULL

  # Clear arena edgelists
  for (networkId in NETWORK_IDS) {
    arenaEdgelist[[paste(fullRunKey, networkId, sep = "_")]] <<- NULL
  }

  # Clear plot state (internal tracking variables)
  clearPlotStateForRun(fullRunKey)

  # Clear rendered plot outputs (so users don't see stale data)
  resetPlots(fullRunKey)

  # Hide all source tabs (will be shown again as results populate)
  hideAllSourceTabsForRun(fullRunKey)
}
