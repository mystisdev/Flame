handleEnrichment <- function(enrichmentType) {
  tryCatch({
    if (existInputGeneLists()) {
      currentEnrichmentType <<- enrichmentType
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

            if (currentEnrichmentType == "functional") {
              # Multi-run: Check for matching parameters
              currentParams <- captureRunParameters()
              matchResult <- findMatchingRun("functional", toolName, currentParams)

              if (!is.null(matchResult$matchType) && matchResult$matchType == "exact") {
                # EXACT MATCH: Just select tab and pulse it (no API call)
                runInfo <- activeRuns[[matchResult$fullRunKey]]
                updateTabsetPanel(session, "toolTabsPanel", selected = runInfo$runId)
                # Pulse the tab to indicate where results are
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

                # Clear old results before re-running
                clearRunResults(existingRunKey)

                # Run enrichment (will populate existing tab)
                handleEnrichmentWithToolMultiRun(isUpdate = TRUE)
                return()
              }

              # NO MATCH: Create new run
              currentUniqueId <<- getNextUniqueId(toolName)
              currentRunNumber <<- getNextRunNumber(toolName)
              runId <- createRunId(toolName, currentUniqueId)
              currentFullRunKey <<- getFullRunKey("functional", runId)
              currentType_Tool <<- currentFullRunKey  # For backward compatibility

              # Register the run
              registerRun("functional", toolName, currentUniqueId, currentRunNumber, currentParams)

              handleEnrichmentWithToolMultiRun(isUpdate = FALSE)
            } else {
              # Literature: single-result mode (unchanged)
              currentType_Tool <<- paste(currentEnrichmentType, currentEnrichmentTool, sep = "_")
              currentFullRunKey <<- currentType_Tool
              handleEnrichmentWithToolLiterature()
            }
          })

          # Update combination tab visibility (only for functional, needs 2+ runs)
          if (currentEnrichmentType == "functional") {
            prepareCombinationTab()
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

# Multi-run handler for functional enrichment (creates dynamic tabs)
# isUpdate: TRUE when updating existing tab with new datasources, FALSE for new run
handleEnrichmentWithToolMultiRun <- function(isUpdate = FALSE) {
  actionText <- if (isUpdate) "Updating" else "Executing"
  renderModal(
    paste0(
      "<h2>Please wait.</h2><br />
      <p>", actionText, " functional enrichment with ", currentEnrichmentTool,
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
          insertResultTabForRun()
        }

        # Hide all source tabs initially (they'll be shown as results populate)
        hideAllSourceTabsForRun(currentFullRunKey)

        if (!isUpdate) {
          # Show the results panel if this is the first run
          if (getActiveFunctionalRunCount() == 1) {
            shinyjs::show("functionalEnrichmentResultsPanel")
            shinyjs::show("functional_enrichment_all_clear")
            # Ensure Combination tab stays hidden (it gets auto-shown when panel becomes visible)
            hideTab(inputId = "toolTabsPanel", target = "Combination")
          }

          # Register dynamic observers AFTER UI is flushed to the client
          # This ensures the dynamic UI elements exist before we attach observers
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
        # For updates, the UI already exists so we can call immediately
        if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          if (!isUpdate) {
            # Capture values for async callback
            local({
              runKey <- currentFullRunKey
              inputTable <- inputGenesConversionTable
              bgTable <- backgroundGenesConversionTable
              session$onFlushed(function() {
                shinyjs::show(paste(runKey, "conversionBoxes", sep = "_"))
                printUnconvertedGenesMultiRun(inputTable, bgTable)
                printConversionTableMultiRun(inputTable, bgTable)
              }, once = TRUE)
            })
          } else {
            # Update mode: UI already exists
            shinyjs::show(paste(currentFullRunKey, "conversionBoxes", sep = "_"))
            printUnconvertedGenesMultiRun(inputGenesConversionTable, backgroundGenesConversionTable)
            printConversionTableMultiRun(inputGenesConversionTable, backgroundGenesConversionTable)
          }
        }

        findAndPrintNoHitGenesMultiRun(noHitGenesCheckList)
        printResultTablesMultiRun()

        # Add run to pending set for deferred control update
        runId <- createRunId(currentEnrichmentTool, currentUniqueId)
        runsPendingControlUpdate <<- union(runsPendingControlUpdate, runId)
        updateTabsetPanel(session, "toolTabsPanel", selected = runId)

        # Pulse the tab to indicate results are ready
        session$sendCustomMessage("handler_pulseTab", runId)
      } else {
        # Enrichment failed
        if (!isUpdate) {
          # Only unregister if this was a new run
          unregisterRun(currentFullRunKey)
        }
      }
    } else {
      # Conversion failed
      if (!isUpdate) {
        # Only unregister if this was a new run
        unregisterRun(currentFullRunKey)
      }
    }
  } else {
    # No datasources
    if (!isUpdate) {
      # Only unregister if this was a new run
      unregisterRun(currentFullRunKey)
    }
  }
}

# Insert a new tab for a functional enrichment run
insertResultTabForRun <- function() {
  # Use uniqueId for internal Shiny IDs (prevents caching bugs)
  runId <- createRunId(currentEnrichmentTool, currentUniqueId)

  # Use displayNumber (currentRunNumber) for user-visible tab title
  tabTitle <- paste0(currentEnrichmentTool, " (", currentRunNumber, ")")

  # Generate tab content using uniqueId (for output IDs)
  tabContent <- generateToolPanelForRun("functional", currentEnrichmentTool, currentUniqueId)

  # Create tab title with close button
  tabTitleHtml <- tags$span(
    tabTitle,
    tags$button(
      class = "close-run-tab",
      type = "button",
      onclick = paste0("event.stopPropagation(); Shiny.setInputValue('closeRunTab', '", runId, "', {priority: 'event'});"),
      icon("times")
    )
  )

  # Insert the tab at the end (Combination is hidden and will be shown only when 2+ runs exist)
  insertTab(
    inputId = "toolTabsPanel",
    tab = tabPanel(
      title = tabTitleHtml,
      value = runId,
      tabContent
    ),
    select = TRUE
  )
}

# Literature enrichment handler (single-result mode, unchanged behavior)
handleEnrichmentWithToolLiterature <- function() {
  renderModal(
    paste0(
      "<h2>Please wait.</h2><br />
      <p>Executing literature enrichment with STRING.</p>"
    )
  )
  if (existDataSources()) {
    hideAllSourceTabsForRun(currentType_Tool)
    resetEnrichmentResults(currentEnrichmentType, currentEnrichmentTool)

    inputGenesConversionTable <- geneConvert(currentUserList)
    if (validInputGenesConversionTable(inputGenesConversionTable)) {
      if(length(currentBackgroundList)==0)
        backgroundGenesConversionTable <- NULL
      else
        backgroundGenesConversionTable <- geneConvert(currentBackgroundList)

      runEnrichmentAnalysis(inputGenesConversionTable$target, backgroundGenesConversionTable$target)

      if (validEnrichmentResult()) {
        showTab(inputId = "toolTabsPanel", target = currentEnrichmentTool)
        noHitGenesCheckList <- executeNamespaceRollback(inputGenesConversionTable)
        printParameters()
        if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          shinyjs::show(paste(currentType_Tool, "conversionBoxes", sep = "_"))
          printUnconvertedGenes(inputGenesConversionTable, backgroundGenesConversionTable)
          printConversionTable(inputGenesConversionTable, backgroundGenesConversionTable)
        }
        findAndPrintNoHitGenes(noHitGenesCheckList)
        printResultTables()
        updateTabsetPanel(session, "toolTabsPanel", selected = currentEnrichmentTool)
      }
    }
  }
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

printUnconvertedGenes <- function(convertedInputs, convertedOutputs = NULL) {
  # Display genes that failed conversion to target namespace
  # Note: This is different from "No-hit Inputs" which are genes that
  # successfully converted but don't appear in any enriched term

  shinyOutputId <- paste(currentType_Tool,
                         "notConverted_input", sep = "_")
  unconvertedInputs <- currentUserList[!currentUserList %in% convertedInputs$input]
  unconvertedInputsCount <- length(unconvertedInputs)
  if (unconvertedInputsCount > 0) {
    unconvertedInputs <- paste(unconvertedInputs, collapse=", ")
    prompt <- sprintf(
      "%d input item(s) could not be converted to the target namespace:\n%s",
      unconvertedInputsCount,
      unconvertedInputs
    )
    renderShinyText(shinyOutputId, prompt)
  } else
    renderShinyText(shinyOutputId, "-")
  
  if(!is.null(convertedOutputs)) {
    shinyOutputId_ref <- paste(currentType_Tool,
                           "notConverted_reference", sep = "_")
    unconvertedOutputs <- currentBackgroundList[!currentBackgroundList %in% convertedOutputs$input]
    unconvertedOutputsCount <- length(unconvertedOutputs)
    if (unconvertedInputsCount > 0) {
      unconvertedOutputs <- paste(unconvertedOutputs, collapse=", ")
      prompt_ref <- sprintf(
        "%d reference background item(s) could not be converted to the target namespace:\n%s",
        unconvertedOutputsCount,
        unconvertedOutputs
      )
      renderShinyText(shinyOutputId_ref, prompt_ref)
    } else
      renderShinyText(shinyOutputId_ref, "-")    
    shinyjs::show(paste(currentType_Tool, "notConverted_reference_div", sep = "_"))
  }
  else {
    shinyjs::hide(paste(currentType_Tool, "notConverted_reference_div", sep = "_"))
  }
  
}

printConversionTable <- function(inputConversionTable, backgroundConversionTable = NULL) {
  #first, for the input list
  shinyOutputId <- paste(currentType_Tool, "conversionTable_input", sep = "_")
  fileName <- paste(currentType_Tool, "conversion_table", sep = "_")
  colnames(inputConversionTable) <- c("Input", "Target", "Name")
  renderShinyDataTable(shinyOutputId, inputConversionTable,
                       fileName = fileName)
  #then, check to see if a custom reference background exists and if so, do the same for the reference
  if(is.null(backgroundConversionTable)) {
    shinyjs::show(paste(currentType_Tool, "conversionTable_genome_div", sep = "_"))
    shinyjs::hide(paste(currentType_Tool, "conversionTable_reference_div", sep = "_"))
  }
  else {
    shinyjs::hide(paste(currentType_Tool, "conversionTable_genome_div", sep = "_"))
    shinyOutputId <- paste(currentType_Tool, "conversionTable_reference", sep = "_")
    fileName <- paste(currentType_Tool, "conversion_table_reference", sep = "_")
    colnames(backgroundConversionTable) <- c("Input", "Target", "Name")
    renderShinyDataTable(shinyOutputId, backgroundConversionTable,
                         fileName = fileName)
    shinyjs::show(paste(currentType_Tool, "conversionTable_reference_div", sep = "_"))
  }
}

findAndPrintNoHitGenes <- function(convertedInputs) {
  noHitGenes <- findNoHitGenes(convertedInputs)
  printNoHitGenes(noHitGenes)
}

findNoHitGenes <- function(convertedInputs) {
  # Find genes that successfully converted but don't appear in any enriched term
  #
  # convertedInputs: List of genes that successfully converted (either original symbols
  #                  or tool-specific IDs, depending on rollback setting)
  #
  # Logic:
  # 1. Collect all genes appearing in Positive Hits across ALL enriched terms
  # 2. Compare convertedInputs against these "hit" genes
  # 3. Return genes that converted successfully but aren't in any term
  #
  # Note: This is different from "Unconverted Inputs" which are genes that
  # failed at the conversion step entirely

  enrichmentResult <- getGlobalEnrichmentResult(currentEnrichmentType, currentEnrichmentTool)

  # Collect all genes from Positive Hits column across all enriched terms
  allHitGenes <- paste(enrichmentResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)

  # Find genes that converted but don't appear in any enriched term
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenes <- function(noHitGenes) {
  shinyOutputId <- paste(currentType_Tool, "genesNotFound", sep = "_")
  noHitGenesCount <- length(noHitGenes)
  if (noHitGenesCount > 0) {
    noHitGenes <- paste(noHitGenes, collapse=", ")
    prompt <- sprintf(
      "%d input item(s) not found in any result term:\n%s",
      noHitGenesCount,
      noHitGenes
    )
    renderShinyText(shinyOutputId, prompt)
  } else
    renderShinyText(shinyOutputId, "-")
}

printResultTables <- function() {
  formatResultTable()
  switch(
    currentEnrichmentType,
    "functional" = printFunctionalResultTable(),
    "literature" = printLiteratureResultTable()
  )
}

formatResultTable <- function() {
  enrichmentResults[[currentType_Tool]] <<-
    enrichmentResults[[currentType_Tool]][order(
      -enrichmentResults[[currentType_Tool]]$`-log10Pvalue`), ]
  
  if (is.null(enrichmentResults[[currentType_Tool]]$Term_ID_noLinks)) { # if no links returned from tool
    enrichmentResults[[currentType_Tool]]$Term_ID_noLinks <<-
      enrichmentResults[[currentType_Tool]]$Term_ID
    
    switch(
      currentEnrichmentType,
      "functional" = attachDBLinks(),
      "literature" = attachLinks("PUBMED", "https://pubmed.ncbi.nlm.nih.gov/", gSub = "PMID:")
    )
  }
}

printFunctionalResultTable <- function() {
  shinyOutputId <- paste(currentType_Tool, "table_all", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "all")
  datasources <- input$functional_enrichment_datasources
  lapply(datasources, function(datasource) {
    partialId <- as.character(TAB_NAMES[datasource])
    shinyOutputId <- paste(currentType_Tool, "table", partialId, sep = "_")
    tabPosition <- match(datasource, ENRICHMENT_DATASOURCES)
    printResultTable(shinyOutputId, tabPosition, datasource)
  })
}

printResultTable <- function(shinyOutputId, tabPosition, datasource) {
  if (datasource == "all" || datasource == "pubmed") {
    transformedResultPartial <- enrichmentResults[[currentType_Tool]]
  } else {
    pattern <- paste0("^", datasource, "$")
    matches <- grepl(pattern, enrichmentResults[[currentType_Tool]]$Source)
    transformedResultPartial <-
      enrichmentResults[[currentType_Tool]][matches, ]
  }

  if (nrow(transformedResultPartial) > 0) {
    transformedResultPartial$`Positive Hits` <-
      gsub(",", ", ", transformedResultPartial$`Positive Hits`)

    showSourceTabForRun(currentType_Tool, datasource)

    caption = "Enrichment Results"
    fileName <- paste(currentType_Tool, datasource, sep = "_")
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

printLiteratureResultTable <- function() {
  shinyOutputId <- paste(currentType_Tool, "table_pubmed", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "pubmed")
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

# Clear a single run completely (data, state, tab)
clearRunCompletely <- function(fullRunKey) {
  runInfo <- parseFullRunKey(fullRunKey)

  # Clear enrichment results
  enrichmentResults[[fullRunKey]] <<- NULL

  # Clear arena edgelists
  for (networkId in NETWORK_IDS) {
    arenaEdgelist[[paste(fullRunKey, networkId, sep = "_")]] <<- NULL
  }

  # Clear background sizes
  enrichmentBackgroundSizes[[toupper(fullRunKey)]] <<- NULL

  # Clear gProfiler results cache if applicable
  if (runInfo$toolName == "gProfiler") {
    gprofilerResults[[fullRunKey]] <<- NULL
  }

  # Clear plot state for this run
  clearPlotStateForRun(fullRunKey)

  # Remove the tab
  removeTab(inputId = "toolTabsPanel", target = runInfo$runId)

  # Unregister the run
  unregisterRun(fullRunKey)
}

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

printUnconvertedGenesMultiRun <- function(convertedInputs, convertedOutputs = NULL) {
  shinyOutputId <- paste(currentFullRunKey, "notConverted_input", sep = "_")
  unconvertedInputs <- currentUserList[!currentUserList %in% convertedInputs$input]
  unconvertedInputsCount <- length(unconvertedInputs)
  if (unconvertedInputsCount > 0) {
    unconvertedInputs <- paste(unconvertedInputs, collapse=", ")
    prompt <- sprintf(
      "%d input item(s) could not be converted to the target namespace:\n%s",
      unconvertedInputsCount,
      unconvertedInputs
    )
    renderShinyText(shinyOutputId, prompt)
  } else {
    renderShinyText(shinyOutputId, "-")
  }

  if(!is.null(convertedOutputs)) {
    shinyOutputId_ref <- paste(currentFullRunKey, "notConverted_reference", sep = "_")
    unconvertedOutputs <- currentBackgroundList[!currentBackgroundList %in% convertedOutputs$input]
    unconvertedOutputsCount <- length(unconvertedOutputs)
    if (unconvertedOutputsCount > 0) {
      unconvertedOutputs <- paste(unconvertedOutputs, collapse=", ")
      prompt_ref <- sprintf(
        "%d reference background item(s) could not be converted to the target namespace:\n%s",
        unconvertedOutputsCount,
        unconvertedOutputs
      )
      renderShinyText(shinyOutputId_ref, prompt_ref)
    } else {
      renderShinyText(shinyOutputId_ref, "-")
    }
    shinyjs::show(paste(currentFullRunKey, "notConverted_reference_div", sep = "_"))
  } else {
    shinyjs::hide(paste(currentFullRunKey, "notConverted_reference_div", sep = "_"))
  }
}

printConversionTableMultiRun <- function(inputConversionTable, backgroundConversionTable = NULL) {
  shinyOutputId <- paste(currentFullRunKey, "conversionTable_input", sep = "_")
  fileName <- paste(currentFullRunKey, "conversion_table", sep = "_")
  colnames(inputConversionTable) <- c("Input", "Target", "Name")
  renderShinyDataTable(shinyOutputId, inputConversionTable, fileName = fileName)

  if(is.null(backgroundConversionTable)) {
    shinyjs::show(paste(currentFullRunKey, "conversionTable_genome_div", sep = "_"))
    shinyjs::hide(paste(currentFullRunKey, "conversionTable_reference_div", sep = "_"))
  } else {
    shinyjs::hide(paste(currentFullRunKey, "conversionTable_genome_div", sep = "_"))
    shinyOutputId <- paste(currentFullRunKey, "conversionTable_reference", sep = "_")
    fileName <- paste(currentFullRunKey, "conversion_table_reference", sep = "_")
    colnames(backgroundConversionTable) <- c("Input", "Target", "Name")
    renderShinyDataTable(shinyOutputId, backgroundConversionTable, fileName = fileName)
    shinyjs::show(paste(currentFullRunKey, "conversionTable_reference_div", sep = "_"))
  }
}

findAndPrintNoHitGenesMultiRun <- function(convertedInputs) {
  noHitGenes <- findNoHitGenesMultiRun(convertedInputs)
  printNoHitGenesMultiRun(noHitGenes)
}

findNoHitGenesMultiRun <- function(convertedInputs) {
  enrichmentResult <- enrichmentResults[[currentFullRunKey]]
  allHitGenes <- paste(enrichmentResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenesMultiRun <- function(noHitGenes) {
  shinyOutputId <- paste(currentFullRunKey, "genesNotFound", sep = "_")
  noHitGenesCount <- length(noHitGenes)
  if (noHitGenesCount > 0) {
    noHitGenes <- paste(noHitGenes, collapse=", ")
    prompt <- sprintf(
      "%d input item(s) not found in any result term:\n%s",
      noHitGenesCount,
      noHitGenes
    )
    renderShinyText(shinyOutputId, prompt)
  } else {
    renderShinyText(shinyOutputId, "-")
  }
}

printResultTablesMultiRun <- function() {
  formatResultTableMultiRun()
  printFunctionalResultTableMultiRun()
}

formatResultTableMultiRun <- function() {
  enrichmentResults[[currentFullRunKey]] <<-
    enrichmentResults[[currentFullRunKey]][order(
      -enrichmentResults[[currentFullRunKey]]$`-log10Pvalue`), ]

  if (is.null(enrichmentResults[[currentFullRunKey]]$Term_ID_noLinks)) {
    enrichmentResults[[currentFullRunKey]]$Term_ID_noLinks <<-
      enrichmentResults[[currentFullRunKey]]$Term_ID
    attachDBLinksMultiRun()
  }
}

attachDBLinksMultiRun <- function() {
  # Reuse the standard link attachment logic with the multi-run result key
  attachDBLinks(currentFullRunKey)
}

printFunctionalResultTableMultiRun <- function() {
  shinyOutputId <- paste(currentFullRunKey, "table_all", sep = "_")
  tabPosition <- 0
  printResultTableMultiRun(shinyOutputId, tabPosition, "all")

  datasources <- input$functional_enrichment_datasources
  lapply(datasources, function(datasource) {
    partialId <- as.character(TAB_NAMES[datasource])
    shinyOutputId <- paste(currentFullRunKey, "table", partialId, sep = "_")
    tabPosition <- match(datasource, ENRICHMENT_DATASOURCES)
    printResultTableMultiRun(shinyOutputId, tabPosition, datasource)
  })
}

printResultTableMultiRun <- function(shinyOutputId, tabPosition, datasource) {
  if (datasource == "all") {
    transformedResultPartial <- enrichmentResults[[currentFullRunKey]]
  } else {
    pattern <- paste0("^", datasource, "$")
    matches <- grepl(pattern, enrichmentResults[[currentFullRunKey]]$Source)
    transformedResultPartial <- enrichmentResults[[currentFullRunKey]][matches, ]
  }

  if (nrow(transformedResultPartial) > 0) {
    transformedResultPartial$`Positive Hits` <-
      gsub(",", ", ", transformedResultPartial$`Positive Hits`)

    showSourceTabForRun(currentFullRunKey, datasource)

    caption = "Enrichment Results"
    fileName <- paste(currentFullRunKey, datasource, sep = "_")
    mode <- "Positive Hits"
    # With rownames=FALSE: 0=⊕, ..., 10=Positive Hits, 11=Term_ID_noLinks
    hiddenColumns <- c(10, 11)
    expandableColumn <- 10

    transformedResultPartial$Source <- as.factor(transformedResultPartial$Source)

    renderEnrichmentTable(shinyOutputId, transformedResultPartial,
                          caption, fileName, mode,
                          hiddenColumns, expandableColumn, filter = 'top')
  }
}

# Helper functions for hiding/showing source tabs using Shiny's built-in functions
hideAllSourceTabsForRun <- function(fullRunKey) {
  sourcePanelId <- paste(fullRunKey, "sources_panel", sep = "_")
  # Hide all datasource tabs (all tab names from TAB_NAMES)
  lapply(names(TAB_NAMES), function(tabTitle) {
    hideTab(inputId = sourcePanelId, target = tabTitle)
  })
}

showSourceTabForRun <- function(fullRunKey, datasource) {
  sourcePanelId <- paste(fullRunKey, "sources_panel", sep = "_")
  # Map datasource to tab title (for "all" -> "ALL", others are the same)
  tabTitle <- if (datasource == "all") "ALL" else datasource
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
