handleEnrichment <- function(enrichmentType) {
  tryCatch({
    if (existInputGeneLists()) {
      # Get config for this enrichment type (no global assignment)
      config <- getEnrichmentConfig(enrichmentType)

      if (existInputOrganism(enrichmentType)) {
        if (existEnrichmentTool(enrichmentType)) {
          tools <- switch(
            enrichmentType,
            "functional" = input$functional_enrichment_tool,
            "literature" = "STRING"
          )

          # Extract values into LOCAL variables (no globals)
          userList <- unlist(userInputLists[names(userInputLists) ==
                                    input[[paste0(enrichmentType,
                                                  "_enrichment_file")]]][[1]])
          organism <- ORGANISMS[ORGANISMS$print_name ==
                                  input[[paste0(enrichmentType,
                                                "_enrichment_organism")]], ]$taxid

          backgroundList <- if(input[[paste0(enrichmentType, "_enrichment_background_choice")]] == "genome") {
            NULL
          } else {
            unlist(userInputLists[names(userInputLists) ==
                                    input[[paste0(enrichmentType,
                                                  "_enrichment_background_list")]]][[1]])
          }

          lapply(tools, function(toolName) {
            # All values are local - no globals needed
            significanceMetric <- decideToolMetric(enrichmentType, toolName)

            # Multi-run: Check for matching parameters (unified for all types)
            # Pass explicit parameters instead of relying on globals inside captureRunParameters
            currentParams <- captureRunParameters(enrichmentType, toolName, organism)
            matchResult <- findMatchingRun(enrichmentType, toolName, currentParams)

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
              existingRun <- activeRuns[[existingRunKey]]

              # Update stored parameters with new datasources
              existingRun$parameters <- currentParams
              existingRun$timestamp <- Sys.time()

              # Update gene list and background in case they changed
              existingRun$geneList <- userList
              existingRun$backgroundList <- backgroundList
              existingRun$significanceMetric <- significanceMetric

              clearRunResults(existingRunKey)

              # Pass Run object directly instead of reading globals
              handleEnrichmentRun(existingRun, isUpdate = TRUE)
              return()
            }

            # NO MATCH: Create new run using ORAEnrichmentRun
            run <- ORAEnrichmentRun$new(
              enrichmentType = enrichmentType,
              toolName = toolName,
              organism = organism,
              geneList = userList,
              backgroundList = backgroundList,
              significanceMetric = significanceMetric
            )
            run$parameters <- currentParams

            # Store the Run object in activeRuns (replaces registerRun)
            run$save()

            # Pass Run object directly instead of reading globals
            handleEnrichmentRun(run, isUpdate = FALSE)
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

existInputOrganism <- function(enrichmentType) {
  notEmpty <- F
  if (input[[paste0(enrichmentType, "_enrichment_organism")]] != "")
    notEmpty <- T
  else
    renderWarning("Select an organism from the list.")
  return(notEmpty)
}

existEnrichmentTool <- function(enrichmentType) {
  exist <- F
  if (!is.null(input[[paste0(enrichmentType, "_enrichment_tool")]]))
    exist <- T
  else
    renderWarning("Select at least one enrichment tool.")
  return(exist)
}

# Returns the significance metric instead of setting global
# Parameters:
#   enrichmentType: "functional" or "literature"
#   toolName: e.g., "gProfiler", "STRING", etc.
decideToolMetric <- function(enrichmentType, toolName) {
  if (input[[paste0(enrichmentType, "_enrichment_metric")]] == DEFAULT_METRIC_TEXT) {
    if(input[[paste0(enrichmentType, "_enrichment_background_choice")]] == "genome")
      return(DEFAULT_METRICS_GENOME[[toupper(toolName)]])
    else
      return(DEFAULT_METRICS_USERBACKGROUND[[toupper(toolName)]])
  }
  else
    return(input[[paste0(enrichmentType, "_enrichment_metric")]])
}

# Unified multi-run handler for all enrichment types (config-driven)
# Uses ENRICHMENT_TYPES_CONFIG to handle differences between functional/literature/future types
# Pass Run object directly instead of reading globals
# isUpdate: TRUE when updating existing tab with new datasources, FALSE for new run
handleEnrichmentRun <- function(run, isUpdate = FALSE) {
  # Get config from Run object (no longer reading from globals)
  config <- getEnrichmentConfig(run$enrichmentType)

  actionText <- if (isUpdate) "Updating" else "Executing"
  renderModal(
    paste0(
      "<h2>Please wait.</h2><br />
      <p>", actionText, " ", config$label, " with ", run$toolName,
      " (Run ", run$runNumber, ").</p>"
    )
  )

  if (existDataSources(run$enrichmentType)) {
    # Initialize empty result storage for this run
    enrichmentResults[[run$id]] <<- data.frame()
    for (networkId in NETWORK_IDS) {
      arenaEdgelist[[paste(run$id, networkId, sep = "_")]] <<- data.frame()
    }

    # Convert input gene symbols to tool-specific format
    inputGenesConversionTable <- geneConvert(run$geneList, run$enrichmentType, run$organism, run$toolName)

    # Store namespace in run object from geneConvert result
    # This allows downstream functions to access it from run$parameters$namespace
    run$parameters$namespace <- attr(inputGenesConversionTable, "namespace")

    if (validInputGenesConversionTable(inputGenesConversionTable)) {
      backgroundList <- run$backgroundList
      if(is.null(backgroundList) || length(backgroundList) == 0)
        backgroundGenesConversionTable <- NULL
      else
        backgroundGenesConversionTable <- geneConvert(backgroundList, run$enrichmentType, run$organism, run$toolName)

      # Run enrichment analysis via Run object
      if (!inherits(run, "ORAEnrichmentRun")) {
        stop(paste("Invalid run object for", run$toolName, "- expected ORAEnrichmentRun"))
      }

      # Get converted background list (may be NULL)
      convertedBackground <- if (!is.null(backgroundGenesConversionTable)) {
        backgroundGenesConversionTable$target
      } else {
        NULL
      }

      # Execute via Run object - stores in both Run and globals
      success <- run$executeWithConvertedLists(
        inputGenesConversionTable$target,
        convertedBackground
      )

      # Note: success=FALSE means either no strategy or empty results
      # Both cases are handled gracefully by validEnrichmentResultMultiRun below
      # which will unregister the run and show appropriate feedback

      if (validEnrichmentResultMultiRun(run)) {
        if (!isUpdate) {
          # Create dynamic tab for this run (only for new runs)
          insertEnrichmentTab(config, run)

          # Register outputs for cleanup
          registerOutputsForRun(run$id)
        }

        # Hide all source tabs initially (they'll be shown as results populate)
        hideAllSourceTabsForRun(run$id)

        if (!isUpdate) {
          # Show the results panel if this is the first run
          if (getActiveRunCount(run$enrichmentType) == 1) {
            shinyjs::show(config$resultsPanelId)
            shinyjs::show(config$clearButtonId)
            # For functional enrichment, ensure Combination tab stays hidden initially
            if (config$supportsCombination) {
              hideTab(inputId = config$tabsetPanelId, target = "Combination")
            }
          }

          # Register dynamic observers AFTER UI is flushed to the client
          local({
            runKeyForObservers <- run$id
            session$onFlushed(function() {
              registerObserversForRun(runKeyForObservers)
            }, once = TRUE)
          })
        }

        noHitGenesCheckList <- executeNamespaceRollback(inputGenesConversionTable, run)
        printParametersMultiRun(run)

        # For new runs, defer shinyjs::show until UI is flushed (element must exist in DOM)
        if (input[[paste0(run$enrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          if (!isUpdate) {
            local({
              runKey <- run$id
              inputTable <- inputGenesConversionTable
              bgTable <- backgroundGenesConversionTable
              origInputs <- run$geneList
              origBackground <- run$backgroundList
              session$onFlushed(function() {
                shinyjs::show(paste(runKey, "conversionBoxes", sep = "_"))
                printUnconvertedGenes(inputTable, bgTable, runKey = runKey,
                                       originalInputs = origInputs, originalBackground = origBackground)
                printConversionTable(inputTable, bgTable, runKey = runKey)
              }, once = TRUE)
            })
          } else {
            # Update mode: UI already exists
            shinyjs::show(paste(run$id, "conversionBoxes", sep = "_"))
            printUnconvertedGenes(inputGenesConversionTable, backgroundGenesConversionTable,
                                   runKey = run$id, originalInputs = run$geneList,
                                   originalBackground = run$backgroundList)
            printConversionTable(inputGenesConversionTable, backgroundGenesConversionTable, runKey = run$id)
          }
        }

        findAndPrintNoHitGenes(noHitGenesCheckList, runKey = run$id)
        printResultTables(runKey = run$id)

        # Update plot control panels after UI is flushed to ensure DOM elements exist
        # This works for both new runs and datasource-differ updates
        local({
          runKeyForUpdate <- run$id
          session$onFlushed(function() {
            updatePlotControlPanelsForRun(runKeyForUpdate)
          }, once = TRUE)
        })

        updateTabsetPanel(session, config$tabsetPanelId, selected = run$runId)

        # Pulse the tab to indicate results are ready
        session$sendCustomMessage("handler_pulseTab", run$runId)
      } else {
        # Enrichment failed
        if (!isUpdate) {
          unregisterRun(run$id)
        }
      }
    } else {
      # Conversion failed
      if (!isUpdate) {
        unregisterRun(run$id)
      }
    }
  } else {
    # No datasources
    if (!isUpdate) {
      unregisterRun(run$id)
    }
  }
}

# Unified tab insertion function for all enrichment types (config-driven)
# Pass Run object directly instead of reading globals
insertEnrichmentTab <- function(config = NULL, run) {
  # run is required - fail fast if not provided
  if (is.null(run)) {
    stop("insertEnrichmentTab: run object is required")
  }

  if (is.null(config)) {
    config <- getEnrichmentConfig(run$enrichmentType)
  }

  # Use Run object properties instead of globals
  runId <- run$runId
  tabTitle <- paste0(run$toolName, " (", run$runNumber, ")")

  # Generate tab content using the appropriate generator from config
  tabContent <- if (config$generateTabContent == "generateToolPanelForRun") {
    generateToolPanelForRun("functional", run$toolName, run$uniqueId)
  } else {
    generateToolPanelForLiteratureRun(run$toolName, run$uniqueId)
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

# Legacy aliases for backward compatibility (use unified functions above)
handleEnrichmentWithToolLiteratureMultiRun <- handleEnrichmentRun
getActiveLiteratureRunCount <- function() {
  getActiveRunCount("literature")
}

# Pass enrichmentType parameter instead of reading global
existDataSources <- function(enrichmentType) {
  exist <- F
  if (!is.null(input[[paste0(enrichmentType, "_enrichment_datasources")]]))
    exist <- T
  else
    renderWarning("Select at least one datasource.")
  return(exist)
}

# Pass all parameters instead of reading globals
# Returns namespace as attribute on result, stores in run$parameters$namespace
geneConvert <- function(geneList, enrichmentType, organism, toolName) {
  # Convert gene symbols to tool-specific identifier format required by enrichment APIs
  # Returns data.frame with columns: input (original), target (converted), name (description)
  # Also attaches 'namespace' attribute to result for caller to store in run object

  namespace <- input[[paste0(enrichmentType, "_enrichment_namespace")]]
  if (namespace == DEFAULT_NAMESPACE_TEXT)
    namespace <- getDefaultTargetNamespace(toolName, organism)

  # Namespace is passed through run$parameters$namespace - no global needed

  if (namespace != "USERINPUT") {
    if (toolName == "STRING") {
      # For STRING: Convert to STRING format via STRING's get_string_ids API
      # Input: ["RPL23", "TPR"] -> Output: ["9606.ENSP00000420311", "9606.ENSP00000360532"]
      inputGenesConversionTable <- stringPOSTConvertENSP(geneList, organism)
    } else if (toolName == "PANTHER") {
      # For PANTHER: Use PANTHER's geneinfo API for gene mapping
      # Input: ["FSD1L", "LTA4H"] -> Output: ["HUMAN|HGNC=13753|UniProtKB=Q9BXM9", "HUMAN|HGNC=6710|UniProtKB=P09960"]
      inputGenesConversionTable <- pantherPOSTConvert(geneList, organism)
    } else if (toolName == "GeneCodis") {
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
      inputGenesConversionTable <- gProfilerConvert(geneList, namespace, organism)
    }
  } else {
    # USERINPUT: No conversion needed - pass genes through as-is
    inputGenesConversionTable <- data.frame(
      "input" = geneList,
      "target" = geneList,
      "name" = geneList
    )
  }

  # Attach namespace as attribute so caller can store it in run$parameters
  attr(inputGenesConversionTable, "namespace") <- namespace
  return(inputGenesConversionTable)
}

# Pass parameters instead of reading globals
getDefaultTargetNamespace <- function(toolName, organism) {
  shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
  switch(
    toolName,
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

gProfilerConvert <- function(geneList, target, organism) {
  # Pass organism as parameter instead of reading currentOrganism global
  organismShortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
  inputGenesConversionTable <- gprofiler2::gconvert(
    geneList,
    organism = organismShortName,
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

getGlobalEnrichmentResult <- function(enrichmentType, toolName) {
  return(enrichmentResults[[paste(enrichmentType, toolName, sep = "_")]])
}

# Pass Run object directly instead of reading globals
executeNamespaceRollback <- function(inputGenesConversionTable, run) {
  # Determine whether to roll back converted IDs to original input names
  rollBackNamesFlag <- ifelse(input[[paste0(run$enrichmentType,
                                            "_enrichment_inputConversion")]] ==
                                "Original input names", T, F)

  # Use run$id as the result key (unified for all enrichment types)
  resultKey <- run$id

  if (rollBackNamesFlag) {
    # Convert Positive Hits back from tool-specific IDs to original gene symbols
    enrichmentResults[[resultKey]] <<-
      rollBackConvertedNames(enrichmentResults[[resultKey]],
                             inputGenesConversionTable, run$toolName)

    # For no-hit calculation: use original gene symbols that successfully converted
    noHitGenesCheckList <- inputGenesConversionTable$input
  } else {
    # For no-hit calculation: use tool-specific IDs (not rolled back)
    noHitGenesCheckList <- inputGenesConversionTable$target
  }

  return(noHitGenesCheckList)
}

rollBackConvertedNames <- function(enrichmentOutput, inputGenesConversionTable, toolName) {
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
  if (toolName == "WebGestalt") # Term_ID_noLinks already generated
    enrichmentOutput <-
    enrichmentOutput[, c("Source", "Term_ID", "Function",  "P-value",
                         "-log10Pvalue", "Term Size", "Query size",
                         "Intersection Size", "Enrichment Score %",
                         "Positive Hits", "Term_ID_noLinks")]
  return(as.data.frame(distinct(enrichmentOutput)))
}


# Pass parameters instead of reading globals
decideToolSelectedDatasources <- function(enrichmentType, toolName) {
  inputSelectedDatasources <-
    input[[paste0(enrichmentType, "_enrichment_datasources")]]
  # toolName is required - no global fallback
  if (is.null(toolName)) {
    warning("decideToolSelectedDatasources: toolName is required")
    return("")
  }
  tool <- toolName
  prefix <- ""
  if (tool == "enrichR")
    prefix <- getEnrichrVariablePrefix()
  toolDatasources <- DATASOURCES[[paste0(prefix, toupper(tool))]]
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
                                   runKey, originalInputs, originalBackground = NULL) {
  # Display genes that failed conversion to target namespace
  # Note: This is different from "No-hit Inputs" which are genes that
  # successfully converted but don't appear in any enriched term
  #
  # @param runKey The run key for element IDs (required)
  # @param originalInputs The original input gene list (from run$geneList)
  # @param originalBackground The original background gene list (from run$backgroundList)

  # Report unconverted input genes
  inputOutputId <- paste(runKey, "notConverted_input", sep = "_")
  unconvertedInputs <- originalInputs[!originalInputs %in% convertedInputs$input]
  renderGeneReport(
    outputId = inputOutputId,
    genes = unconvertedInputs,
    messageTemplate = "%d input item(s) could not be converted to the target namespace:\n%s"
  )

  # Report unconverted background genes (if provided)
  refOutputId <- paste(runKey, "notConverted_reference", sep = "_")
  refDivId <- paste(runKey, "notConverted_reference_div", sep = "_")

  if (!is.null(convertedOutputs) && !is.null(originalBackground)) {
    unconvertedBackground <- originalBackground[!originalBackground %in% convertedOutputs$input]
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
                                  runKey) {
  # Display gene conversion table for input list and optional background list
  # @param runKey The run key for element IDs (required)

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

findNoHitGenes <- function(convertedInputs, runKey) {
  # Find genes that successfully converted but don't appear in any enriched term
  #
  # convertedInputs: List of genes that successfully converted (either original symbols
  #                  or tool-specific IDs, depending on rollback setting)
  # runKey: The run key for looking up enrichment results (required)
  #
  # Logic:
  # 1. Collect all genes appearing in Positive Hits across ALL enriched terms
  # 2. Compare convertedInputs against these "hit" genes
  # 3. Return genes that converted successfully but aren't in any term
  #
  # Note: This is different from "Unconverted Inputs" which are genes that
  # failed at the conversion step entirely

  enrichmentResult <- enrichmentResults[[runKey]]

  # Collect all genes from Positive Hits column across all enriched terms
  allHitGenes <- paste(enrichmentResult$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)

  # Find genes that converted but don't appear in any enriched term
  noHitGenes <- convertedInputs[!convertedInputs %in% allHitGenes]
  return(noHitGenes)
}

printNoHitGenes <- function(noHitGenes, runKey) {
  # Display genes not found in any enriched term
  # @param runKey The run key for element IDs (required)
  shinyOutputId <- paste(runKey, "genesNotFound", sep = "_")
  renderGeneReport(
    outputId = shinyOutputId,
    genes = noHitGenes,
    messageTemplate = "%d input item(s) not found in any result term:\n%s"
  )
}

printResultTables <- function(runKey) {
  # Print all result tables for an enrichment run
  # @param runKey The run key for element IDs and result lookup (required)
  formatResultTable(runKey)
  # Derive enrichment type from runKey instead of reading global
  runInfo <- parseFullRunKey(runKey)
  switch(
    runInfo$enrichmentType,
    "functional" = printFunctionalResultTable(runKey),
    "literature" = printLiteratureResultTable(runKey)
  )
}

# Pass Run object directly instead of reading globals
formatResultTable <- function(runKey) {
  # Format and sort enrichment results, add database links
  # @param runKey The run key for result lookup (required)
  enrichmentResults[[runKey]] <<-
    enrichmentResults[[runKey]][order(
      -enrichmentResults[[runKey]]$`-log10Pvalue`), ]

  if (is.null(enrichmentResults[[runKey]]$Term_ID_noLinks)) {
    enrichmentResults[[runKey]]$Term_ID_noLinks <<-
      enrichmentResults[[runKey]]$Term_ID

    # Derive enrichment type from runKey instead of using global
    runInfo <- parseFullRunKey(runKey)
    enrichmentType <- runInfo$enrichmentType

    # Get Run object for context (organism, toolName, namespace)
    run <- activeRuns[[runKey]]
    if (!is.null(run)) {
      organism <- run$organism
      toolName <- run$toolName
      namespace <- run$parameters$namespace
    } else {
      # Run object not found - this shouldn't happen during normal operation
      # Log warning and skip link attachment to avoid using stale globals
      warning(paste("formatResultTable: Run object not found for", runKey, "- skipping DB links"))
      return()
    }

    switch(
      enrichmentType,
      "functional" = attachDBLinks(runKey, organism, toolName, namespace),
      "literature" = attachLinks("PUBMED", "https://pubmed.ncbi.nlm.nih.gov/", gSub = "PMID:", resultKey = runKey)
    )
  }
}

printFunctionalResultTable <- function(runKey) {
  # Print functional enrichment result tables for all datasources
  # @param runKey The run key for element IDs and result lookup (required)
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
                              runKey) {
  # Print a single result table for a datasource
  # @param runKey The run key for result lookup (required)
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

printLiteratureResultTable <- function(runKey) {
  # Print literature enrichment result table
  # @param runKey The run key for element IDs (required)
  shinyOutputId <- paste(runKey, "table_pubmed", sep = "_")
  tabPosition <- 0
  printResultTable(shinyOutputId, tabPosition, "pubmed", runKey)
}

handleMultiClear <- function() {
  resetCombination()
  # Clear ALL active functional runs
  for (fullRunKey in names(activeRuns)) {
    if (startsWith(fullRunKey, "functional_")) {
      clearRunCompletely(fullRunKey)
    }
  }
  # Reset counters ONLY for tools with no remaining runs (across both types)
  # This prevents resetting STRING if literature still has STRING runs
  for (tool in ENRICHMENT_TOOLS) {
    if (countActiveRunsForTool(tool) == 0) {
      resetRunCounterForTool(tool)
    }
  }
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
  # Reset STRING counter ONLY if no remaining STRING runs (across both types)
  # This prevents resetting if functional still has STRING runs
  if (countActiveRunsForTool("STRING") == 0) {
    resetRunCounterForTool("STRING")
  }
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

  # Destroy dynamic observers via registry. Observers FIRST, then Outputs
  # (Observers may reference outputs; destroy watchers before data)
  observerRegistry$clearRun(fullRunKey)

  # Clear Shiny outputs via registry
  outputRegistry$clearRun(fullRunKey, output)

  # Remove the tab (using config for panel ID)
  removeTab(inputId = config$tabsetPanelId, target = runInfo$runId)

  # Unregister the run
  unregisterRun(fullRunKey)
}

# Legacy aliases for backward compatibility
clearRunCompletely <- clearEnrichmentRun
clearLiteratureRunCompletely <- clearEnrichmentRun

# Multi-Run Helper Functions

# Pass Run object directly instead of reading globals
validEnrichmentResultMultiRun <- function(run) {
  valid <- FALSE
  enrichmentResult <- enrichmentResults[[run$id]]
  if (!is.null(enrichmentResult) && nrow(enrichmentResult) > 0) {
    valid <- TRUE
  } else {
    renderWarning(paste0(
      "Functional enrichment with ", run$toolName,
      " (Run ", run$runNumber, ") could not return any valid results."))
  }
  return(valid)
}

# Pass Run object directly instead of reading globals
printParametersMultiRun <- function(run) {
  # Handle NULL backgroundSize gracefully (occurs with genome background for some tools)
  bgSize <- enrichmentBackgroundSizes[[toupper(run$id)]]
  bgSizeDisplay <- if (is.null(bgSize)) "Genome-wide (tool default)" else bgSize

  parametersOutput <- paste0(
    "Run: ", run$toolName, " (", run$runNumber, ")",
    "\nFile: ", input[[paste0(run$enrichmentType, "_enrichment_file")]],
    "\nOrganism: ", ORGANISMS[ORGANISMS$taxid == run$organism, ]$print_name,
    "\nBackground: ", input[[
      paste0(run$enrichmentType, "_enrichment_background_choice")]],
    "\nBackground size (no. of genes): ", bgSizeDisplay,
    "\nDatasources: ", decideToolSelectedDatasources(run$enrichmentType, run$toolName),
    "\nNamespace: ", run$parameters$namespace,
    "\nSignificance metric: ", run$significanceMetric,
    "\nSignificance threshold: ", input[[
      paste0(run$enrichmentType, "_enrichment_threshold")]]
  )
  renderShinyText(paste(run$id, "enrichment_parameters", sep = "_"), parametersOutput)
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

  # Clear rendered outputs via registry (keeps outputs registered for re-use)
  outputRegistry$clearOutputs(fullRunKey, output)

  # Hide all source tabs (will be shown again as results populate)
  hideAllSourceTabsForRun(fullRunKey)
}
