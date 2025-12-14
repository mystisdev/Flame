handleEnrichment <- function(enrichmentType) {
  tryCatch({
    if (existInputGeneLists()) {
      currentEnrichmentType <<- enrichmentType
      if (existInputOrganism()) {
        if (existEnrichmentTool()) {
          resetCombination()
          
          tools <- switch(
            currentEnrichmentType,
            "functional" = input$functional_enrichment_tool,
            "literature" = "STRING"
          )

          # Clear results from tools that are not in the current selection
          if (currentEnrichmentType == "functional") {
            lapply(ENRICHMENT_TOOLS, function(toolName) {
              if (!(toolName %in% tools)) {
                type_tool <- paste("functional", toolName, sep = "_")
                if (exists("enrichmentResults") &&
                    type_tool %in% names(enrichmentResults) &&
                    nrow(enrichmentResults[[type_tool]]) > 0) {
                  resetEnrichmentResults("functional", toolName)
                  hideTab(inputId = "toolTabsPanel", target = toolName)
                }
              }
            })
          }

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
            currentType_Tool <<-
              paste(currentEnrichmentType, currentEnrichmentTool, sep = "_")
            currentSignificanceMetric <<- decideToolMetric()
            handleEnrichmentWithTool()
          })
          
          prepareCombinationTab()
        }
      }
    }
  }, error = function(e) {
    cat(paste("Functional enrichment analysis error:  ", e))
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

handleEnrichmentWithTool <- function() {
  renderModal(
    paste0(
      "<h2>Please wait.</h2><br />
      <p>Executing ", currentEnrichmentType, " enrichment
      with ", currentEnrichmentTool, ".</p>"
    )
  )
  if (existDataSources()) {
    session$sendCustomMessage("handler_hideSourceTabs", currentType_Tool)
    resetEnrichmentResults(currentEnrichmentType, currentEnrichmentTool)
    # Convert input gene symbols to tool-specific format (e.g., STRING IDs, ENSP IDs, etc.)
    inputGenesConversionTable <- geneConvert(currentUserList)
    if (validInputGenesConversionTable(inputGenesConversionTable)) {
      if(length(currentBackgroundList)==0)
        backgroundGenesConversionTable <- NULL
      else
        # Convert background genes using same method as input genes
        backgroundGenesConversionTable <- geneConvert(currentBackgroundList)
      # Pass converted gene IDs to enrichment analysis (not original symbols)
      runEnrichmentAnalysis(inputGenesConversionTable$target, backgroundGenesConversionTable$target)
      if (validEnrichmentResult()) {
        showTab(inputId = "toolTabsPanel", target = currentEnrichmentTool)
        noHitGenesCheckList <- executeNamespaceRollback(inputGenesConversionTable)
        printParameters()
        if (input[[paste0(currentEnrichmentType, "_enrichment_namespace")]] != "USERINPUT") {
          shinyjs::show(paste(currentType_Tool,
                              "conversionBoxes", sep = "_"))
          printUnconvertedGenes(inputGenesConversionTable, backgroundGenesConversionTable)
          printConversionTable(inputGenesConversionTable, backgroundGenesConversionTable)
        }
        findAndPrintNoHitGenes(noHitGenesCheckList)
        printResultTables()
        # Defer plot control updates until after tab switch (see server.R observer)
        pendingPickerUpdate <<- TRUE
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

  if (rollBackNamesFlag) {
    # Convert Positive Hits back from tool-specific IDs to original gene symbols
    enrichmentResults[[currentType_Tool]] <<-
      rollBackConvertedNames(enrichmentResults[[currentType_Tool]],
                             inputGenesConversionTable)

    # For no-hit calculation: use original gene symbols that successfully converted
    # Use only converted genes (inputGenesConversionTable$input),
    # NOT all user inputs, to distinguish between:
    # - "Unconverted Inputs" = genes that failed conversion
    # - "No-hit Inputs" = genes that converted but don't appear in any enriched term
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

    session$sendCustomMessage("handler_showSourceTab",
                              list(prefix = currentType_Tool,
                                   tabPosition = tabPosition))

    caption = "Enrichment Results"
    fileName <- paste(currentType_Tool, datasource, sep = "_")
    mode <- "Positive Hits"
    hiddenColumns <- c(0, 11, 12)
    expandableColumn <- 11

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
  # Clear ALL tools that have results, not just the currently selected ones
  lapply(ENRICHMENT_TOOLS, function(toolName) {
    type_tool <- paste("functional", toolName, sep = "_")
    # Check if this tool has any results stored
    if (exists("enrichmentResults") &&
        type_tool %in% names(enrichmentResults) &&
        nrow(enrichmentResults[[type_tool]]) > 0) {
      resetEnrichmentResults("functional", toolName)
      hideTab(inputId = "toolTabsPanel", target = toolName)
    }
  })
  prepareCombinationTab()
  shinyjs::hide("functional_enrichment_all_clear")
}
