# =============================================================================
# ENRICHMENT MAIN - Helper Functions for Enrichment Sessions
# =============================================================================
# This file contains helper functions used by EnrichmentFormSession and
# ORAEnrichmentSession for tab insertion, result printing, cleanup, etc.
#
# NOTE: The main enrichment handling (handleEnrichment, handleMultiClear,
# cascade logic, parameter matching) has been moved to EnrichmentFormSession
# in enrich-form.R.
# =============================================================================

# Insert tab for a session (called from EnrichmentFormSession)
# IMPORTANT: parentSession must be the main app session, not a module session
insertEnrichmentTabForSession <- function(enrichSession, config, parentSession) {
  runId <- enrichSession$runId
  tabTitle <- paste0(enrichSession$toolName, " (", enrichSession$displayNumber, ")")

  # Generate tab content
  tabContent <- generateToolPanelForRun(
    "functional", enrichSession$toolName, enrichSession$uniqueId
  )

  # Create tab title with close button
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

  # Insert the tab - must use parentSession since toolTabsPanel is in parent UI
  shiny::insertTab(
    inputId = config$tabsetPanelId,
    tab = tabPanel(
      title = tabTitleHtml,
      value = runId,
      tabContent
    ),
    select = TRUE,
    session = parentSession
  )
}

# Print parameters for a session
printParametersForSession <- function(enrichSession, listName) {
  enrichmentType <- strsplit(enrichSession$id, "_")[[1]][1]
  bgSize <- enrichSession$getBackgroundSize()
  bgSizeDisplay <- if (is.null(bgSize)) "Genome-wide (tool default)" else bgSize
  params <- enrichSession$getParameters()

  # Get background mode from params (stored during capture)
  bgMode <- if (!is.null(params$backgroundMode)) params$backgroundMode else "genome"

  # Get datasources from session results
  results <- enrichSession$getResults()
  datasourcesDisplay <- if (!is.null(results) && nrow(results) > 0) {
    paste(unique(results$Source), collapse = ", ")
  } else {
    paste(params$datasources, collapse = ", ")
  }

  parametersOutput <- paste0(
    "Run: ", enrichSession$toolName, " (", enrichSession$displayNumber, ")",
    "\nFile: ", listName,
    "\nOrganism: ", ORGANISMS[ORGANISMS$taxid == enrichSession$organism, ]$print_name,
    "\nBackground: ", bgMode,
    "\nBackground size (no. of genes): ", bgSizeDisplay,
    "\nDatasources: ", datasourcesDisplay,
    "\nNamespace: ", params$namespace,
    "\nSignificance metric: ", params$metric,
    "\nSignificance threshold: ", params$threshold
  )
  renderShinyText(paste(enrichSession$id, "enrichment_parameters", sep = "_"),
                  parametersOutput)
}

# Find and print no-hit genes using session data
findAndPrintNoHitGenesFromSession <- function(checkList, enrichSession) {
  results <- enrichSession$getResults()
  if (is.null(results) || nrow(results) == 0) {
    printNoHitGenes(checkList, runKey = enrichSession$id)
    return()
  }

  # Collect all genes from Positive Hits column
  allHitGenes <- paste(results$`Positive Hits`, collapse = ",")
  allHitGenes <- strsplit(allHitGenes, ",")[[1]]
  allHitGenes <- unique(allHitGenes)

  # Find genes not in any term
  noHitGenes <- checkList[!checkList %in% allHitGenes]
  printNoHitGenes(noHitGenes, runKey = enrichSession$id)
}

# Print result tables using session data
printResultTablesFromSession <- function(enrichSession) {
  runKey <- enrichSession$id
  results <- enrichSession$getResults()

  if (is.null(results) || nrow(results) == 0) {
    return()
  }

  # Print "All" table
  shinyOutputId <- paste(runKey, "table_all", sep = "_")
  printResultTableWithData(shinyOutputId, results, "all", runKey)

  # Print per-datasource tables
  params <- enrichSession$getParameters()
  datasources <- params$datasources
  lapply(datasources, function(datasource) {
    partialId <- as.character(TAB_NAMES[datasource])
    shinyOutputId <- paste(runKey, "table", partialId, sep = "_")
    pattern <- paste0("^", datasource, "$")
    matches <- grepl(pattern, results$Source)
    filteredResults <- results[matches, ]

    if (nrow(filteredResults) > 0) {
      printResultTableWithData(shinyOutputId, filteredResults, datasource, runKey)
    }
  })
}

# Print a single result table with provided data
printResultTableWithData <- function(shinyOutputId, data, datasource, runKey) {
  if (nrow(data) > 0) {
    data$`Positive Hits` <- gsub(",", ", ", data$`Positive Hits`)
    showSourceTabForRun(runKey, datasource)

    caption <- "Enrichment Results"
    fileName <- paste(runKey, datasource, sep = "_")
    mode <- "Positive Hits"
    hiddenColumns <- c(10, 11)
    expandableColumn <- 10

    # Convert Source to factor for dropdown filtering
    data$Source <- as.factor(data$Source)

    renderEnrichmentTable(shinyOutputId, data,
                          caption, fileName, mode,
                          hiddenColumns, expandableColumn, filter = 'top')
  }
}

# =============================================================================
# GENE CONVERSION FUNCTIONS
# =============================================================================
# Used by ORAEnrichmentSession for ID conversion

# Convert gene symbols to tool-specific identifier format
# Returns data.frame with columns: input (original), target (converted), name
# Also attaches 'namespace' attribute to result
geneConvert <- function(geneList, enrichmentType, organism, toolName, namespace) {
  if (namespace == DEFAULT_NAMESPACE_TEXT) {
    namespace <- getDefaultTargetNamespace(toolName, organism)
  }

  if (namespace != "USERINPUT") {
    if (toolName == "STRING") {
      # For STRING: Convert to STRING format via STRING's get_string_ids API
      inputGenesConversionTable <- stringPOSTConvertENSP(geneList, organism)
    } else if (toolName == "PANTHER") {
      # For PANTHER: Use PANTHER's geneinfo API for gene mapping
      inputGenesConversionTable <- pantherPOSTConvert(geneList, organism)
    } else if (toolName == "GeneCodis") {
      # For GeneCodis: No conversion needed - accepts multiple ID formats
      inputGenesConversionTable <- data.frame(
        "input" = geneList,
        "target" = geneList,
        "name" = geneList
      )
    } else {
      # For gProfiler, WebGestalt, enrichR: Use g:Profiler conversion
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

  # Attach namespace as attribute
  attr(inputGenesConversionTable, "namespace") <- namespace
  return(inputGenesConversionTable)
}

# Get default target namespace for a tool
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
  } else {
    inputGenesConversionTable <- NULL
  }
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
    if ("search" %in% names(pantherResponse) &&
        "mapped_genes" %in% names(pantherResponse$search)) {
      mappedGenes <- pantherResponse$search$mapped_genes$gene
      if (length(mappedGenes) > 0) {
        inputGenesConversionTable <- data.frame(
          input = mappedGenes$mapped_id_list,
          target = mappedGenes$accession,
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

# =============================================================================
# GENE REPORT HELPER FUNCTIONS
# =============================================================================

# Helper function for rendering gene report text
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
                                   runKey, originalInputs,
                                   originalBackground = NULL) {
  # Display genes that failed conversion to target namespace

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
    unconvertedBackground <- originalBackground[
      !originalBackground %in% convertedOutputs$input]
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

printConversionTable <- function(inputConversionTable,
                                  backgroundConversionTable = NULL, runKey) {
  # Display gene conversion table for input list and optional background list

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
    renderShinyDataTable(shinyOutputId, backgroundConversionTable,
                         fileName = fileName)
    shinyjs::show(refDivId)
  }
}

findNoHitGenes <- function(convertedInputs, runKey) {
  # Find genes that successfully converted but don't appear in any enriched term
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
  shinyOutputId <- paste(runKey, "genesNotFound", sep = "_")
  renderGeneReport(
    outputId = shinyOutputId,
    genes = noHitGenes,
    messageTemplate = "%d input item(s) not found in any result term:\n%s"
  )
}

# =============================================================================
# TAB VISIBILITY FUNCTIONS
# =============================================================================

hideAllSourceTabsForRun <- function(fullRunKey, parentSession = NULL) {
  sourcePanelId <- paste(fullRunKey, "sources_panel", sep = "_")
  # Use parent session if provided, otherwise default
  sess <- if (!is.null(parentSession)) {
    parentSession
  } else {
    shiny::getDefaultReactiveDomain()
  }
  # Hide all datasource tabs
  lapply(names(TAB_NAMES), function(tabTitle) {
    hideTab(inputId = sourcePanelId, target = tabTitle, session = sess)
  })
}

showSourceTabForRun <- function(fullRunKey, datasource, parentSession = NULL) {
  sourcePanelId <- paste(fullRunKey, "sources_panel", sep = "_")
  # Use parent session if provided, otherwise default
  sess <- if (!is.null(parentSession)) {
    parentSession
  } else {
    shiny::getDefaultReactiveDomain()
  }
  # Map datasource to tab title
  tabTitle <- if (datasource == "all") {
    "ALL"
  } else if (datasource == "pubmed") {
    "PUBMED"
  } else {
    datasource
  }
  showTab(inputId = sourcePanelId, target = tabTitle, session = sess)
}

# =============================================================================
# CLEANUP FUNCTIONS
# =============================================================================

# Unified function to clear a single run completely (data, state, tab)
clearEnrichmentRun <- function(fullRunKey) {
  runInfo <- parseFullRunKey(fullRunKey)
  config <- getEnrichmentConfig(runInfo$enrichmentType)

  # Get session from registry and call cleanup
  enrichSession <- enrichmentSessionRegistry$get(fullRunKey)
  if (!is.null(enrichSession)) {
    enrichSession$cleanup()
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

  # Remove from enrichment session registry
  enrichmentSessionRegistry$remove(fullRunKey)
}

# Legacy alias for backward compatibility
clearRunCompletely <- clearEnrichmentRun

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
