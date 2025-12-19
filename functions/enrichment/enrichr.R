runEnrichr <- function(userInputList) {
  site <- setEnrichrOrganism()
  databases <- setEnrichrDatabases()
  
  result <- enrichR::enrichr(userInputList, databases)
  
  enrichrResult <- do.call(rbind, result) # extracting df from list of lists
  enrichrResult <- filterSignificance(enrichrResult)
  
  enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<- getEnrichrBackgroundSize(site, databases)
  
  if (isResultValid(enrichrResult)) {
    enrichrResult <- parseEnirchrResult(enrichrResult, length(userInputList))
    enrichmentResults[[currentType_Tool]] <<-
      transformEnrichmentResultTable(enrichrResult)
  }
}

setEnrichrOrganism <- function() {
  site <- switch(
    ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name,
    "hsapiens" = "Enrichr",
    "mmusculus" = "Enrichr",
    "dmelanogaster" = "FlyEnrichr",
    "celegans" = "WormEnrichr",
    "scerevisiae" = "YeastEnrichr",
    "drerio" = "FishEnrichr",
    "btaurus" = "OxEnrichr"
  )
  enrichR::setEnrichrSite(site)
  return(site)
}

setEnrichrDatabases <- function() {
  toolSources <- getEnrichrOrganismSourceCodes()
  selectedDatasources <-
    input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  databases <- as.character(toolSources[
    selectedDatasources[which(selectedDatasources %in% names(toolSources))]
  ])
  return(databases)
}

getEnrichrOrganismSourceCodes <- function() {
  prefix <- getEnrichrVariablePrefix()
  toolSources <- DATASOURCES_CODES[[paste0(prefix, "ENRICHR")]]
  return(toolSources)
}

getEnrichrVariablePrefix <- function() {
  return(
    switch(
      ORGANISMS[ORGANISMS$print_name ==
                  input[["functional_enrichment_organism"]], ]$short_name,
      "mmusculus" = "MOUSE_",
      "dmelanogaster" = "FLY_",
      "celegans" = "WORM_",
      "scerevisiae" = "YEAST_",
      "drerio" = "FISH_",
      "btaurus" = "OX_"
    )
  )
}

filterSignificance <- function(enrichrResult) {
  threshold <- as.numeric(input$functional_enrichment_threshold)
  enrichrResult <- enrichrResult[enrichrResult$Adjusted.P.value <= threshold, ]
  return(enrichrResult)
}

parseEnirchrResult <- function(enrichrResult, numInputs) {
  enrichrResult$database <- appendEnrichrDatabases(rownames(enrichrResult))
  rownames(enrichrResult) <- NULL
  toolSources <- getEnrichrOrganismSourceCodes()
  enrichrResult$database <- 
    unlistDatasourceCodes(enrichrResult$database, toolSources)
  enrichrResult$querySize <- numInputs
  enrichrResult$Genes <- gsub(";", ",", enrichrResult$Genes)
  enrichrResult <- enrichrResult %>%
    tidyr::separate(Overlap, c("overlap", "size"), sep="\\/")
  enrichrResult$overlap <- as.numeric(enrichrResult$overlap)
  enrichrResult$size <- as.numeric(enrichrResult$size)
  result <- splitEnrichrTermIds(enrichrResult)
  enrichrResult$Term <- result$terms
  enrichrResult$TermId <- result$ids
  
  enrichrResult <-
    enrichrResult[, c(
      "database", "TermId", "Term", "Adjusted.P.value",
      "size", "querySize", "overlap", "Genes")]
  colnames(enrichrResult) <- ENRICHMENT_DF_COLNAMES
  enrichrResult <- mapKEGGIds(enrichrResult)
  return(enrichrResult)
}

appendEnrichrDatabases <- function(rowNames) {
  return(sapply((strsplit(rowNames, "\\.")), "[[", 1))
}

# Configuration-driven term parsing patterns
# Each pattern defines how to extract term names and IDs from enrichR results
ENRICHR_TERM_PATTERNS <- list(
  GO = list(dbPattern = "^GO:", separator = " \\(GO:", idPrefix = "GO:", trimSuffix = TRUE),
  KEGG_HUMAN = list(dbPattern = "^KEGG$", separator = " Homo sapiens ", idPrefix = "", trimSuffix = FALSE),
  KEGG_YEAST = list(dbPattern = "^KEGG$", parseMode = "fixed", termEnd = -8, idStart = -7, idEnd = 0),
  REAC = list(dbPattern = "^REAC$", separator = " R-", idPrefix = "R-", trimSuffix = FALSE),
  WP = list(dbPattern = "^WP$", separator = "WP", idPrefix = "WP", trimSuffix = FALSE),
  PANTHER = list(dbPattern = "^PANTHER Pathways$", separator = " Homo sapiens ", idPrefix = "", trimSuffix = FALSE),
  DO = list(dbPattern = "^DO$", separator = "\\(DOID:", idPrefix = "DOID:", trimSuffix = TRUE),
  WBP = list(dbPattern = "^WBP$", separator = "_WBPhenotype:", idPrefix = "WBPhenotype:", trimSuffix = FALSE),
  WBBT = list(dbPattern = "^WBBT$", separator = "\\(WBbt:", idPrefix = "WBbt:", trimSuffix = TRUE),
  ORPHA = list(dbPattern = "^ORPHA$", separator = " ORPHA:", idPrefix = "ORPHA:", trimSuffix = FALSE),
  MGI_MOUSE = list(dbPattern = "^MGI$", separator = " \\(MP:", idPrefix = "MP:", trimSuffix = TRUE),
  MGI_OX = list(dbPattern = "^MGI$", parseMode = "fixed", termStart = 12, idStart = 0, idEnd = 10),
  HP = list(dbPattern = "^HP$", separator = " \\(HP:", idPrefix = "HP:", trimSuffix = TRUE)
)

# Generic function to parse term IDs for any database type
# Handles both split-based and fixed-position parsing modes
parseEnrichrTermsForDb <- function(enrichrResult, config) {
  items <- enrichrResult[grep(config$dbPattern, enrichrResult$database), ]$Term
  if (identical(items, character(0))) {
    return(list(terms = c(), ids = c()))
  }

  if (!is.null(config$parseMode) && config$parseMode == "fixed") {
    # Fixed-position extraction (KEGG_YEAST, MGI_OX)
    if (!is.null(config$termEnd)) {
      terms <- trimws(substr(items, 1, nchar(items) + config$termEnd))
    } else {
      terms <- substr(items, config$termStart, nchar(items))
    }
    ids <- substr(items, nchar(items) + config$idStart + 1, nchar(items) + config$idEnd)
  } else {
    # Split-based extraction (most common)
    splitList <- strsplit(items, config$separator)
    terms <- sapply(splitList, "[[", 1)
    ids <- sapply(splitList, function(x) if (length(x) > 1) x[[2]] else "")

    if (!is.null(config$trimSuffix) && config$trimSuffix) {
      ids <- substring(ids, 1, nchar(ids) - 1)
    }
    ids <- paste0(config$idPrefix, ids)
  }

  return(list(terms = terms, ids = ids))
}

# Main function to split all term IDs based on organism-specific patterns
splitEnrichrTermIds <- function(enrichrResult) {
  terms <- c()
  ids <- c()
  organism <- ORGANISMS[ORGANISMS$taxid == currentOrganism, ]$short_name

  # Define which patterns to use based on organism
  patternsToUse <- c("GO", "REAC", "WP", "DO", "WBP", "WBBT", "ORPHA", "HP")

  # Add organism-specific patterns for KEGG
  if (organism == "hsapiens") {
    patternsToUse <- c(patternsToUse, "KEGG_HUMAN", "PANTHER")
  } else {
    patternsToUse <- c(patternsToUse, "KEGG_YEAST")
  }

  # Add organism-specific patterns for MGI
  if (organism == "mmusculus") {
    patternsToUse <- c(patternsToUse, "MGI_MOUSE")
  } else {
    patternsToUse <- c(patternsToUse, "MGI_OX")
  }

  # Parse all patterns using the generic function
  for (patternKey in patternsToUse) {
    config <- ENRICHR_TERM_PATTERNS[[patternKey]]
    if (!is.null(config)) {
      result <- parseEnrichrTermsForDb(enrichrResult, config)
      terms <- c(terms, result$terms)
      ids <- c(ids, result$ids)
    }
  }

  return(list(terms = terms, ids = ids))
}

getEnrichrBackgroundSize <- function(site, selected_dbs) {
  if(site == "Enrichr") {
    dbs_all <- listEnrichrDbs()
    size <- max(unlist(dbs_all[dbs_all$libraryName %in% selected_dbs,]$numTerms))
  }
  else
    size <- NULL
  return(size)
}