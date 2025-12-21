# Get organism-specific prefix for EnrichR database codes
# Used by UI update functions to get correct datasource options
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
splitEnrichrTermIds <- function(enrichrResult, organismTaxid = NULL) {
  # Accept organism parameter instead of reading global
  # Fall back to global for backward compatibility
  if (is.null(organismTaxid)) organismTaxid <- currentOrganism

  terms <- c()
  ids <- c()
  organism <- ORGANISMS[ORGANISMS$taxid == organismTaxid, ]$short_name

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


# =============================================================================
# EnrichRStrategy - Tool Strategy Implementation
# =============================================================================
#
# Implements the ToolStrategy interface for enrichR.
# This class wraps the existing enrichR functions but:
# - Takes explicit parameters instead of reading globals
# - Returns results instead of storing in globals

EnrichRStrategy <- R6::R6Class("EnrichRStrategy",

  inherit = ToolStrategy,

  public = list(
    initialize = function() {
      super$initialize("enrichR")
    },

    # Execute enrichR analysis
    # @param inputList Character vector of gene symbols
    # @param organism Organism taxid
    # @param backgroundList Not used by enrichR (always uses database background)
    # @param params List with: datasources, threshold
    # @return Data frame of enrichment results, or NULL
    run = function(inputList, organism, backgroundList, params) {
      # Set up enrichR site based on organism
      site <- private$getEnrichrSite(organism)
      enrichR::setEnrichrSite(site)

      # Get database codes for selected datasources
      databases <- private$getDatabaseCodes(organism, params$datasources)

      if (length(databases) == 0) {
        warning("No valid enrichR databases selected")
        return(NULL)
      }

      # Call enrichR API
      result <- enrichR::enrichr(inputList, databases)

      # Combine results from all databases
      enrichrResult <- do.call(rbind, result)

      # Filter by significance threshold
      threshold <- as.numeric(params$threshold)
      enrichrResult <- enrichrResult[enrichrResult$Adjusted.P.value <= threshold, ]

      # Store background size (still using global for now - will be refactored)
      if (exists("currentType_Tool")) {
        enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<-
          getEnrichrBackgroundSize(site, databases)
      }

      # Check if we have valid results
      if (!isResultValid(enrichrResult)) {
        return(NULL)
      }

      # Parse results into standard format
      enrichrResult <- private$parseResult(enrichrResult, length(inputList), organism)

      return(enrichrResult)
    },

    # enrichR uses gene symbols directly - no conversion needed
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    },

    # Get valid datasources for enrichR based on organism
    getValidDatasources = function(organism) {
      prefix <- private$getVariablePrefix(organism)
      return(names(DATASOURCES_CODES[[paste0(prefix, "ENRICHR")]]))
    },

    # enrichR only supports FDR (Adjusted.P.value)
    getDefaultMetric = function(hasBackground) {
      return("fdr")
    }
  ),

  private = list(
    # Map organism taxid to enrichR site name
    getEnrichrSite = function(organism) {
      shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
      site <- switch(shortName,
        "hsapiens" = "Enrichr",
        "mmusculus" = "Enrichr",
        "dmelanogaster" = "FlyEnrichr",
        "celegans" = "WormEnrichr",
        "scerevisiae" = "YeastEnrichr",
        "drerio" = "FishEnrichr",
        "btaurus" = "OxEnrichr",
        "Enrichr"  # default
      )
      return(site)
    },

    # Get organism-specific variable prefix for datasource lookup
    getVariablePrefix = function(organism) {
      shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name
      prefix <- switch(shortName,
        "mmusculus" = "MOUSE_",
        "dmelanogaster" = "FLY_",
        "celegans" = "WORM_",
        "scerevisiae" = "YEAST_",
        "drerio" = "FISH_",
        "btaurus" = "OX_",
        ""  # default (human)
      )
      return(prefix)
    },

    # Convert UI datasource names to enrichR database codes
    getDatabaseCodes = function(organism, selectedDatasources) {
      prefix <- private$getVariablePrefix(organism)
      toolSources <- DATASOURCES_CODES[[paste0(prefix, "ENRICHR")]]
      databases <- as.character(toolSources[
        selectedDatasources[which(selectedDatasources %in% names(toolSources))]
      ])
      return(databases)
    },

    # Parse enrichR result into standard format
    parseResult = function(enrichrResult, numInputs, organism) {
      # Add database column from rownames
      enrichrResult$database <- sapply(strsplit(rownames(enrichrResult), "\\."), "[[", 1)
      rownames(enrichrResult) <- NULL

      # Convert database codes back to display names
      prefix <- private$getVariablePrefix(organism)
      toolSources <- DATASOURCES_CODES[[paste0(prefix, "ENRICHR")]]
      enrichrResult$database <- unlistDatasourceCodes(enrichrResult$database, toolSources)

      enrichrResult$querySize <- numInputs
      enrichrResult$Genes <- gsub(";", ",", enrichrResult$Genes)

      # Split overlap column
      enrichrResult <- enrichrResult %>%
        tidyr::separate(Overlap, c("overlap", "size"), sep = "\\/")
      enrichrResult$overlap <- as.numeric(enrichrResult$overlap)
      enrichrResult$size <- as.numeric(enrichrResult$size)

      # Parse term IDs (uses organism-specific patterns)
      result <- private$splitTermIds(enrichrResult, organism)
      enrichrResult$Term <- result$terms
      enrichrResult$TermId <- result$ids

      # Select and rename columns to standard format
      enrichrResult <- enrichrResult[, c(
        "database", "TermId", "Term", "Adjusted.P.value",
        "size", "querySize", "overlap", "Genes"
      )]
      colnames(enrichrResult) <- ENRICHMENT_DF_COLNAMES

      # Map KEGG IDs
      enrichrResult <- mapKEGGIds(enrichrResult)

      return(enrichrResult)
    },

    # Split term IDs based on organism-specific patterns
    # Accept organism parameter instead of reading global
    splitTermIds = function(enrichrResult, organism) {
      return(splitEnrichrTermIds(enrichrResult, organism))
    }
  )
)

# Register the strategy with the global registry
toolRegistry$register("functional", "enrichR", EnrichRStrategy$new())