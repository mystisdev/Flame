# =============================================================================
# Enrichr ORA Strategy
# =============================================================================
#
# Implements Over-Representation Analysis using the Enrichr API.
#
# Dependencies:
#   - config.R (for ToolId, ParadigmId, ORGANISMS, TOOLS, ENRICHMENT_DF_COLNAMES)
#   - enrich-strategy-base.R (for EnrichmentStrategy, strategyRegistry)
#   - enrichR package
#   - tidyr package
#
# =============================================================================

EnrichRORAStrategy <- R6::R6Class("EnrichRORAStrategy",

  inherit = EnrichmentStrategy,

  public = list(
    initialize = function() {
      super$initialize(ToolId$ENRICHR, ParadigmId$ORA)
    },

    run = function(inputList, organism, backgroundList, params) {
      # Get organism short name
      shortName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name

      # Set up enrichR site based on organism
      site <- private$getEnrichrSite(shortName)
      enrichR::setEnrichrSite(site)

      # Get database codes for selected datasources
      databases <- private$getDatabaseCodes(shortName, params$datasources)

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

      # Calculate background size
      backgroundSize <- private$getEnrichrBackgroundSize(site, databases)

      # Check if we have valid results
      if (!private$isResultValid(enrichrResult)) {
        return(NULL)
      }

      # Parse results into standard format
      enrichrResult <- private$parseResult(enrichrResult, length(inputList), shortName)

      # Filter by datasources
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        enrichrResult <- enrichrResult[enrichrResult$Source %in% params$datasources, ]
      }

      if (nrow(enrichrResult) == 0) {
        return(NULL)
      }

      # Return structured result
      return(list(
        result = enrichrResult,
        backgroundSize = backgroundSize
      ))
    },

    # enrichR uses gene symbols directly - no conversion needed
    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    }
  ),

  private = list(
    # -------------------------------------------------------------------------
    # Term parsing patterns (configuration)
    # -------------------------------------------------------------------------
    termPatterns = list(
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
    ),

    # -------------------------------------------------------------------------
    # Core private methods
    # -------------------------------------------------------------------------

    getEnrichrSite = function(shortName) {
      toolConfig <- TOOLS[[self$toolId]]
      site <- toolConfig$organismSites[[shortName]]
      if (is.null(site)) site <- "Enrichr"
      return(site)
    },

    getDatabaseCodes = function(shortName, selectedDatasources) {
      toolConfig <- TOOLS[[self$toolId]]
      datasourceCodes <- toolConfig$organismDatasourceCodes[[shortName]]
      if (is.null(datasourceCodes)) {
        datasourceCodes <- toolConfig$organismDatasourceCodes[["hsapiens"]]
      }

      databases <- as.character(datasourceCodes[
        selectedDatasources[which(selectedDatasources %in% names(datasourceCodes))]
      ])
      return(databases)
    },

    parseResult = function(enrichrResult, numInputs, shortName) {
      # Add database column from rownames
      enrichrResult$database <- sapply(strsplit(rownames(enrichrResult), "\\."), "[[", 1)
      rownames(enrichrResult) <- NULL

      # Convert database codes back to display names
      toolConfig <- TOOLS[[self$toolId]]
      datasourceCodes <- toolConfig$organismDatasourceCodes[[shortName]]
      if (is.null(datasourceCodes)) {
        datasourceCodes <- toolConfig$organismDatasourceCodes[["hsapiens"]]
      }
      enrichrResult$database <- private$unlistDatasourceCodes(enrichrResult$database, datasourceCodes)

      enrichrResult$querySize <- numInputs
      enrichrResult$Genes <- gsub(";", ",", enrichrResult$Genes)

      # Split overlap column
      enrichrResult <- enrichrResult %>%
        tidyr::separate(Overlap, c("overlap", "size"), sep = "\\/")
      enrichrResult$overlap <- as.numeric(enrichrResult$overlap)
      enrichrResult$size <- as.numeric(enrichrResult$size)

      # Parse term IDs
      result <- private$splitEnrichrTermIds(enrichrResult, shortName)
      enrichrResult$Term <- result$terms
      enrichrResult$TermId <- result$ids

      # Select and rename columns to standard format
      enrichrResult <- enrichrResult[, c(
        "database", "TermId", "Term", "Adjusted.P.value",
        "size", "querySize", "overlap", "Genes"
      )]
      colnames(enrichrResult) <- ENRICHMENT_DF_COLNAMES

      # Map KEGG IDs
      enrichrResult <- private$mapKEGGIds(enrichrResult)

      return(enrichrResult)
    },

    # -------------------------------------------------------------------------
    # enrichR-specific helper methods
    # -------------------------------------------------------------------------

    getEnrichrBackgroundSize = function(site, selected_dbs) {
      if (site == "Enrichr") {
        dbs_all <- listEnrichrDbs()
        size <- max(unlist(dbs_all[dbs_all$libraryName %in% selected_dbs, ]$numTerms))
      } else {
        size <- NULL
      }
      return(size)
    },

    parseEnrichrTermsForDb = function(enrichrResult, config) {
      items <- enrichrResult[grep(config$dbPattern, enrichrResult$database), ]$Term
      if (identical(items, character(0))) {
        return(list(terms = c(), ids = c()))
      }

      if (!is.null(config$parseMode) && config$parseMode == "fixed") {
        if (!is.null(config$termEnd)) {
          terms <- trimws(substr(items, 1, nchar(items) + config$termEnd))
        } else {
          terms <- substr(items, config$termStart, nchar(items))
        }
        ids <- substr(items, nchar(items) + config$idStart + 1, nchar(items) + config$idEnd)
      } else {
        splitList <- strsplit(items, config$separator)
        terms <- sapply(splitList, "[[", 1)
        ids <- sapply(splitList, function(x) if (length(x) > 1) x[[2]] else "")

        if (!is.null(config$trimSuffix) && config$trimSuffix) {
          ids <- substring(ids, 1, nchar(ids) - 1)
        }
        ids <- paste0(config$idPrefix, ids)
      }

      return(list(terms = terms, ids = ids))
    },

    splitEnrichrTermIds = function(enrichrResult, organismShortName) {
      if (is.null(organismShortName)) {
        warning("splitEnrichrTermIds: organismShortName is required")
        return(enrichrResult)
      }

      terms <- c()
      ids <- c()

      patternsToUse <- c("GO", "REAC", "WP", "DO", "WBP", "WBBT", "ORPHA", "HP")

      if (organismShortName == "hsapiens") {
        patternsToUse <- c(patternsToUse, "KEGG_HUMAN", "PANTHER")
      } else {
        patternsToUse <- c(patternsToUse, "KEGG_YEAST")
      }

      if (organismShortName == "mmusculus") {
        patternsToUse <- c(patternsToUse, "MGI_MOUSE")
      } else {
        patternsToUse <- c(patternsToUse, "MGI_OX")
      }

      for (patternKey in patternsToUse) {
        config <- private$termPatterns[[patternKey]]
        if (!is.null(config)) {
          result <- private$parseEnrichrTermsForDb(enrichrResult, config)
          terms <- c(terms, result$terms)
          ids <- c(ids, result$ids)
        }
      }

      return(list(terms = terms, ids = ids))
    }
  )
)

# Register the strategy
strategyRegistry$register(ToolId$ENRICHR, ParadigmId$ORA, EnrichRORAStrategy$new())
