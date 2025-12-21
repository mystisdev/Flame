# Run Classes for Multi-Run Enrichment Architecture
# Contains abstract base class and concrete implementations for different analysis types
#
# Usage:
#   run <- ORAEnrichmentRun$new(enrichmentType, toolName, organism, geneList, ...)
#   run$executeWithConvertedLists(convertedList, convertedBackground)

library(R6)

# =============================================================================
# AnalysisRun - Abstract Base Class
# =============================================================================
#
# Base class for enrichment analysis runs.
# Provides identity management and storage.

AnalysisRun <- R6::R6Class("AnalysisRun",
  public = list(
    # === Identity (common to all) ===
    id = NULL,              # fullRunKey, e.g., "functional_gProfiler_1"
    runId = NULL,           # e.g., "gProfiler_1"
    uniqueId = NULL,        # Internal Shiny ID (never resets)
    runNumber = NULL,       # Display number (resets on Clear All)

    # === Context (common to all biology) ===
    analysisType = NULL,    # 'ora', 'gsea', 'topology'
    enrichmentType = NULL,  # 'functional', 'literature' (UI section)
    toolName = NULL,
    organism = NULL,

    # === State ===
    parameters = list(),  # Named 'parameters' to match existing activeRuns structure
    results = NULL,
    timestamp = NULL,

    # === Initialization ===
    initialize = function(enrichmentType, toolName, organism) {
      self$enrichmentType <- enrichmentType
      self$toolName <- toolName
      self$organism <- organism
      self$timestamp <- Sys.time()

      # Generate IDs using existing helper functions from runs.R
      self$uniqueId <- getNextUniqueId(toolName)
      self$runNumber <- getNextRunNumber(toolName)
      self$runId <- createRunId(toolName, self$uniqueId)
      self$id <- getFullRunKey(enrichmentType, self$runId)
    },

    # === Storage ===
    save = function() {
      # Store Run object in activeRuns
      activeRuns[[self$id]] <<- self
    }
  )
)


# =============================================================================
# ORAEnrichmentRun - Over-Representation Analysis Implementation
# =============================================================================
#
# Handles standard ORA enrichment using gene lists against background.
# Used by: gProfiler, enrichR, WebGestalt, STRING, PANTHER, GeneCodis

ORAEnrichmentRun <- R6::R6Class("ORAEnrichmentRun",
  inherit = AnalysisRun,

  public = list(
    # === ORA-specific data ===
    analysisType = "ora",
    geneList = NULL,
    backgroundList = NULL,
    convertedList = NULL,
    convertedBackground = NULL,
    significanceMetric = NULL,

    # === ORA-specific initialization ===
    initialize = function(enrichmentType, toolName, organism,
                          geneList, backgroundList = NULL,
                          significanceMetric = NULL) {
      super$initialize(enrichmentType, toolName, organism)
      self$parameters <- list()  # Initialize empty to prevent NULL cascade
      self$geneList <- geneList
      self$backgroundList <- backgroundList
      self$significanceMetric <- significanceMetric
    },

    # === Main Execution Method ===
    # This method replaces runEnrichmentAnalysis() by using the Run object directly.
    # It stores results in BOTH the Run object AND globals for UI compatibility.
    #
    # @param convertedInputList Gene list after ID conversion (from geneConvert)
    # @param convertedBackgroundList Background list after ID conversion (or NULL)
    # @return TRUE if successful, FALSE if strategy missing or no results
    executeWithConvertedLists = function(convertedInputList, convertedBackgroundList = NULL) {
      # Store converted lists for potential later use
      self$convertedList <- convertedInputList
      self$convertedBackground <- convertedBackgroundList

      # Check if we have a strategy
      if (!exists("toolRegistry") || !toolRegistry$hasStrategy(self$enrichmentType, self$toolName)) {
        warning(paste("No strategy for", self$toolName, "- no strategy registered"))
        return(FALSE)
      }

      # Set currentType_Tool to run ID before strategy execution
      # Strategies use this global to store background sizes and per-run caches
      currentType_Tool <<- self$id

      # Get strategy and execute
      strategy <- toolRegistry$get(self$enrichmentType, self$toolName)
      result <- strategy$run(
        convertedInputList,
        self$organism,
        convertedBackgroundList,
        self$parameters
      )

      # Store in Run object
      self$results <- result

      # Store in global for UI compatibility (TEMPORARY - will be removed)
      if (!is.null(result) && nrow(result) > 0) {
        enrichmentResults[[self$id]] <<- transformEnrichmentResultTable(result)
        return(TRUE)
      }

      return(FALSE)
    },

    # === Utility Methods for Plot Handlers ===

    # Get Shiny input ID with run-specific prefix
    # @param componentId The component suffix (e.g., "barchart_sourceSelect")
    # @return Full input ID (e.g., "functional_gProfiler_1_barchart_sourceSelect")
    getInputId = function(componentId) {
      paste(self$id, componentId, sep = "_")
    },

    # Get results (returns transformed data from global enrichmentResults)
    # The global contains the properly transformed table with all columns
    # (Source, Positive Hits, etc.) that plots and tables expect
    # @return Data frame of enrichment results, or NULL if none
    getResults = function() {
      return(enrichmentResults[[self$id]])
    },

    # Check if results exist
    # @return TRUE if transformed results are available in global
    hasResults = function() {
      results <- enrichmentResults[[self$id]]
      return(!is.null(results) && is.data.frame(results) && nrow(results) > 0)
    }
  )
)
