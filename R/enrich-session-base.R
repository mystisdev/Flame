# =============================================================================
# FLAME Enrichment Session Base Class
# =============================================================================
#
# Abstract base class for all enrichment session types (ORA, GSEA, etc.).
# Provides common functionality for:
# - Results storage and access
# - Observer lifecycle management
# - Cleanup
#
# Subclasses must implement:
# - execute(): Run the enrichment and store results
# - ui(): Generate the tab UI (session owns its UI)
# - server(): Set up observers using moduleServer()
# - cleanup(output): Clean up observers and clear outputs
#
# Dependencies:
# - enrich-session-registry.R (for EnrichmentSessionRegistry)
# - infrastructure-config.R (for config)
#
# =============================================================================

#' Abstract Enrichment Session Base Class
#'
#' Base class for all enrichment session types. Each instance represents
#' one enrichment run and owns its results.
#'
#' @section Lifecycle:
#' 1. Create: session <- ORAEnrichmentSession$new(...)
#' 2. Execute: session$execute()
#' 3. Insert tab: EnrichmentController$insertTab() handles UI generation
#' 4. Start server: session$server(input, output, session)
#' 5. Cleanup: session$cleanup()
#'
#' @section Results Ownership:
#' Results are stored in private$.results, NOT in globals.
#' Access via session$getResults() or session$hasResults().
#'
EnrichmentSession <- R6::R6Class(
  "EnrichmentSession",

  public = list(
    # === Identity ===
    #' @field id Unique session ID (e.g., "functional_gProfiler_5")
    id = NULL,

    #' @field runId Run ID for tab panel (e.g., "gProfiler_5")
    runId = NULL,

    #' @field uniqueId Unique counter (never resets - for Shiny element IDs)
    uniqueId = NULL,

    #' @field displayNumber Display number for tab title (can reset)
    displayNumber = NULL,

    #' @field toolName Tool name (e.g., "gProfiler", "STRING")
    toolName = NULL,

    #' @field organism Organism taxid
    organism = NULL,

    #' @field enrichmentType Enrichment type (e.g., "functional")
    enrichmentType = NULL,

    #' Initialize an EnrichmentSession
    #' @param id Character. Unique session ID.
    #' @param runId Character. Run ID for tab panel.
    #' @param uniqueId Integer. Unique counter for Shiny IDs.
    #' @param displayNumber Integer. Display number for tab title.
    #' @param toolName Character. Tool name.
    #' @param organism Integer. Organism taxid.
    #' @param input AnalyteList. Input gene list.
    #' @param parameters List. Enrichment parameters.
    initialize = function(id, runId, uniqueId, displayNumber, toolName, organism,
                          input, parameters = list()) {
      self$id <- id
      self$runId <- runId
      self$uniqueId <- uniqueId
      self$displayNumber <- displayNumber
      self$toolName <- toolName
      self$organism <- organism
      # Parse enrichmentType from id (format: "functional_toolName_uniqueId")
      self$enrichmentType <- strsplit(id, "_")[[1]][1]
      private$.input <- input
      private$.parameters <- parameters
      private$.observers <- list()
    },

    # === Abstract Methods (must be implemented by subclasses) ===

    #' Execute the enrichment (abstract)
    #' @return Invisible self for chaining
    execute = function() {
      stop("Subclass must implement execute()")
    },

    #' Generate the tab UI (abstract)
    #'
    #' Subclasses implement their own UI generation and track output IDs
    #' for cleanup. Called by EnrichmentController$insertTab().
    #'
    #' @return Shiny UI tagList
    ui = function() {
      stop("Subclass must implement ui()")
    },

    #' Set up server logic (abstract)
    #' @param input Shiny input object
    #' @param output Shiny output object
    #' @param session Shiny session object
    server = function(input, output, session) {
      stop("Subclass must implement server()")
    },

    #' Render results tables (abstract)
    #'
    #' Each enrichment paradigm (ORA, GSEA, etc.) implements its own version.
    #' ORA renders tables with P-value, Positive Hits columns.
    #' GSEA will render tables with NES, Leading Edge columns.
    #'
    #' @param output Shiny output object to render tables into
    renderResultsTables = function(output) {
      stop("Subclass must implement renderResultsTables()")
    },

    # === Concrete Methods ===

    #' Check if results exist
    #' @return Logical
    hasResults = function() {
      !is.null(private$.results) && nrow(private$.results) > 0
    },

    #' Get the enrichment results
    #' @return DataFrame or NULL
    getResults = function() {
      private$.results
    },

    #' Get the background size
    #' @return Integer or NULL
    getBackgroundSize = function() {
      private$.backgroundSize
    },

    #' Get the conversion table
    #' @return DataFrame or NULL
    getConversionTable = function() {
      private$.conversionTable
    },

    #' Get the background conversion table
    #' @return DataFrame or NULL
    getBackgroundConversionTable = function() {
      private$.backgroundConversionTable
    },

    #' Get the input analyte list
    #' @return AnalyteList
    getInput = function() {
      private$.input
    },

    #' Get the parameters
    #' @return List
    getParameters = function() {
      private$.parameters
    },

    #' Get Shiny input ID with session-specific prefix
    #' @param suffix Character. The component suffix (e.g., "barchart_sourceSelect")
    #' @return Full input ID (e.g., "functional_gProfiler_1_barchart_sourceSelect")
    getInputId = function(suffix) {
      paste(self$id, suffix, sep = "_")
    },

    #' Get the enrichment type (parsed from session ID)
    #' @return Character. Enrichment type (e.g., "functional")
    getEnrichmentType = function() {
      strsplit(self$id, "_")[[1]][1]
    },

    # === Arena Edgelist (temporary for Part 2, moves to NetworkOutputSession in Part 3) ===

    #' Set arena edgelist for a network type
    #' @param networkType Character. Network type (e.g., "function_gene").
    #' @param edgelist DataFrame. The edgelist data.
    setArenaEdgelist = function(networkType, edgelist) {
      private$.arenaEdgelists[[networkType]] <- edgelist
    },

    #' Get arena edgelist for a network type
    #' @param networkType Character. Network type.
    #' @return DataFrame or NULL
    getArenaEdgelist = function(networkType) {
      private$.arenaEdgelists[[networkType]]
    },

    #' Update parameters (used when only datasources changed)
    #' @param newParameters List. New parameters to set.
    updateParameters = function(newParameters) {
      private$.parameters <- newParameters
    },

    #' Clear results and state (for re-execution after parameter update)
    #' Does NOT destroy observers or remove UI - just clears data
    clearResults = function() {
      private$.results <- NULL
      private$.conversionTable <- NULL
      private$.backgroundConversionTable <- NULL
      private$.arenaEdgelists <- list()
    },

    #' Clean up observers and resources
    cleanup = function() {
      # Destroy all observers
      for (obs in private$.observers) {
        if (!is.null(obs)) {
          tryCatch(obs$destroy(), error = function(e) NULL)
        }
      }
      private$.observers <- list()

      # Clear results
      private$.results <- NULL
      private$.conversionTable <- NULL
      private$.arenaEdgelists <- list()
    },

    #' Print summary
    print = function() {
      cat(sprintf("<EnrichmentSession> %s\n", self$id))
      cat(sprintf("  Tool: %s\n", self$toolName))
      cat(sprintf("  Organism: %s\n", self$organism))
      cat(sprintf("  Has results: %s\n", self$hasResults()))
      if (self$hasResults()) {
        cat(sprintf("  Result rows: %d\n", nrow(private$.results)))
      }
      invisible(self)
    }
  ),

  private = list(
    # Input analyte list
    .input = NULL,

    # Enrichment results (owned by session, NOT global)
    .results = NULL,

    # Background size
    .backgroundSize = NULL,

    # Conversion table (gene ID mappings)
    .conversionTable = NULL,

    # Background conversion table (for background gene list)
    .backgroundConversionTable = NULL,

    # Arena edgelists per network type (temporary for Part 2)
    .arenaEdgelists = NULL,

    # Enrichment parameters
    .parameters = NULL,

    # List of observers for cleanup
    .observers = list()
  )
)
