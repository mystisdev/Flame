# Tool Registry and Strategy Classes
# Manages tool dispatch using the Strategy pattern
#
# This file follows the pattern established in registry.R:
# - Multiple related R6 classes in one file
# - Classes grouped by semantic purpose (tool management)
#
# Usage:
#   # Register a strategy (done in each tool file)
#   toolRegistry$register("functional", "gProfiler", GProfilerStrategy$new())
#
#   # Get and use a strategy
#   strategy <- toolRegistry$get("functional", "gProfiler")
#   result <- strategy$run(geneList, organism, backgroundList, params)
#
# The ToolStrategy base class defines the interface that all tool
# implementations must follow. Each tool file (gprofiler.R, enrichr.R, etc.)
# will add its own strategy class that inherits from ToolStrategy.

library(R6)

# =============================================================================
# ToolStrategy - Abstract Interface for Tool Implementations
# =============================================================================
#
# All tool strategies must implement:
#   - run(): Execute the enrichment analysis
#   - convertIDs(): Convert gene IDs to the format expected by this tool
#
# Optional overrides:
#   - getValidDatasources(): Return available datasources for this tool
#   - getDefaultMetric(): Return the default significance metric

ToolStrategy <- R6::R6Class("ToolStrategy",
  public = list(
    name = NULL,

    initialize = function(name) {
      self$name <- name
    },

    # === Core Methods (abstract - must override) ===

    # Execute the enrichment analysis
    # @param inputList Character vector of gene IDs (already converted)
    # @param organism Organism taxid or identifier
    # @param backgroundList Optional character vector of background genes
    # @param params List of additional parameters (threshold, datasources, etc.)
    # @return Data frame of enrichment results, or NULL if no results
    run = function(inputList, organism, backgroundList, params) {
      stop("Abstract: implement run() in subclass")
    },

    # Convert gene IDs to the format expected by this tool
    # @param geneList Character vector of gene IDs
    # @param organism Organism taxid or identifier
    # @param targetNamespace Target namespace (e.g., "ENTREZGENE_ACC", "ENSG")
    # @return Character vector of converted IDs
    convertIDs = function(geneList, organism, targetNamespace) {
      stop("Abstract: implement convertIDs() in subclass")
    },

    # === Optional Methods (can override) ===

    # Get valid datasources for this tool and organism
    # @param organism Organism taxid or identifier
    # @return Character vector of datasource names (e.g., c("GO:BP", "KEGG"))
    getValidDatasources = function(organism) {
      return(character(0))
    },

    # Get the default significance metric for this tool
    # @param hasBackground Whether a custom background is being used
    # @return String identifier for the metric (e.g., "fdr", "g_SCS")
    getDefaultMetric = function(hasBackground) {
      return("fdr")
    },

    # Validate that the tool can run with the given parameters
    # @param organism Organism to check
    # @param datasources Datasources to check
    # @return TRUE if valid, or throws an error with message
    validateParams = function(organism, datasources) {
      return(TRUE)
    }
  )
)


# =============================================================================
# ToolRegistry - Central Registry for Tool Strategies
# =============================================================================
#
# Manages registration and lookup of tool strategies.
# Organized by enrichment type (functional, literature) and tool name.
#
# Structure: strategies[[enrichmentType]][[toolName]] = ToolStrategy instance

ToolRegistry <- R6::R6Class("ToolRegistry",
  private = list(
    # Structure: strategies[[type]][[toolName]]
    strategies = list()
  ),

  public = list(
    # Register a strategy for a given type and tool
    # @param type Enrichment type (e.g., "functional", "literature")
    # @param toolName Tool name (e.g., "gProfiler", "enrichR")
    # @param strategy ToolStrategy instance
    # @return Self (for chaining)
    register = function(type, toolName, strategy) {
      if (is.null(private$strategies[[type]])) {
        private$strategies[[type]] <- list()
      }
      private$strategies[[type]][[toolName]] <- strategy
      invisible(self)
    },

    # Get a registered strategy
    # @param type Enrichment type
    # @param toolName Tool name
    # @return ToolStrategy instance
    # @throws Error if strategy not found
    get = function(type, toolName) {
      strategy <- private$strategies[[type]][[toolName]]
      if (is.null(strategy)) {
        stop(paste("No strategy registered for", toolName, "in", type))
      }
      return(strategy)
    },

    # Check if a strategy is registered
    # @param type Enrichment type
    # @param toolName Tool name
    # @return TRUE if registered, FALSE otherwise
    hasStrategy = function(type, toolName) {
      !is.null(private$strategies[[type]]) &&
        !is.null(private$strategies[[type]][[toolName]])
    },

    # List all registered tools for a type
    # @param type Enrichment type
    # @return Character vector of tool names
    listTools = function(type) {
      if (is.null(private$strategies[[type]])) return(character(0))
      return(names(private$strategies[[type]]))
    },

    # List all registered enrichment types
    # @return Character vector of type names
    listTypes = function() {
      return(names(private$strategies))
    },

    # Get count of registered strategies
    # @return Integer count
    getStrategyCount = function() {
      total <- 0
      for (type in names(private$strategies)) {
        total <- total + length(private$strategies[[type]])
      }
      return(total)
    },

    # Debug helper: print registry contents
    print = function() {
      cat("ToolRegistry contents:\n")
      for (type in names(private$strategies)) {
        cat(paste0("  ", type, ":\n"))
        for (tool in names(private$strategies[[type]])) {
          cat(paste0("    - ", tool, "\n"))
        }
      }
      invisible(self)
    }
  )
)


# =============================================================================
# Global Instance
# =============================================================================
#
# The toolRegistry is instantiated globally because:
# 1. Tool strategies are stateless (no session-specific data)
# 2. Registration happens once at startup
# 3. All sessions can share the same strategies
#
# This differs from outputRegistry/observerRegistry which are per-session
# because they track session-specific Shiny components.

toolRegistry <- ToolRegistry$new()
