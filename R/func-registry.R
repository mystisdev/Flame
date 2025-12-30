# ============================================================================
# Output Registry for Dynamic Shiny Outputs (Cleanup Management)
# ============================================================================
#
# Tracks all dynamically created Shiny outputs for centralized cleanup.
# Instantiate per-session in server.R, NOT in global.R
# to prevent session bleed between users.
#
# Usage:
#   1. Register outputs when tab is created: registerOutputsForRun(fullRunKey)
#   2. Clear outputs when tab is closed: outputRegistry$clearRun(fullRunKey, output)

library(R6)

OutputRegistry <- R6::R6Class("OutputRegistry",
  private = list(
    # Storage: list(runKey = list(outputId = list(id, type)))
    outputs = list()
  ),

  public = list(
    # Initialize empty registry
    initialize = function() {
      private$outputs <- list()
    },

    # Register an output for a run
    # @param fullRunKey The run context key (e.g., "functional_gProfiler_1")
    # @param outputId The Shiny output ID
    # @param outputType The type for cleanup: "plotly", "datatable", "visNetwork", "upsetjs", "text", "ui"
    registerOutput = function(fullRunKey, outputId, outputType = "general") {
      if (is.null(private$outputs[[fullRunKey]])) {
        private$outputs[[fullRunKey]] <- list()
      }
      private$outputs[[fullRunKey]][[outputId]] <- list(
        id = outputId,
        type = outputType
      )
      invisible(self)
    },

    # Register multiple outputs at once with the same type
    registerOutputs = function(fullRunKey, outputIds, outputType = "general") {
      for (id in outputIds) {
        self$registerOutput(fullRunKey, id, outputType)
      }
      invisible(self)
    },

    # Get all output IDs for a run
    getOutputs = function(fullRunKey) {
      outputs <- private$outputs[[fullRunKey]]
      if (is.null(outputs)) return(character(0))
      return(names(outputs))
    },

    # Remove a run from the registry (tracking only, doesn't touch outputs)
    # Used internally by clearRun() after clearing outputs
    # @param fullRunKey The run context key
    unregister = function(fullRunKey) {
      private$outputs[[fullRunKey]] <- NULL
      invisible(TRUE)
    },

    # Clear all registered outputs for a run (renders empty, keeps registered)
    # Used by clearRunResults() for datasource updates where tab stays open
    # Networks are hidden via shinyjs::hide() since they need explicit visibility control
    # @param fullRunKey The run context key
    # @param output The Shiny output object
    clearOutputs = function(fullRunKey, output) {
      outputIds <- self$getOutputs(fullRunKey)

      for (outputId in outputIds) {
        outputInfo <- private$outputs[[fullRunKey]][[outputId]]
        tryCatch({
          switch(outputInfo$type,
            "plotly" = { output[[outputId]] <- renderPlotly({}) },
            "datatable" = { output[[outputId]] <- DT::renderDataTable(NULL) },
            "visNetwork" = {
              output[[outputId]] <- renderVisNetwork({})
              shinyjs::hide(outputId)  # Networks must be hidden when empty
            },
            "upsetjs" = { output[[outputId]] <- upsetjs::renderUpsetjs({}) },
            "text" = { output[[outputId]] <- renderText("") },
            "ui" = { output[[outputId]] <- renderUI({}) },
            # Default: attempt NULL assignment
            { output[[outputId]] <- NULL }
          )
        }, error = function(e) {
          # Silently ignore errors for outputs that may not exist or are already cleared
        })
      }
      # Note: Does NOT unregister - outputs remain tracked for future clearing
      invisible(TRUE)
    },

    # Clear all registered outputs for a run AND remove from registry
    # Called by clearEnrichmentRun() when tab is closed permanently
    # @param fullRunKey The run context key
    # @param output The Shiny output object
    clearRun = function(fullRunKey, output) {
      self$clearOutputs(fullRunKey, output)
      self$unregister(fullRunKey)
      invisible(TRUE)
    },

    # Check if a run has registered outputs
    hasOutputs = function(fullRunKey) {
      !is.null(private$outputs[[fullRunKey]]) && length(private$outputs[[fullRunKey]]) > 0
    },

    # Get count of registered runs (for debugging)
    getRunCount = function() {
      length(private$outputs)
    },

    # List all registered run keys (for debugging)
    listRuns = function() {
      names(private$outputs)
    },

    # Get count of outputs for a specific run (for debugging)
    getOutputCount = function(fullRunKey) {
      if (is.null(private$outputs[[fullRunKey]])) return(0)
      length(private$outputs[[fullRunKey]])
    }
  )
)

# ============================================================================
# Observer Registry for Dynamic Observer Cleanup
# ============================================================================
#
# Tracks all dynamically created observers for centralized destruction.
# Observers are created per enrichment run and MUST be destroyed when tabs 
# close to prevent:
#   - Memory leaks (observers accumulate with each open/close cycle)
#   - Race conditions (orphaned observers may fire after data is cleared)
#
# Cleanup Order: Observers FIRST, then Outputs
# (Observers may reference outputs; destroy watchers before data)
#
# Usage:
#   1. Register observers when created: observerRegistry$registerObservers(fullRunKey, observers)
#   2. Destroy when tab closes: observerRegistry$clearRun(fullRunKey)

ObserverRegistry <- R6::R6Class("ObserverRegistry",
  private = list(
    # Storage: list(runKey = list(observer1, observer2, ...))
    observers = list()
  ),

  public = list(
    # Initialize empty registry
    initialize = function() {
      private$observers <- list()
    },

    # Register a single observer for a run
    # @param fullRunKey The run context key (e.g., "functional_gProfiler_1")
    # @param observer The observer handle returned by observeEvent()
    registerObserver = function(fullRunKey, observer) {
      if (is.null(observer)) return(invisible(self))

      if (is.null(private$observers[[fullRunKey]])) {
        private$observers[[fullRunKey]] <- list()
      }

      private$observers[[fullRunKey]] <- append(
        private$observers[[fullRunKey]],
        list(observer)
      )
      invisible(self)
    },

    # Register multiple observers at once (for lapply results)
    # @param fullRunKey The run context key
    # @param observerList A list of observer handles
    registerObservers = function(fullRunKey, observerList) {
      if (is.null(observerList)) return(invisible(self))

      for (obs in observerList) {
        self$registerObserver(fullRunKey, obs)
      }
      invisible(self)
    },

    # Destroy all registered observers for a run
    # Called by clearEnrichmentRun() BEFORE output cleanup
    # @param fullRunKey The run context key
    clearRun = function(fullRunKey) {
      observerList <- private$observers[[fullRunKey]]

      if (!is.null(observerList)) {
        for (observer in observerList) {
          tryCatch({
            observer$destroy()
          }, error = function(e) {
            # Silently ignore errors for observers that may already be destroyed
          })
        }
      }

      # Remove from registry
      private$observers[[fullRunKey]] <- NULL
      invisible(TRUE)
    },

    # Check if a run has registered observers
    hasObservers = function(fullRunKey) {
      !is.null(private$observers[[fullRunKey]]) && length(private$observers[[fullRunKey]]) > 0
    },

    # Get count of observers for a specific run (for debugging)
    getObserverCount = function(fullRunKey) {
      if (is.null(private$observers[[fullRunKey]])) return(0)
      length(private$observers[[fullRunKey]])
    },

    # Get total observer count across all runs (for debugging memory leaks)
    getTotalObserverCount = function() {
      total <- 0
      for (runKey in names(private$observers)) {
        total <- total + length(private$observers[[runKey]])
      }
      total
    },

    # List all registered run keys (for debugging)
    listRuns = function() {
      names(private$observers)
    }
  )
)

# Reserved key for combination tab outputs (shared across runs)
COMBINATION_REGISTRY_KEY <- "combination_shared"
