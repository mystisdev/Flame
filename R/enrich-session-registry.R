# =============================================================================
# FLAME Enrichment Session Registry
# =============================================================================
#
# Manages a collection of EnrichmentSession objects for a user session.
# Provides reactive pattern for combination tab auto-updates.
#
# This registry is instantiated per-session in server.R (NOT global.R).
#
# Dependencies:
# - enrich-session-base.R (for EnrichmentSession base class)
#
# =============================================================================

#' Enrichment Session Registry Class
#'
#' Manages a collection of EnrichmentSession objects for a user session.
#' Uses reactive pattern (like AnalyteListRegistry) for auto-updates.
#'
#' The combination tab observes this registry and auto-updates when
#' sessions are added or removed.
#'
#' IMPORTANT: Must be instantiated within a Shiny server function (reactive context)
#' because it creates a reactiveVal internally.
#'
EnrichmentSessionRegistry <- R6::R6Class(
  "EnrichmentSessionRegistry",

  public = list(
    #' Initialize an empty registry
    #' @note Must be called within a Shiny reactive context (server function)
    initialize = function() {
      private$.sessions <- list()
      private$.version <- shiny::reactiveVal(0)
      private$.idCounter <- 0
      private$.displayCounters <- list()
    },

    #' Add an enrichment session to the registry
    #' @param session An EnrichmentSession object
    #' @return Invisible self for chaining
    add = function(session) {
      if (!inherits(session, "EnrichmentSession")) {
        stop("Can only add EnrichmentSession objects to the registry")
      }

      private$.sessions[[session$id]] <- session
      private$invalidate()
      invisible(self)
    },

    #' Get a session by ID
    #' @param id Character. The session ID.
    #' @return The EnrichmentSession object or NULL if not found
    get = function(id) {
      private$.sessions[[id]]
    },

    #' Check if a session exists
    #' @param id Character. The session ID.
    #' @return Logical
    exists = function(id) {
      id %in% names(private$.sessions)
    },

    #' Remove a session by ID
    #' @param id Character. The session ID.
    #' @return Invisible self for chaining
    remove = function(id) {
      private$.sessions[[id]] <- NULL
      private$invalidate()
      invisible(self)
    },

    #' Get all sessions (non-reactive)
    #' @return Named list of EnrichmentSession objects
    getAll = function() {
      private$.sessions
    },

    #' Get all sessions (reactive)
    #' Use this in reactive contexts to auto-update when registry changes.
    #' @return Named list of EnrichmentSession objects
    getAllReactive = function() {
      private$.version()
      private$.sessions
    },

    #' Get the number of sessions
    #' @return Integer count
    count = function() {
      length(private$.sessions)
    },

    #' Query sessions by tool name
    #' @param toolName Character. Tool name (e.g., "gProfiler", "STRING").
    #' @return Named list of matching EnrichmentSession objects
    queryByTool = function(toolName) {
      Filter(function(s) s$toolName == toolName, private$.sessions)
    },

    #' Count sessions for a specific tool
    #' @param toolName Character. Tool name.
    #' @return Integer count
    countByTool = function(toolName) {
      length(self$queryByTool(toolName))
    },

    #' Get the next unique ID (increments counter)
    #' @return Integer. The next unique ID value.
    getNextUniqueId = function() {
      private$.idCounter <- private$.idCounter + 1
      private$.idCounter
    },

    #' Generate a unique session ID
    #' @param toolName Character. Tool name for the ID.
    #' @return Character. Unique ID in format "functional_{toolName}_{uniqueId}"
    generateId = function(toolName) {
      paste0("functional_", toolName, "_", self$getNextUniqueId())
    },

    #' Generate a display number for a tool
    #' Display numbers can reset (for user-facing tab titles).
    #' @param toolName Character. Tool name.
    #' @return Integer. Display number for this run.
    generateDisplayNumber = function(toolName) {
      if (is.null(private$.displayCounters[[toolName]])) {
        private$.displayCounters[[toolName]] <- 0
      }
      private$.displayCounters[[toolName]] <- private$.displayCounters[[toolName]] + 1
      private$.displayCounters[[toolName]]
    },

    #' Reset display counter for a tool (called when all tabs for that tool are closed)
    #' @param toolName Character. Tool name.
    resetDisplayCounter = function(toolName) {
      private$.displayCounters[[toolName]] <- 0
    },

    #' Clear all sessions
    #' @return Invisible self for chaining
    clear = function() {
      # Call cleanup on each session before clearing
      for (session in private$.sessions) {
        if (!is.null(session) && "cleanup" %in% names(session)) {
          session$cleanup()
        }
      }
      private$.sessions <- list()
      private$.displayCounters <- list()
      private$invalidate()
      invisible(self)
    },

    #' Print summary
    print = function() {
      cat(sprintf("<EnrichmentSessionRegistry> (%d sessions)\n", self$count()))
      if (self$count() > 0) {
        for (id in names(private$.sessions)) {
          session <- private$.sessions[[id]]
          cat(sprintf("  - %s: %s (display #%d)\n",
                      id, session$toolName, session$displayNumber))
        }
      }
      invisible(self)
    }
  ),

  private = list(
    # Storage for sessions (plain list)
    .sessions = NULL,

    # Reactive version counter for invalidation
    .version = NULL,

    # Unique ID counter (never resets - prevents Shiny caching bugs)
    .idCounter = NULL,

    # Display counters per tool (can reset on Clear All)
    .displayCounters = NULL,

    # Increment version to trigger reactive invalidation
    invalidate = function() {
      current <- shiny::isolate(private$.version())
      private$.version(current + 1)
    }
  )
)
