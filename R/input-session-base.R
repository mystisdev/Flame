# =============================================================================
# FLAME Input Session Base
# =============================================================================
#
# This file contains the abstract InputSession base class.
# All concrete InputSession classes inherit from this base.
#
# Dependencies:
# - AnalyteListRegistry (from input-analytelist.R)
# - AnalyteType, UnrankedAnalyteList (from input-analytelist.R)
#
# =============================================================================

# =============================================================================
# ABSTRACT INPUT SESSION BASE CLASS
# =============================================================================

#' Abstract Input Session Base Class
#'
#' Base class for all input session types. Provides common functionality for:
#' - Registry access and analyte list creation
#' - Observer lifecycle management
#' - Unique name generation
#'
#' Subclasses must implement:
#' - server(parentInput, parentSession)
#'
#' @section Common Methods:
#' - addAnalyteList(name, ids, analyteType): Create and register an analyte list
#' - generateUniqueName(prefix): Generate a unique name with given prefix
#' - cleanup(): Destroy all registered observers
#'
InputSession <- R6::R6Class(
  "InputSession",

  public = list(
    #' @field id Module namespace ID
    id = NULL,

    #' Initialize an InputSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      if (!inherits(registry, "AnalyteListRegistry")) {
        stop("registry must be an AnalyteListRegistry")
      }
      self$id <- id
      private$.registry <- registry
      private$.observers <- list()
    },

    #' Initialize server logic (abstract - must be implemented by subclasses)
    #' @param parentInput Parent session's input object
    #' @param parentSession Parent session object
    server = function(parentInput, parentSession) {
      stop("Subclass must implement server()")
    },

    #' Clean up observers and resources
    cleanup = function() {
      for (obs in private$.observers) {
        if (!is.null(obs)) {
          obs$destroy()
        }
      }
      private$.observers <- list()
    },

    #' Add an analyte list to the registry
    #' @param name Character. Name for the analyte list.
    #' @param ids Character vector. The analyte IDs.
    #' @param analyteType Character. Type from AnalyteType (default: GENE).
    #' @return Character. The name of the added list.
    addAnalyteList = function(name, ids, analyteType = AnalyteType$GENE) {
      analyteList <- UnrankedAnalyteList$new(
        name = name,
        analyteType = analyteType,
        ids = ids
      )
      private$.registry$add(analyteList)
      return(name)
    },

    #' Generate a unique name with the given prefix
    #' @param prefix Character. Prefix for the name.
    #' @return Character. Unique name in format "prefix_XXXXXX".
    generateUniqueName = function(prefix) {
      existingNames <- private$.registry$getNames()
      repeat {
        suffix <- paste(sample(c(0:9, letters, LETTERS), 6), collapse = "")
        name <- paste(prefix, suffix, sep = "_")
        if (!name %in% existingNames) {
          break
        }
      }
      return(name)
    },

    #' Get the registry
    #' @return AnalyteListRegistry
    getRegistry = function() {
      private$.registry
    }
  ),

  private = list(
    # Reference to the AnalyteListRegistry
    .registry = NULL,

    # List of observers for cleanup
    .observers = list(),

    # Parent Shiny session
    .parentSession = NULL,

    # Module Shiny session
    .moduleSession = NULL,

    # Maximum input object size in bytes (1MB)
    .objectSizeLimit = 1000000,

    #' Check if object size exceeds limit
    #' @param inputDF Data frame to check.
    #' @return Logical. TRUE if too large.
    isInvalidObjectSize = function(inputDF) {
      if (object.size(inputDF) > private$.objectSizeLimit) {
        renderWarning(paste0("Make sure your input is <",
                             private$.objectSizeLimit, " bytes."))
        return(TRUE)
      }
      return(FALSE)
    }
  )
)
