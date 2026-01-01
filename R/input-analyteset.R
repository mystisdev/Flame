# =============================================================================
# FLAME Input AnalyteSet Classes
# =============================================================================
#
# This file contains AnalyteSet classes for representing collections of
# biological analytes (genes, metabolites, etc.) and the AnalyteSetRegistry
# for managing them across the session.
#
# AnalyteSet classes use AnalyteType and ScoreDirection from infrastructure-config.R
#
# Note: AnalyteSetRegistry coexists with userInputLists during migration.
# =============================================================================

# =============================================================================
# ANALYTESET BASE CLASS
# =============================================================================

#' AnalyteSet Base Class
#'
#' Abstract base class for all analyte set types. An AnalyteSet represents a
#' named collection of biological identifiers (genes, metabolites, etc.) that
#' can be used as input for enrichment analysis.
#'
#' @field name Character. Display name for this analyte set.
#' @field analyteType Character. Type from AnalyteType enum.
#' @field createdAt POSIXct. Timestamp when set was created.
#'
AnalyteSet <- R6::R6Class(
  "AnalyteSet",
  public = list(
    #' @field name Display name for this analyte set
    name = NULL,

    #' @field analyteType Type of analytes in this set (from AnalyteType)
    analyteType = NULL,

    #' @field createdAt Timestamp when set was created
    createdAt = NULL,

    #' Initialize an AnalyteSet
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    initialize = function(name, analyteType) {
      self$name <- name
      self$analyteType <- analyteType
      self$createdAt <- Sys.time()
    },

    #' Get all identifiers in this set
    #' @return Character vector of identifiers
    getIds = function() {
      stop("Abstract method: must be implemented by subclass")
    },

    #' Get the number of identifiers
    #' @return Integer count
    size = function() {
      stop("Abstract method: must be implemented by subclass")
    },

    #' Create a printable representation
    #' @return Character string
    print = function() {
      cat(sprintf("<%s> '%s' (%d items)\n",
                  class(self)[1], self$name, self$size()))
      invisible(self)
    }
  )
)

# =============================================================================
# UNRANKED ANALYTESET
# =============================================================================

#' Unranked AnalyteSet Class
#'
#' Represents a simple set of identifiers without ranking or scores.
#' This is the most common type, used for basic gene lists.
#'
#' @examples
#' \dontrun{
#' genes <- UnrankedAnalyteSet$new("My Genes", AnalyteType$GENE,
#'                                  c("TP53", "BRCA1", "EGFR"))
#' genes$getIds()  # c("TP53", "BRCA1", "EGFR")
#' genes$size()    # 3
#' }
#'
UnrankedAnalyteSet <- R6::R6Class(
  "UnrankedAnalyteSet",
  inherit = AnalyteSet,

  public = list(
    #' Initialize an UnrankedAnalyteSet
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    #' @param ids Character vector. The identifiers.
    initialize = function(name, analyteType, ids) {
      super$initialize(name, analyteType)
      private$.ids <- unique(as.character(ids))
    },

    #' Get all identifiers
    #' @return Character vector of identifiers
    getIds = function() {
      private$.ids
    },

    #' Get the number of identifiers
    #' @return Integer count
    size = function() {
      length(private$.ids)
    },

    #' Convert to data frame (compatible with existing userInputLists format)
    #' @return Data frame with one column named after the set
    toDataFrame = function() {
      df <- data.frame(ids = private$.ids, stringsAsFactors = FALSE)
      colnames(df) <- self$name
      df
    }
  ),

  private = list(
    .ids = NULL
  )
)

# =============================================================================
# RANKED ANALYTESET (Placeholder for GSEA support)
# =============================================================================

#' Ranked AnalyteSet Class
#'
#' Represents a set of identifiers with associated scores and ranks.
#' Used for GSEA and other ranked enrichment methods.
#'
#' Note: This is a placeholder implementation.
#' Full GSEA support will be added when WebGestalt GSEA is integrated.
#'
#' @examples
#' \dontrun{
#' genes <- RankedAnalyteSet$new(
#'   "DE Genes",
#'   AnalyteType$GENE,
#'   ids = c("TP53", "BRCA1", "EGFR"),
#'   scores = c(2.5, -1.8, 0.5),
#'   direction = ScoreDirection$DESCENDING
#' )
#' genes$getIds()     # c("TP53", "BRCA1", "EGFR")
#' genes$getScores()  # c(2.5, -1.8, 0.5)
#' genes$getRanks()   # c(1, 3, 2) - auto-computed from scores
#' }
#'
RankedAnalyteSet <- R6::R6Class(
  "RankedAnalyteSet",
  inherit = AnalyteSet,

  public = list(
    #' @field direction Score direction from ScoreDirection enum
    direction = NULL,

    #' Initialize a RankedAnalyteSet
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    #' @param ids Character vector. The identifiers.
    #' @param scores Numeric vector. Scores for each identifier.
    #' @param direction Character. From ScoreDirection enum.
    initialize = function(name, analyteType, ids, scores, direction) {
      super$initialize(name, analyteType)

      if (length(ids) != length(scores)) {
        stop("ids and scores must have the same length")
      }

      private$.ids <- as.character(ids)
      private$.scores <- as.numeric(scores)
      self$direction <- direction

      private$computeRanks()
    },

    #' Get all identifiers
    #' @return Character vector of identifiers
    getIds = function() {
      private$.ids
    },

    #' Get scores for all identifiers
    #' @return Numeric vector of scores
    getScores = function() {
      private$.scores
    },

    #' Get ranks for all identifiers
    #' @return Integer vector of ranks
    getRanks = function() {
      private$.ranks
    },

    #' Get the number of identifiers
    #' @return Integer count
    size = function() {
      length(private$.ids)
    },

    #' Convert to data frame with ID, score, and rank columns
    #' @return Data frame
    toDataFrame = function() {
      data.frame(
        id = private$.ids,
        score = private$.scores,
        rank = private$.ranks,
        stringsAsFactors = FALSE
      )
    }
  ),

  private = list(
    .ids = NULL,
    .scores = NULL,
    .ranks = NULL,

    computeRanks = function() {
      if (self$direction == "DESCENDING") {
        private$.ranks <- rank(-private$.scores, ties.method = "first")
      } else {
        private$.ranks <- rank(private$.scores, ties.method = "first")
      }
    }
  )
)

# =============================================================================
# ANALYTESET REGISTRY
# =============================================================================

#' AnalyteSet Registry Class
#'
#' Manages a collection of AnalyteSet objects for a session. Provides methods
#' to add, retrieve, remove, and query analyte sets.
#'
#' This registry is instantiated per-session and stored in sessionEnv.
#' It will eventually replace the current userInputLists mechanism.
#'
#' @examples
#' \dontrun{
#' registry <- AnalyteSetRegistry$new()
#' registry$add(UnrankedAnalyteSet$new("List1", AnalyteType$GENE,
#'                                      c("TP53", "BRCA1")))
#' registry$get("List1")      # Returns the analyte set
#' registry$getAll()          # Returns list of all sets
#' registry$remove("List1")   # Removes the set
#' }
#'
AnalyteSetRegistry <- R6::R6Class(
  "AnalyteSetRegistry",

  public = list(
    #' Initialize an empty registry
    initialize = function() {
      private$analyteSets <- list()
    },

    #' Add an analyte set to the registry
    #' @param analyteSet An AnalyteSet object
    #' @return Invisible self for chaining
    add = function(analyteSet) {
      if (!inherits(analyteSet, "AnalyteSet")) {
        stop("Can only add AnalyteSet objects to the registry")
      }

      if (self$exists(analyteSet$name)) {
        warning(sprintf("AnalyteSet '%s' already exists, replacing", analyteSet$name))
      }

      private$analyteSets[[analyteSet$name]] <- analyteSet
      invisible(self)
    },

    #' Get an analyte set by name
    #' @param name Character. The set name.
    #' @return The AnalyteSet object or NULL if not found
    get = function(name) {
      private$analyteSets[[name]]
    },

    #' Check if an analyte set exists
    #' @param name Character. The set name.
    #' @return Logical
    exists = function(name) {
      name %in% names(private$analyteSets)
    },

    #' Remove an analyte set by name
    #' @param name Character. The set name.
    #' @return Invisible self for chaining
    remove = function(name) {
      private$analyteSets[[name]] <- NULL
      invisible(self)
    },

    #' Rename an analyte set
    #' @param oldName Character. Current name.
    #' @param newName Character. New name.
    #' @return Invisible self for chaining
    rename = function(oldName, newName) {
      if (!self$exists(oldName)) {
        stop(sprintf("AnalyteSet '%s' does not exist", oldName))
      }
      if (self$exists(newName)) {
        stop(sprintf("AnalyteSet '%s' already exists", newName))
      }

      analyteSet <- private$analyteSets[[oldName]]
      analyteSet$name <- newName
      private$analyteSets[[newName]] <- analyteSet
      private$analyteSets[[oldName]] <- NULL

      invisible(self)
    },

    #' Get all analyte sets
    #' @return Named list of AnalyteSet objects
    getAll = function() {
      private$analyteSets
    },

    #' Get all analyte set names
    #' @return Character vector of names
    getNames = function() {
      names(private$analyteSets)
    },

    #' Get the number of analyte sets
    #' @return Integer count
    count = function() {
      length(private$analyteSets)
    },

    #' Query analyte sets by type
    #' @param analyteType Character. Type from AnalyteType.
    #' @return Named list of matching AnalyteSet objects
    queryByType = function(analyteType) {
      Filter(function(s) s$analyteType == analyteType, private$analyteSets)
    },

    #' Query analyte sets by class (UnrankedAnalyteSet, RankedAnalyteSet)
    #' @param className Character. Class name.
    #' @return Named list of matching AnalyteSet objects
    queryByClass = function(className) {
      Filter(function(s) inherits(s, className), private$analyteSets)
    },

    #' Clear all analyte sets
    #' @return Invisible self for chaining
    clear = function() {
      private$analyteSets <- list()
      invisible(self)
    },

    #' Print summary
    print = function() {
      cat(sprintf("<AnalyteSetRegistry> (%d sets)\n", self$count()))
      if (self$count() > 0) {
        for (name in self$getNames()) {
          analyteSet <- private$analyteSets[[name]]
          cat(sprintf("  - %s: %s (%d items)\n",
                      name, class(analyteSet)[1], analyteSet$size()))
        }
      }
      invisible(self)
    }
  ),

  private = list(
    analyteSets = NULL
  )
)
