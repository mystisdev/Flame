# =============================================================================
# FLAME Input AnalyteList Classes
# =============================================================================
#
# This file contains AnalyteList classes for representing collections of
# biological analytes (genes, metabolites, etc.) and the AnalyteListRegistry
# for managing them across the session.
#
# AnalyteList classes use AnalyteType and ScoreDirection from infrastructure-config.R
#
# Note: AnalyteListRegistry coexists with userInputLists during migration.
# =============================================================================

# =============================================================================
# ANALYTELIST BASE CLASS
# =============================================================================

#' AnalyteList Base Class
#'
#' Abstract base class for all analyte list types. An AnalyteList represents a
#' named collection of biological identifiers (genes, metabolites, etc.) that
#' can be used as input for enrichment analysis.
#'
#' @field name Character. Display name for this analyte list.
#' @field analyteType Character. Type from AnalyteType enum.
#' @field createdAt POSIXct. Timestamp when list was created.
#'
AnalyteList <- R6::R6Class(
  "AnalyteList",
  public = list(
    #' @field name Display name for this analyte list
    name = NULL,

    #' @field analyteType Type of analytes in this list (from AnalyteType)
    analyteType = NULL,

    #' @field createdAt Timestamp when list was created
    createdAt = NULL,

    #' Initialize an AnalyteList
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    initialize = function(name, analyteType) {
      self$name <- name
      self$analyteType <- analyteType
      self$createdAt <- Sys.time()
    },

    #' Get all identifiers in this list
    #' @return Character vector of identifiers
    getIds = function() {
      stop("Abstract method: must be implemented by subclass")
    },

    #' Get the number of identifiers
    #' @return Integer count
    size = function() {
      length(self$getIds())
    },

    #' Create a printable representation
    #' @return Character string
    print = function() {
      cat(sprintf("<%s> '%s' (%d items)\n",
                  class(self)[1], self$name, self$size()))
      invisible(self)
    }
  ),

  private = list(
    # Maximum identifiers allowed per analyte list
    .maxIds = 3000
  )
)

# =============================================================================
# UNRANKED ANALYTELIST
# =============================================================================

#' Unranked AnalyteList Class
#'
#' Represents a simple list of identifiers without ranking or scores.
#' This is the most common type, used for basic gene lists.
#'
#' @examples
#' \dontrun{
#' genes <- UnrankedAnalyteList$new("My Genes", AnalyteType$GENE,
#'                                  c("TP53", "BRCA1", "EGFR"))
#' genes$getIds()  # c("TP53", "BRCA1", "EGFR")
#' genes$size()    # 3
#' }
#'
UnrankedAnalyteList <- R6::R6Class(
  "UnrankedAnalyteList",
  inherit = AnalyteList,

  public = list(
    #' Initialize an UnrankedAnalyteList
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    #' @param ids Character vector. The identifiers.
    #' @note Throws error if ids exceeds maximum allowed.
    initialize = function(name, analyteType, ids) {
      super$initialize(name, analyteType)
      ids <- unique(as.character(ids))

      if (length(ids) > private$.maxIds) {
        stop(sprintf("Too many items (%d). Maximum allowed is %d.",
                     length(ids), private$.maxIds))
      }

      private$.ids <- ids
    },

    #' Get all identifiers
    #' @return Character vector of identifiers
    getIds = function() {
      private$.ids
    },

    #' Convert to data frame (compatible with existing userInputLists format)
    #' @return Data frame with one column named after the list
    toDataFrame = function() {
      df <- data.frame(ids = self$getIds(), stringsAsFactors = FALSE)
      colnames(df) <- self$name
      df
    }
  ),

  private = list(
    .ids = NULL
  )
)

# =============================================================================
# RANKED ANALYTELIST (Placeholder for GSEA support)
# =============================================================================

#' Ranked AnalyteList Class
#'
#' Represents a list of identifiers with associated scores and ranks.
#' Used for GSEA and other ranked enrichment methods.
#'
#' Note: This is a placeholder implementation.
#' Full GSEA support will be added when WebGestalt GSEA is integrated.
#'
#' @examples
#' \dontrun{
#' genes <- RankedAnalyteList$new(
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
RankedAnalyteList <- R6::R6Class(
  "RankedAnalyteList",
  inherit = AnalyteList,

  public = list(
    #' @field direction Score direction from ScoreDirection enum
    direction = NULL,

    #' Initialize a RankedAnalyteList
    #' @param name Character. Display name.
    #' @param analyteType Character. Type from AnalyteType.
    #' @param ids Character vector. The identifiers.
    #' @param scores Numeric vector. Scores for each identifier.
    #' @param direction Character. From ScoreDirection enum.
    #' @note Throws error if ids exceeds maximum allowed.
    initialize = function(name, analyteType, ids, scores, direction) {
      super$initialize(name, analyteType)

      if (length(ids) != length(scores)) {
        stop("ids and scores must have the same length")
      }

      if (length(ids) > private$.maxIds) {
        stop(sprintf("Too many items (%d). Maximum allowed is %d.",
                     length(ids), private$.maxIds))
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

    #' Convert to data frame with ID, score, and rank columns
    #' @return Data frame
    toDataFrame = function() {
      data.frame(
        id = self$getIds(),
        score = self$getScores(),
        rank = self$getRanks(),
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
# ANALYTELIST REGISTRY
# =============================================================================

#' AnalyteList Registry Class
#'
#' Manages a collection of AnalyteList objects for a session. Provides methods
#' to add, retrieve, remove, and query analyte lists.
#'
#' This registry is instantiated per-session in server.R (NOT global.R).
#' It will eventually replace the current userInputLists mechanism.
#'
#' ## Reactive Integration
#'
#' The registry includes built-in reactive support via a version trigger.
#' All mutating operations (add, remove, rename, clear) automatically
#' invalidate any reactive expressions that depend on the registry.
#'
#' - Use `getNames()` for non-reactive access (in imperative code)
#' - Use `getNamesReactive()` for reactive access (in UI/reactive contexts)
#'
#' IMPORTANT: Must be instantiated within a Shiny server function (reactive context)
#' because it creates a reactiveVal internally.
#'
#' @examples
#' \dontrun{
#' # In server.R (reactive context)
#' registry <- AnalyteListRegistry$new()
#' registry$add(UnrankedAnalyteList$new("List1", AnalyteType$GENE,
#'                                      c("TP53", "BRCA1")))
#' registry$get("List1")      # Returns the analyte list
#' registry$getAll()          # Returns list of all lists
#' registry$remove("List1")   # Removes the list
#'
#' # In reactive context (auto-updates when registry changes)
#' output$listSelector <- renderUI({
#'   choices <- registry$getNamesReactive()
#'   selectInput("list", "Select:", choices = choices)
#' })
#' }
#'
AnalyteListRegistry <- R6::R6Class(
  "AnalyteListRegistry",

  public = list(
    #' Initialize an empty registry
    #' @note Must be called within a Shiny reactive context (server function)
    initialize = function() {
      private$.lists <- list()
      private$.version <- shiny::reactiveVal(0)
    },

    #' Add an analyte list to the registry
    #' @param analyteList An AnalyteList object
    #' @return Invisible self for chaining
    #' @note Throws error if registry is full.
    add = function(analyteList) {
      if (!inherits(analyteList, "AnalyteList")) {
        stop("Can only add AnalyteList objects to the registry")
      }

      if (self$isFull()) {
        stop(sprintf("Maximum number of lists (%d) reached.",
                     private$.maxLists))
      }

      if (self$exists(analyteList$name)) {
        warning(sprintf("AnalyteList '%s' already exists, replacing", analyteList$name))
      }

      private$.lists[[analyteList$name]] <- analyteList
      private$invalidate()
      invisible(self)
    },

    #' Check if registry has reached maximum capacity
    #' @return Logical. TRUE if no more lists can be added.
    isFull = function() {
      self$count() >= private$.maxLists
    },

    #' Get an analyte list by name
    #' @param name Character. The list name.
    #' @return The AnalyteList object or NULL if not found
    get = function(name) {
      private$.lists[[name]]
    },

    #' Check if an analyte list exists
    #' @param name Character. The list name.
    #' @return Logical
    exists = function(name) {
      name %in% names(private$.lists)
    },

    #' Remove an analyte list by name
    #' @param name Character. The list name.
    #' @return Invisible self for chaining
    remove = function(name) {
      private$.lists[[name]] <- NULL
      private$invalidate()
      invisible(self)
    },

    #' Rename an analyte list
    #' @param oldName Character. Current name.
    #' @param newName Character. New name.
    #' @return Invisible self for chaining
    rename = function(oldName, newName) {
      if (!self$exists(oldName)) {
        stop(sprintf("AnalyteList '%s' does not exist", oldName))
      }
      if (self$exists(newName)) {
        stop(sprintf("AnalyteList '%s' already exists", newName))
      }

      analyteList <- private$.lists[[oldName]]
      analyteList$name <- newName
      private$.lists[[newName]] <- analyteList
      private$.lists[[oldName]] <- NULL
      private$invalidate()

      invisible(self)
    },

    #' Get all analyte lists
    #' @return Named list of AnalyteList objects
    getAll = function() {
      private$.lists
    },

    #' Get all analyte list names (non-reactive)
    #' @return Character vector of names
    #' @note Use getNamesReactive() in reactive contexts for auto-updates
    getNames = function() {
      names(private$.lists)
    },

    #' Get all analyte list names (reactive)
    #'
    #' Use this in reactive contexts (renderUI, reactive, observe) to
    #' automatically re-execute when the registry changes.
    #'
    #' @return Character vector of names
    getNamesReactive = function() {
      private$.version()
      names(private$.lists)
    },

    #' Get the number of analyte lists
    #' @return Integer count
    count = function() {
      length(private$.lists)
    },

    #' Query analyte lists by type
    #' @param analyteType Character. Type from AnalyteType.
    #' @return Named list of matching AnalyteList objects
    queryByType = function(analyteType) {
      Filter(function(s) s$analyteType == analyteType, private$.lists)
    },

    #' Query analyte lists by class (UnrankedAnalyteList, RankedAnalyteList)
    #' @param className Character. Class name.
    #' @return Named list of matching AnalyteList objects
    queryByClass = function(className) {
      Filter(function(s) inherits(s, className), private$.lists)
    },

    #' Clear all analyte lists
    #' @return Invisible self for chaining
    clear = function() {
      private$.lists <- list()
      private$invalidate()
      invisible(self)
    },

    #' Print summary
    print = function() {
      cat(sprintf("<AnalyteListRegistry> (%d lists)\n", self$count()))
      if (self$count() > 0) {
        for (name in self$getNames()) {
          analyteList <- private$.lists[[name]]
          cat(sprintf("  - %s: %s (%d items)\n",
                      name, class(analyteList)[1], analyteList$size()))
        }
      }
      invisible(self)
    }
  ),

  private = list(
    # Storage for analyte lists (plain list)
    .lists = NULL,

    # Maximum lists allowed in registry
    .maxLists = 10,

    # Reactive version counter for invalidation
    .version = NULL,

    # Increment version to trigger reactive invalidation
    invalidate = function() {
      current <- shiny::isolate(private$.version())
      private$.version(current + 1)
    }
  )
)
