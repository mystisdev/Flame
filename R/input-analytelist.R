# =============================================================================
# FLAME AnalyteList Base Class
# =============================================================================
#
# Abstract base class for all analyte list types. An AnalyteList represents a
# named collection of biological identifiers (genes, metabolites, etc.) that
# can be used as input for enrichment analysis.
#
# Dependencies:
# - infrastructure-config.R (for AnalyteType)
#
# Subclasses (in separate files):
# - input-analytelist-unranked.R (UnrankedAnalyteList)
# - input-analytelist-ranked.R (RankedAnalyteList)
#
# Registry (in separate file):
# - input-analytelist-registry.R (AnalyteListRegistry)
#
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
