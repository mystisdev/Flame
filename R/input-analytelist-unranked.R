# =============================================================================
# FLAME Unranked AnalyteList Class
# =============================================================================
#
# Represents a simple list of identifiers without ranking or scores.
# This is the most common type, used for basic gene lists.
#
# Dependencies:
# - input-analytelist.R (for AnalyteList base class)
#
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
