mapGProfilerIds <- function(gprofilerParsedResult) {
  gprofilerParsedResult <- mapKEGGIds(gprofilerParsedResult)
  gprofilerParsedResult <- mapREACIds(gprofilerParsedResult)
  gprofilerParsedResult <- mapWPIds(gprofilerParsedResult)
  return(gprofilerParsedResult)
}

mapREACIds <- function(df) {
  if (length(df$Source[which(df$Source == "REAC")]) > 0) {
    df[df$Source == "REAC", ]$Term_ID <-
      gsub("REAC:", "", df[df$Source == "REAC", ]$Term_ID)
  }
  return(df)
}

mapWPIds <- function(df) {
  if (length(df$Source[which(df$Source == "WP")]) > 0) {
    df[df$Source == "WP", ]$Term_ID <-
      gsub("WP:", "", df[df$Source == "WP", ]$Term_ID)
  }
  return(df)
}

# =============================================================================
# GProfilerStrategy - Tool Strategy Implementation
# =============================================================================

GProfilerStrategy <- R6::R6Class("GProfilerStrategy",

  inherit = ToolStrategy,

  public = list(
    initialize = function() {
      super$initialize("gProfiler")
    },

    run = function(inputList, organism, backgroundList, params) {
      # Filter datasources to only those supported by gProfiler
      sources <- DATASOURCES[["GPROFILER"]][
        DATASOURCES[["GPROFILER"]] %in% params$datasources
      ]

      if (identical(sources, character(0))) {
        return(NULL)
      }

      # Determine background mode
      if (is.null(backgroundList)) {
        domain_scope <- "annotated"
        custom_bg <- NULL
        significant <- TRUE
      } else {
        domain_scope <- "custom"
        custom_bg <- backgroundList
        significant <- FALSE
      }

      # Get organism short name
      organismName <- ORGANISMS[ORGANISMS$taxid == organism, ]$short_name

      # Call gProfiler API
      result <- gprofiler2::gost(
        query = inputList,
        organism = organismName,
        significant = significant,
        evcodes = TRUE,
        user_threshold = as.numeric(params$threshold),
        correction_method = params$metric,
        sources = sources,
        domain_scope = domain_scope,
        custom_bg = custom_bg
      )

      # Check if valid
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }

      # Calculate background size
      backgroundSize <- if (is.null(backgroundList)) {
        private$getBackgroundSize(result)
      } else {
        length(backgroundList)
      }

      # Return structured result (no global writes)
      return(list(
        result = private$parseResult(result),
        backgroundSize = backgroundSize
      ))
    },

    # gProfiler uses its own ID conversion
    convertIDs = function(geneList, organism, targetNamespace) {
      # gProfiler accepts most ID formats directly
      return(geneList)
    },

    getValidDatasources = function(organism) {
      return(DATASOURCES[["GPROFILER"]])
    },

    getDefaultMetric = function(hasBackground) {
      if (hasBackground) "fdr" else "g_SCS"
    }
  ),

  private = list(
    parseResult = function(result) {
      parsed <- result$result[, c(
        "source", "term_id", "term_name", "p_value", "term_size",
        "query_size", "intersection_size", "intersection"
      )]
      colnames(parsed) <- ENRICHMENT_DF_COLNAMES
      parsed <- mapGProfilerIds(parsed)
      return(parsed)
    },

    getBackgroundSize = function(result) {
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }
      metadata <- result$meta$result_metadata
      bsizes <- lapply(names(metadata), function(i) {
        return(metadata[[i]]$domain_size)
      })
      return(max(unlist(bsizes)))
    }
  )
)

# Register the strategy
toolRegistry$register("functional", "gProfiler", GProfilerStrategy$new())
