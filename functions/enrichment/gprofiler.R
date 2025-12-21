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

# Check if gProfiler result is valid for a given run
isGprofilerResultValid <- function(runKey = NULL) {
  # Check per-run cache first (multi-run architecture)
  if (!is.null(runKey) && !is.null(gprofilerResults[[runKey]])) {
    result <- gprofilerResults[[runKey]]
    return(!is.null(result) && length(result) > 0)
  }
  # Fallback to legacy global (backward compatibility)
  return(!is.null(gprofilerResult) && length(gprofilerResult) > 0)
}

getGprofilerBackgroundSize <- function(runKey = NULL) {
  # Check per-run cache first (multi-run architecture)
  if (!is.null(runKey) && !is.null(gprofilerResults[[runKey]])) {
    result <- gprofilerResults[[runKey]]
  } else {
    # Fallback to legacy global
    result <- gprofilerResult
  }

  if (!is.null(result) && length(result) > 0) {
    metadata <- result$meta$result_metadata
    bsizes <- lapply(names(metadata), function(i) {
      return(metadata[[i]]$domain_size)
    })
    size <- max(unlist(bsizes))
  } else {
    size <- NULL
  }
  return(size)
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

      # Store background size (still using global for now)
      if (exists("currentType_Tool")) {
        if (is.null(backgroundList)) {
          enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<-
            private$getBackgroundSize(result)
        } else {
          enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<- length(backgroundList)
        }
        # Store raw result for Manhattan plot (per-run cache)
        gprofilerResults[[currentType_Tool]] <<- result
        # Also update legacy global for backward compatibility
        gprofilerResult <<- result
      }

      # Check if valid
      if (is.null(result) || length(result) == 0) {
        return(NULL)
      }

      # Parse result
      return(private$parseResult(result))
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
