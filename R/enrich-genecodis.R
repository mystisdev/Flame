# Helper functions used by GeneCodisStrategy

pollGeneCodisJob <- function(job_id, gene_count, max_timeout = 60) {
  # Adaptive initial wait based on gene list size
  initial_wait <- if (gene_count < 20) {
    8
  } else if (gene_count < 100) {
    12
  } else {
    18
  }

  Sys.sleep(initial_wait)

  # Poll for completion with 2-second intervals
  qc_url <- sprintf("https://genecodis.genyo.es/gc4/qc?job=%s", job_id)
  start_time <- Sys.time()
  poll_interval <- 2

  while (difftime(Sys.time(), start_time, units = "secs") < max_timeout) {
    tryCatch({
      qc_response <- httr::GET(qc_url, httr::timeout(10))

      if (qc_response$status_code == 200) {
        # Job is ready
        return(TRUE)
      } else if (qc_response$status_code >= 400) {
        # HTTP error codes indicate permanent failures
        stop("GeneCodis encountered an error processing your request. Please verify your gene list contains valid identifiers (Entrez IDs, gene symbols, Ensembl IDs, or UniProt IDs).")
      }

      # Job not ready yet, wait and retry
      Sys.sleep(poll_interval)

    }, error = function(e) {
      # Check if this is our intentional error (invalid genes) or a real network error
      if (grepl("Please verify your gene list", e$message)) {
        # Re-throw our intentional error - don't retry
        stop(e$message)
      } else {
        # Real network error, wait and retry
        Sys.sleep(poll_interval)
      }
    })
  }

  # Timeout reached
  return(FALSE)
}

retrieveGeneCodisResults <- function(job_id, job_name, annotations) {
  allResults <- list()

  for (annotation in annotations) {
    # Construct result URL with job_name-annotation format
    annotation_key <- paste0(job_name, "-", annotation)
    url <- sprintf("https://genecodis.genyo.es/gc4/results?job=%s&annotation=%s",
                   job_id, annotation_key)

    # Retrieve results with error handling
    tryCatch({
      response <- httr::GET(url, httr::timeout(30))

      if (response$status_code == 200) {
        allResults[[annotation]] <- response
      } else {
        # Non-200 status might mean no results for this annotation (not an error)
        allResults[[annotation]] <- NULL
      }
    }, error = function(e) {
      renderWarning(paste("Failed to retrieve GeneCodis results for", annotation, ":", e$message))
      allResults[[annotation]] <- NULL
    })
  }

  return(allResults)
}

isGeneCodisResponseValid <- function(responseList) {
  # Check if we have at least one valid response
  valid_responses <- sum(sapply(responseList, function(r) !is.null(r)))

  if (valid_responses == 0) {
    renderWarning("No valid results returned from GeneCodis. Try adjusting your gene list or selected data sources.")
    return(FALSE)
  }

  return(TRUE)
}

parseGeneCodisResult <- function(responseList, threshold = NULL) {
  # Accept threshold parameter instead of reading global input
  # Parse TSV responses from GeneCodis API and combine results
  allResults <- data.frame()

  for (annotation in names(responseList)) {
    response <- responseList[[annotation]]

    if (is.null(response)) {
      next
    }

    responseBody <- rawToChar(httr::content(response, "raw"))

    # Skip if empty response
    if (nchar(responseBody) == 0) {
      next
    }

    # Check for error messages
    if (grepl("error:", responseBody, ignore.case = TRUE)) {
      next  # Skip this annotation
    }

    # Parse TSV with error handling
    tryCatch({
      results <- read.delim(text = responseBody, header = TRUE,
                           stringsAsFactors = FALSE, sep = "\t")

      if (nrow(results) == 0) {
        next
      }

      # Map GeneCodis annotation name to FLAME source code
      results$flame_source <- mapGeneCodisToFlameSource(annotation)

      # Apply user's significance filtering using passed parameter
      threshold <- as.numeric(threshold)

      # GeneCodis returns pval_adj (FDR-adjusted p-value)
      if ("pval_adj" %in% colnames(results)) {
        significant_mask <- results$pval_adj < threshold
        results <- results[significant_mask, ]
      }

      if (nrow(results) == 0) {
        next
      }

      # Select and rename columns to FLAME format
      # GeneCodis TSV columns:
      # description, annotation_id, genes_found, input_size, term_genes,
      # universe, pval, pval_adj, relative_enrichment, annotsbias, genes

      genecodisSelected <- data.frame(
        Source = trimws(results$flame_source),  # Remove whitespace for clean matching
        Term_ID = results$annotation_id,
        Function = results$description,
        Pvalue = results$pval_adj,  # Use adjusted p-value
        Term_Size = results$term_genes,
        Query_size = results$input_size,
        Intersection_Size = results$genes_found,
        Positive_Hits = results$genes,
        stringsAsFactors = FALSE
      )

      # Rename columns to FLAME's standard names
      colnames(genecodisSelected) <- ENRICHMENT_DF_COLNAMES

      allResults <- rbind(allResults, genecodisSelected)

    }, error = function(e) {
      renderWarning(paste("Failed to parse GeneCodis response for", annotation, ":", e$message))
    })
  }

  return(allResults)
}

isGeneCodisResultValid <- function(parsedResult) {
  if (is.null(parsedResult)) {
    renderWarning("No significant enrichment results found in GeneCodis analysis")
    return(FALSE)
  }

  if (!is.data.frame(parsedResult)) {
    renderWarning("No significant enrichment results found in GeneCodis analysis")
    return(FALSE)
  }

  if (nrow(parsedResult) == 0) {
    renderWarning("No significant enrichment results found in GeneCodis analysis")
    return(FALSE)
  }

  return(TRUE)
}

# Helper function: Map FLAME data source codes to GeneCodis annotation names
# @param selectedDataSources Character vector of datasource codes
# @param organism Organism taxid (numeric or character)
mapDataSourcesToGeneCodisDatasets <- function(selectedDataSources, organism) {
  # Get organism-specific prefix using same logic as GeneCodisStrategy
  prefix <- switch(as.character(organism),
    "9606" = "",
    "10090" = "MMUSCULUS_",
    "10116" = "RNORVEGICUS_",
    "6239" = "CELEGANS_",
    "7227" = "DMELANOGASTER_",
    "7955" = "DRERIO_",
    "9615" = "CLFAMILIARIS_",
    "9031" = "GGALLUS_",
    "9913" = "BTAURUS_",
    "9823" = "SSCROFA_",
    "3702" = "ATHALIANA_",
    "39947" = "OSATIVA_",
    "559292" = "SCEREVISIAE_",
    "511145" = "ECOLI_",
    ""
  )
  organismDataSources <- DATASOURCES[[paste0(prefix, "GENECODIS")]]

  # Filter selected datasources to only those GeneCodis supports for this organism
  # This handles multi-tool selection where other tools may support additional datasources
  selectedDataSources <- selectedDataSources[selectedDataSources %in% organismDataSources]

  # Use DATASOURCES_CODES[["GENECODIS"]] for API code mapping
  # (same API codes across all organisms)
  datasetMapping <- DATASOURCES_CODES[["GENECODIS"]]

  genecodisDatasets <- c()
  for (source in selectedDataSources) {
    if (source %in% names(datasetMapping)) {
      mapped <- datasetMapping[[source]]
      genecodisDatasets <- c(genecodisDatasets, mapped)
    }
  }

  return(genecodisDatasets)
}

# Helper function: Map GeneCodis annotation names back to FLAME source codes
mapGeneCodisToFlameSource <- function(annotation) {
  mapping <- list(
    "GO_MF" = "GO:MF",
    "GO_BP" = "GO:BP",
    "GO_CC" = "GO:CC",
    "KEGG" = "KEGG",
    "Reactome" = "REAC",
    "WikiPathways" = "WP",
    "Panther" = "PANTHER Pathways",
    "HPO" = "HP",
    "OMIM" = "OMIM",
    "MGI" = "MGI",
    "BioPlanet" = "BioPlanet",
    "PharmGKB" = "PharmGKB",
    "LINCS" = "LINCS",
    "CollecTRI" = "CollecTRI",
    "miRTarBase" = "MIRNA"
  )

  if (annotation %in% names(mapping)) {
    return(mapping[[annotation]])
  }

  # Return original if no mapping found
  return(annotation)
}


# =============================================================================
# GeneCodisStrategy - Tool Strategy Implementation
# =============================================================================

GeneCodisStrategy <- R6::R6Class("GeneCodisStrategy",

  inherit = ToolStrategy,

  public = list(
    initialize = function() {
      super$initialize("GeneCodis")
    },

    run = function(inputList, organism, backgroundList, params) {
      # Map datasources to GeneCodis annotations
      genecodisAnnotations <- mapDataSourcesToGeneCodisDatasets(params$datasources, organism)

      if (length(genecodisAnnotations) == 0) {
        renderWarning("No valid GeneCodis annotations for selected data sources.")
        return(NULL)
      }

      # Send job request
      jobInfo <- private$sendRequest(inputList, organism, backgroundList, genecodisAnnotations)

      if (is.null(jobInfo$job_id)) {
        return(NULL)
      }

      # Poll for completion
      jobReady <- pollGeneCodisJob(jobInfo$job_id, length(inputList))

      if (!jobReady) {
        renderWarning("GeneCodis job timed out.")
        return(NULL)
      }

      # Retrieve results
      genecodisResult <- retrieveGeneCodisResults(
        jobInfo$job_id, jobInfo$job_name, jobInfo$annotations
      )

      if (!isGeneCodisResponseValid(genecodisResult)) {
        return(NULL)
      }

      # Parse results - pass threshold from params
      result <- parseGeneCodisResult(genecodisResult, params$threshold)

      if (!isGeneCodisResultValid(result)) {
        return(NULL)
      }

      # Filter by datasources using params (no global dependency)
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        filteredResult <- result[result$Source %in% params$datasources, ]
        if (nrow(filteredResult) == 0 && nrow(result) > 0) {
          renderWarning(paste0(
            "Data source filtering removed all results. ",
            "Results had sources: ", paste(unique(result$Source), collapse = ", "),
            " but you selected: ", paste(params$datasources, collapse = ", ")
          ))
        }
        result <- filteredResult
      }

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Return structured result (no global writes)
      return(list(
        result = result,
        backgroundSize = getSimpleBackgroundSize(backgroundList),
        rawResult = NULL
      ))
    },

    convertIDs = function(geneList, organism, targetNamespace) {
      # GeneCodis accepts multiple ID formats
      return(geneList)
    },

    getValidDatasources = function(organism) {
      prefix <- private$getVariablePrefix(organism)
      return(names(DATASOURCES_CODES[[paste0(prefix, "GENECODIS")]]))
    },

    getDefaultMetric = function(hasBackground) {
      return("fdr")
    }
  ),

  private = list(
    getVariablePrefix = function(organism) {
      switch(as.character(organism),
        "9606" = "",
        "10090" = "MMUSCULUS_",
        "10116" = "RNORVEGICUS_",
        "6239" = "CELEGANS_",
        "7227" = "DMELANOGASTER_",
        "7955" = "DRERIO_",
        "9615" = "CLFAMILIARIS_",
        "9031" = "GGALLUS_",
        "9913" = "BTAURUS_",
        "9823" = "SSCROFA_",
        "3702" = "ATHALIANA_",
        "39947" = "OSATIVA_",
        "559292" = "SCEREVISIAE_",
        "511145" = "ECOLI_",
        ""
      )
    },

    sendRequest = function(inputList, taxid, backgroundList, annotations) {
      url <- "https://genecodis.genyo.es/gc4/analysis"
      jobName <- paste0("FLAME_", format(Sys.time(), "%Y%m%d_%H%M%S"))

      body <- list(
        organism = as.integer(taxid),
        inputtype = "genes",
        input = list(input = inputList),
        annotations = I(annotations),
        stat = "hypergeom",
        scope = "annotated",
        coannotation = "no",
        inputmode = "on",
        universe = if (is.null(backgroundList)) list() else backgroundList,
        email = "",
        jobName = jobName,
        algorithm = "fpgrowth",
        inputSupport = 0,
        inputNames = list(input1unique = jobName),
        gc4uid = ""
      )

      json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

      tryCatch({
        response <- httr::POST(url,
          body = json_body,
          encode = "raw",
          httr::content_type("application/json"),
          httr::timeout(30)
        )

        if (response$status_code == 200) {
          response_text <- rawToChar(httr::content(response, "raw"))

          if (grepl("error:", response_text, ignore.case = TRUE)) {
            renderWarning(paste("GeneCodis API error:", response_text))
            return(list(job_id = NULL, job_name = NULL, annotations = NULL))
          }

          if (grepl("jobID:", response_text)) {
            job_id <- gsub(".*jobID:\\s*", "", response_text)
            job_id <- trimws(job_id)
            return(list(job_id = job_id, job_name = jobName, annotations = annotations))
          }
        }

        return(list(job_id = NULL, job_name = NULL, annotations = NULL))
      }, error = function(e) {
        renderWarning(paste("Connection to GeneCodis failed:", e$message))
        return(list(job_id = NULL, job_name = NULL, annotations = NULL))
      })
    }
  )
)

# Register the strategy
toolRegistry$register("functional", "GeneCodis", GeneCodisStrategy$new())
