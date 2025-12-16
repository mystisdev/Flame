# Get organism-specific prefix for GeneCodis datasources
# Maps taxonomy ID to prefix (e.g., taxid 10090 -> "MMUSCULUS_")
# Prefix names are arbitrary - they just match DATASOURCES keys
getGeneCodisVariablePrefix <- function() {
  organism_taxid <- ORGANISMS[ORGANISMS$print_name == input[["functional_enrichment_organism"]], ]$taxid

  return(
    switch(
      as.character(organism_taxid),
      "9606" = "",  # Homo sapiens (human) - no prefix
      "10090" = "MMUSCULUS_",  # Mus musculus (mouse)
      "10116" = "RNORVEGICUS_",  # Rattus norvegicus (rat)
      "6239" = "CELEGANS_",  # Caenorhabditis elegans (worm)
      "7227" = "DMELANOGASTER_",  # Drosophila melanogaster (fly)
      "7955" = "DRERIO_",  # Danio rerio (zebrafish)
      "9615" = "CLFAMILIARIS_",  # Canis lupus familiaris (dog)
      "9031" = "GGALLUS_",  # Gallus gallus (chicken)
      "9913" = "BTAURUS_",  # Bos taurus (cow)
      "9823" = "SSCROFA_",  # Sus scrofa (pig) - taxid 9823
      "3702" = "ATHALIANA_",  # Arabidopsis thaliana - taxid 3702
      "39947" = "OSATIVA_",  # Oryza sativa Japonica Group - taxid 39947
      "559292" = "SCEREVISIAE_",  # Saccharomyces cerevisiae S288C - taxid 559292
      "511145" = "ECOLI_",  # Escherichia coli str. K-12 substr. MG1655 - taxid 511145
      ""  # Default - organism not supported by GeneCodis
    )
  )
}

runGeneCodis <- function(userInputList, taxid, user_reference = NULL) {
  # GeneCodis accepts multiple ID formats: gene symbols, Entrez IDs, Ensembl IDs, UniProt IDs
  # The API automatically recognizes the format, so no conversion is needed

  # Send enrichment job to GeneCodis API
  jobInfo <- sendGeneCodisRequest(userInputList, taxid, user_reference)

  if (!is.null(jobInfo$job_id)) {
    # Poll for job completion with adaptive waiting
    jobReady <- pollGeneCodisJob(jobInfo$job_id, length(userInputList))

    if (jobReady) {
      # Retrieve results for each annotation category
      genecodisResult <- retrieveGeneCodisResults(jobInfo$job_id, jobInfo$job_name, jobInfo$annotations)

      if (isGeneCodisResponseValid(genecodisResult)) {
        # Parse TSV responses into FLAME-compatible format
        genecodisParsedResult <- parseGeneCodisResult(genecodisResult)

        if (isGeneCodisResultValid(genecodisParsedResult)) {
          # Filter by user's selected data sources
          genecodisParsedResult <- filterGeneCodisByDataSources(genecodisParsedResult)

          if (nrow(genecodisParsedResult) > 0) {
            # Store results in global structure after adding -log10Pvalue and enrichment scores
            enrichmentResults[[currentType_Tool]] <<-
              transformEnrichmentResultTable(genecodisParsedResult)
          }
        }
      }
    } else {
      renderWarning("GeneCodis job timed out. Please try again with a smaller gene list.")
    }
  }

  # Always store background size for statistics display
  enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<-
    getGeneCodisBackgroundSize(user_reference)
}

sendGeneCodisRequest <- function(userInputList, taxid, user_reference = NULL) {
  url <- "https://genecodis.genyo.es/gc4/analysis"

  # Get selected data sources and map to GeneCodis annotation codes
  selectedDataSources <- input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  genecodisAnnotations <- mapDataSourcesToGeneCodisDatasets(selectedDataSources)

  # Check if mapping produced empty results
  if(length(genecodisAnnotations) == 0) {
    renderWarning("No valid GeneCodis annotations mapped from selected data sources. Check your data source selection.")
    return(list(job_id = NULL, job_name = NULL, annotations = NULL))
  }

  # Create unique job name with timestamp
  jobName <- paste0("FLAME_", format(Sys.time(), "%Y%m%d_%H%M%S"))

  # Build request body
  body <- list(
    organism = as.integer(taxid),
    inputtype = "genes",
    input = list(input = userInputList),
    annotations = I(genecodisAnnotations),  # I() prevents auto_unbox from converting single element to scalar
    stat = "hypergeom",
    scope = "annotated",
    coannotation = "no",
    inputmode = "on",
    universe = if (is.null(user_reference)) list() else user_reference,
    email = "",
    jobName = jobName,
    algorithm = "fpgrowth",
    inputSupport = 0,
    inputNames = list(input1unique = jobName),
    gc4uid = ""
  )

  # Encode JSON with auto_unbox to prevent scalars from becoming arrays
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # Send POST request with pre-encoded JSON
  tryCatch({
    response <- httr::POST(url,
                           body = json_body,
                           encode = "raw",
                           httr::content_type("application/json"),
                           httr::timeout(30))

    if (response$status_code == 200) {
      response_text <- rawToChar(httr::content(response, "raw"))

      # Check for API errors
      if (grepl("error:", response_text, ignore.case = TRUE)) {
        renderWarning(paste("GeneCodis API error:", response_text))
        return(list(job_id = NULL, job_name = NULL, annotations = NULL))
      }

      # Extract job ID
      if (grepl("jobID:", response_text)) {
        job_id <- gsub(".*jobID:\\s*", "", response_text)
        job_id <- trimws(job_id)
        return(list(job_id = job_id, job_name = jobName, annotations = genecodisAnnotations))
      } else {
        renderWarning(paste("GeneCodis response missing job ID. Response was:", substr(response_text, 1, 200)))
        return(list(job_id = NULL, job_name = NULL, annotations = NULL))
      }
    } else {
      # Print error response body for debugging
      error_body <- tryCatch({
        rawToChar(httr::content(response, "raw"))
      }, error = function(e) {
        "Could not read error response"
      })
      renderWarning(paste("GeneCodis request failed with status", response$status_code, ":", substr(error_body, 1, 200)))
      return(list(job_id = NULL, job_name = NULL, annotations = NULL))
    }
  }, error = function(e) {
    renderWarning(paste("Connection to GeneCodis failed:", e$message))
    return(list(job_id = NULL, job_name = NULL, annotations = NULL))
  })
}

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

parseGeneCodisResult <- function(responseList) {
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

      # Apply user's significance filtering
      threshold <- as.numeric(input$functional_enrichment_threshold)

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

filterGeneCodisByDataSources <- function(result) {
  selectedDataSources <- input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]

  if (is.null(selectedDataSources) || length(selectedDataSources) == 0) {
    return(result)
  }

  # Filter results to only include selected data sources
  filteredResult <- result[result$Source %in% selectedDataSources, ]

  if(nrow(filteredResult) == 0 && nrow(result) > 0) {
    renderWarning(paste0("Data source filtering removed all results. ",
                        "Results had sources: ", paste(unique(result$Source), collapse=", "),
                        " but you selected: ", paste(selectedDataSources, collapse=", ")))
  }

  return(filteredResult)
}

getGeneCodisBackgroundSize <- function(user_reference) {
  if (is.null(user_reference)) {
    # Genome-wide background - GeneCodis determines this based on organism
    # Return NULL to indicate genome-wide (FLAME will display appropriately)
    return(NULL)
  } else {
    # Custom background
    return(length(user_reference))
  }
}

# Helper function: Map FLAME data source codes to GeneCodis annotation names
mapDataSourcesToGeneCodisDatasets <- function(selectedDataSources) {
  # Get organism-specific datasources for GeneCodis
  prefix <- getGeneCodisVariablePrefix()
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
