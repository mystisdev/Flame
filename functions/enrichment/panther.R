# Helper functions used by PANTHERStrategy

extractPantherGeneList <- function(input_list_column) {
  # Extract PANTHER protein IDs from PANTHER's input_list column
  #
  # input_list_column is a data.frame column where each element is a data.frame
  # with mapped_ids (gene symbols) and mapped_panther_ids (PANTHER accessions)
  #
  # We extract mapped_panther_ids (NOT mapped_ids) to maintain
  # consistency with other tools:
  # - All tools store converted IDs in Positive Hits (STRING IDs, ENSP IDs, etc.)
  # - This allows rollBackConvertedNames() to properly convert back to gene symbols
  # - This allows findNoHitGenes() to correctly identify genes without annotations

  if (is.null(input_list_column) || length(input_list_column) == 0) {
    return("")
  }

  # Determine number of items - handle both data.frame and list inputs
  # nrow() returns NULL for lists, which causes seq_len(NULL) to error
  if (is.data.frame(input_list_column)) {
    n <- nrow(input_list_column)
  } else {
    n <- length(input_list_column)
  }

  # Guard against NULL/0 - seq_len(NULL) or seq_len(0) would fail or return empty
  if (is.null(n) || n == 0) {
    return("")
  }

  # Extract mapped_panther_ids (PANTHER protein accessions) from each row
  gene_lists <- sapply(seq_len(n), function(i) {
    # Handle both data.frame row access and list element access
    if (is.data.frame(input_list_column)) {
      row_data <- input_list_column[i, ]
    } else {
      row_data <- input_list_column[[i]]
    }
    if (!is.null(row_data$mapped_panther_ids) && !is.na(row_data$mapped_panther_ids)) {
      return(as.character(row_data$mapped_panther_ids))
    } else {
      return("")
    }
  })

  return(gene_lists)
}

alterPantherSourceKeywords <- function(pantherResult) {
  # Map PANTHER's data source names to FLAME's standard names
  # This ensures consistency with existing FLAME categories
  # NOTE: Most conversions happen in mapPantherDatasetToFlameSource
  # This function acts as a defensive fallback

  # Full GO terms (defensive - should already be converted)
  pantherResult$Source <- gsub("^GO:0003674$", "GO:MF", pantherResult$Source)
  pantherResult$Source <- gsub("^GO:0008150$", "GO:BP", pantherResult$Source)
  pantherResult$Source <- gsub("^GO:0005575$", "GO:CC", pantherResult$Source)

  # Reactome pathway (defensive - should already be converted)
  pantherResult$Source <- gsub("^ANNOT_TYPE_ID_REACTOME_PATHWAY$", "REAC", pantherResult$Source)

  return(pantherResult)
}

mapPantherTermIds <- function(pantherResult) {
  # Convert PANTHER-specific term IDs to FLAME-expected format where needed

  # GO terms should already be in correct format (GO:0123456)
  # No conversion needed for GO:MF, GO:BP, GO:CC

  # Reactome pathway IDs may need prefix adjustment
  # PANTHER returns Reactome IDs - check if they need R- prefix
  if (any(pantherResult$Source == "REAC")) {
    reac_mask <- pantherResult$Source == "REAC"
    # Check if Term_ID needs R- prefix (if not already present)
    missing_prefix <- reac_mask & !grepl("^R-", pantherResult$Term_ID)
    if (any(missing_prefix)) {
      pantherResult[missing_prefix, "Term_ID"] <- paste0("R-", pantherResult[missing_prefix, "Term_ID"])
    }
  }

  return(pantherResult)
}

mapDataSourcesToPantherDatasets <- function(selectedDataSources) {
  # Map FLAME data source names to PANTHER annotation dataset IDs
  datasetMapping <- list(
    "GO:MF" = "GO:0003674",
    "GO:BP" = "GO:0008150",
    "GO:CC" = "GO:0005575",
    "GOSLIM:MF" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF",
    "GOSLIM:BP" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP",
    "GOSLIM:CC" = "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC",
    "REAC" = "ANNOT_TYPE_ID_REACTOME_PATHWAY",
    "PANTHER Pathways" = "ANNOT_TYPE_ID_PANTHER_PATHWAY",
    "PANTHERPC" = "ANNOT_TYPE_ID_PANTHER_PC"
  )

  pantherDatasets <- c()
  for (source in selectedDataSources) {
    if (source %in% names(datasetMapping)) {
      pantherDatasets <- c(pantherDatasets, datasetMapping[[source]])
    }
  }

  # If no valid datasets selected, default to all categories
  if (length(pantherDatasets) == 0) {
    pantherDatasets <- unlist(datasetMapping)
  }

  return(pantherDatasets)
}

mapPantherDatasetToFlameSource <- function(dataset) {
  # Map PANTHER annotation dataset IDs back to FLAME source names
  mapping <- list(
    "GO:0003674" = "GO:MF",
    "GO:0008150" = "GO:BP",
    "GO:0005575" = "GO:CC",
    "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF" = "GOSLIM:MF",
    "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP" = "GOSLIM:BP",
    "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC" = "GOSLIM:CC",
    "ANNOT_TYPE_ID_REACTOME_PATHWAY" = "REAC",
    "ANNOT_TYPE_ID_PANTHER_PATHWAY" = "PANTHER Pathways",
    "ANNOT_TYPE_ID_PANTHER_PC" = "PANTHERPC"
  )

  if (dataset %in% names(mapping)) {
    return(mapping[[dataset]])
  } else {
    return(dataset)  # Return original if no mapping found
  }
}


# =============================================================================
# PANTHERStrategy - Tool Strategy Implementation
# =============================================================================

PANTHERStrategy <- R6::R6Class("PANTHERStrategy",

  inherit = ToolStrategy,

  public = list(
    initialize = function() {
      super$initialize("PANTHER")
    },

    run = function(inputList, organism, backgroundList, params) {
      # Map datasources to PANTHER dataset IDs
      pantherDatasets <- mapDataSourcesToPantherDatasets(params$datasources)

      # Map metric to PANTHER correction parameter
      correction <- switch(params$metric,
        "False discovery rate" = "FDR",
        "P-value" = "NONE",
        "Bonferroni" = "BONFERRONI",
        "FDR"  # default
      )

      # Send requests for each dataset
      allResponses <- private$sendRequests(
        inputList, organism, backgroundList, pantherDatasets, correction
      )

      if (!private$areResponsesValid(allResponses)) {
        return(NULL)
      }

      # Parse and combine results
      result <- private$parseResponses(allResponses, params$metric, params$threshold)

      if (!isEnrichmentResultValid(result)) {
        return(NULL)
      }

      # Filter by datasources
      if (exists("currentEnrichmentType")) {
        result <- filterEnrichmentByDataSources(result, currentEnrichmentType)
      }

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Store background size (still using global for now)
      if (exists("currentType_Tool")) {
        enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<-
          getSimpleBackgroundSize(backgroundList)
      }

      return(result)
    },

    convertIDs = function(geneList, organism, targetNamespace) {
      return(geneList)
    },

    getValidDatasources = function(organism) {
      return(DATASOURCES[["PANTHER"]])
    },

    getDefaultMetric = function(hasBackground) {
      return("FDR")
    }
  ),

  private = list(
    sendRequests = function(inputList, taxid, backgroundList, datasets, correction) {
      url <- "https://pantherdb.org/services/oai/pantherdb/enrich/overrep"
      identifiers <- paste0(inputList, collapse = ",")
      allResults <- list()

      for (dataset in datasets) {
        if (is.null(backgroundList)) {
          params <- list(
            geneInputList = identifiers,
            organism = taxid,
            annotDataSet = dataset,
            correction = correction,
            enrichmentTestType = "FISHER",
            mappedInfo = "COMP_LIST"
          )
        } else {
          background <- paste0(backgroundList, collapse = ",")
          params <- list(
            geneInputList = identifiers,
            organism = taxid,
            refInputList = background,
            refOrganism = taxid,
            annotDataSet = dataset,
            correction = correction,
            enrichmentTestType = "FISHER",
            mappedInfo = "COMP_LIST"
          )
        }

        response <- httr::POST(url, body = params, encode = "form")
        allResults[[dataset]] <- response
      }

      return(allResults)
    },

    areResponsesValid = function(responseList) {
      for (dataset in names(responseList)) {
        if (responseList[[dataset]]$status_code != 200) {
          renderWarning(paste("Connection to PANTHER failed for", dataset))
          return(FALSE)
        }
      }
      return(TRUE)
    },

    parseResponses = function(responseList, metric, threshold) {
      allResults <- data.frame()
      threshold <- as.numeric(threshold)

      sigColumn <- switch(metric,
        "False discovery rate" = "fdr",
        "P-value" = "pValue",
        "Bonferroni" = "pValue",
        "fdr"
      )

      for (dataset in names(responseList)) {
        response <- responseList[[dataset]]
        responseBody <- rawToChar(httr::content(response, "raw"))

        if (nchar(responseBody) == 0) next

        tryCatch({
          pantherResult <- jsonlite::fromJSON(responseBody)
        }, error = function(e) {
          next
        })

        if (!"results" %in% names(pantherResult)) next
        if (!"result" %in% names(pantherResult$results)) next

        results <- pantherResult$results$result
        if (length(results) == 0 || !is.data.frame(results)) next

        results$annotation_dataset <- dataset
        results$flame_source <- mapPantherDatasetToFlameSource(dataset)
        results$term_id <- results$term$id
        results$term_label <- results$term$label

        query_size <- if (nrow(allResults) == 0) {
          max(results$number_in_list, na.rm = TRUE)
        } else {
          max(c(allResults$query_size[1], max(results$number_in_list, na.rm = TRUE)), na.rm = TRUE)
        }
        results$query_size <- query_size

        # Filter by threshold
        if (metric == "False discovery rate") {
          significant_mask <- results[[sigColumn]] < threshold
        } else {
          significant_mask <- results$pValue < threshold
        }
        results <- results[significant_mask, ]

        if (nrow(results) == 0) next

        # Filter unclassified
        unclassified_mask <- is.na(results$term_id) |
          is.na(results$term_label) |
          results$term_label == "" |
          toupper(results$term_label) == "UNCLASSIFIED"
        results <- results[!unclassified_mask, ]

        if (nrow(results) == 0) next

        positive_hits <- if ("input_list" %in% names(results)) {
          extractPantherGeneList(results$input_list)
        } else {
          rep("", nrow(results))
        }

        pantherSelected <- data.frame(
          Source = results$flame_source,
          Term_ID = results$term_id,
          Function = results$term_label,
          Pvalue = results[[sigColumn]],
          Term_Size = results$number_in_reference,
          Query_size = results$query_size,
          Intersection_Size = results$number_in_list,
          Positive_Hits = positive_hits,
          stringsAsFactors = FALSE
        )

        colnames(pantherSelected) <- ENRICHMENT_DF_COLNAMES
        allResults <- rbind(allResults, pantherSelected)
      }

      if (nrow(allResults) > 0) {
        allResults <- alterPantherSourceKeywords(allResults)
        allResults <- mapPantherTermIds(allResults)
      }

      return(allResults)
    }
  )
)

# Register the strategy
toolRegistry$register("functional", "PANTHER", PANTHERStrategy$new())