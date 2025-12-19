runPanther <- function(userInputList, taxid, user_reference = NULL) {
  # userInputList contains gene identifiers that PANTHER API accepts
  # PANTHER supports: ENSEMBL gene/protein/transcript, ENTREZ, gene symbols,
  # NCBI GI, HGNC, IPI, UniGene, UniProt accession/ID

  # Send enrichment request to PANTHER API
  pantherResult <- sendPantherRequest(userInputList, taxid, user_reference)

  if (isPantherResponseValid(pantherResult)) {
    # Parse API response into FLAME-compatible format
    pantherParsedResult <- parsePantherResult(pantherResult)

    if (isEnrichmentResultValid(pantherParsedResult)) {
      # Filter by user's selected data sources (GO, REAC, etc.)
      pantherParsedResult <- filterEnrichmentByDataSources(pantherParsedResult, currentEnrichmentType)

      if (nrow(pantherParsedResult) > 0) {
        # Store results in global structure after adding -log10Pvalue and enrichment scores
        enrichmentResults[[currentType_Tool]] <<-
          transformEnrichmentResultTable(pantherParsedResult)

      }
    }
  }

  # Always store background size for statistics display
  enrichmentBackgroundSizes[[toupper(currentType_Tool)]] <<- getSimpleBackgroundSize(user_reference)
}

sendPantherRequest <- function(userInputList, taxid, user_reference = NULL) {
  url <- "https://pantherdb.org/services/oai/pantherdb/enrich/overrep"

  # Get selected data sources and map to PANTHER annotation dataset IDs
  selectedDataSources <- input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  pantherDatasets <- mapDataSourcesToPantherDatasets(selectedDataSources)

  # PANTHER expects comma-separated gene lists using PANTHER accession IDs
  identifiers <- paste0(userInputList, collapse = ",")

  # Get significance metric setting and map to PANTHER correction parameter
  significanceMetric <- currentSignificanceMetric
  correction <- switch(
    significanceMetric,
    "False discovery rate" = "FDR",
    "P-value" = "NONE",
    "Bonferroni" = "BONFERRONI",
    DEFAULT_METRIC_TEXT = "FDR"
  )

  # Send requests for each selected annotation dataset
  allResults <- list()

  for (dataset in pantherDatasets) {
    if (is.null(user_reference)) {
      # Genome-wide background analysis
      params <- list(
        geneInputList = identifiers,
        organism = taxid,
        annotDataSet = dataset,
        correction = correction,
        enrichmentTestType = "FISHER",
        mappedInfo = "COMP_LIST"  # Request gene-term associations
      )
    } else {
      # Custom background analysis
      background <- paste0(user_reference, collapse = ",")
      params <- list(
        geneInputList = identifiers,
        organism = taxid,
        refInputList = background,
        refOrganism = taxid,
        annotDataSet = dataset,
        correction = correction,
        enrichmentTestType = "FISHER",
        mappedInfo = "COMP_LIST"  # Request gene-term associations
      )
    }

    response <- httr::POST(url, body = params, encode = "form")
    allResults[[dataset]] <- response
  }

  return(allResults)
}

isPantherResponseValid <- function(responseList) {
  isValid <- TRUE
  for (dataset in names(responseList)) {
    response <- responseList[[dataset]]
    if (response$status_code != 200) {
      isValid <- FALSE
      renderWarning(paste("Connection to PANTHER could not be established for", dataset, ". Please try again later."))
      break
    }
  }
  return(isValid)
}

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

  # Extract mapped_panther_ids (PANTHER protein accessions) from each row
  gene_lists <- sapply(seq_len(nrow(input_list_column)), function(i) {
    row_data <- input_list_column[i, ]
    if (!is.null(row_data$mapped_panther_ids) && !is.na(row_data$mapped_panther_ids)) {
      return(as.character(row_data$mapped_panther_ids))
    } else {
      return("")
    }
  })

  return(gene_lists)
}

parsePantherResult <- function(responseList) {
  # Parse JSON responses from PANTHER API and combine results
  allResults <- data.frame()

  # Select the p-value column based on user's metric choice
  significanceColumnName <- switch(
    currentSignificanceMetric,
    "False discovery rate" = "fdr",
    "P-value" = "pValue",
    "Bonferroni" = "pValue",  # PANTHER applies Bonferroni to pValue when correction=BONFERRONI
    DEFAULT_METRIC_TEXT = "fdr"
  )

  for (dataset in names(responseList)) {
    response <- responseList[[dataset]]
    responseBody <- rawToChar(httr::content(response, "raw"))

    # Skip if empty response
    if (nchar(responseBody) == 0) next

    # Parse JSON with error handling
    tryCatch({
      pantherResult <- jsonlite::fromJSON(responseBody)
    }, error = function(e) {
      renderWarning(paste("Failed to parse PANTHER response for dataset", dataset, ":", e$message))
      next
    })

    # Extract enrichment results
    if ("results" %in% names(pantherResult) && "result" %in% names(pantherResult$results)) {
      results <- pantherResult$results$result

      if (length(results) > 0 && is.data.frame(results)) {
        # Add source annotation dataset information
        results$annotation_dataset <- dataset

        # Map dataset to FLAME data source name
        results$flame_source <- mapPantherDatasetToFlameSource(dataset)

        # Extract term information - results$term is already a data.frame with id and label columns
        results$term_id <- results$term$id
        results$term_label <- results$term$label

        # Calculate query size from number_in_list (assuming this represents unique genes)
        # PANTHER doesn't provide total query size directly, so we estimate from results
        if (nrow(allResults) == 0) {
          query_size <- max(results$number_in_list, na.rm = TRUE)
        } else {
          query_size <- max(c(allResults$query_size[1], max(results$number_in_list, na.rm = TRUE)), na.rm = TRUE)
        }
        results$query_size <- query_size


        # Apply user's significance filtering (same as other tools)
        threshold <- as.numeric(input$functional_enrichment_threshold)

        # Filter based on user's selected significance metric
        if (currentSignificanceMetric == "False discovery rate") {
          significant_mask <- results[[significanceColumnName]] < threshold
        } else {
          # P-value or Bonferroni - use pValue column
          significant_mask <- results$pValue < threshold
        }

        results <- results[significant_mask, ]

        if (nrow(results) == 0) {
          next  # Skip this dataset if no significant results
        }

        # Filter out unclassified terms
        # PANTHER returns "UNCLASSIFIED" terms with null term_id that
        # provide no functional information. These appear across all
        # annotation categories and can have significant p-values.
        unclassified_mask <- is.na(results$term_id) |
          is.na(results$term_label) |
          results$term_label == "" |
          toupper(results$term_label) == "UNCLASSIFIED"

        results <- results[!unclassified_mask, ]

        if (nrow(results) == 0) {
          next  # Skip this dataset if no valid results after filtering
        }

        # Extract gene lists from input_list column (if available from mappedInfo=COMP_LIST)
        positive_hits <- if ("input_list" %in% names(results)) {
          extractPantherGeneList(results$input_list)
        } else {
          rep("", nrow(results))  # Fallback to empty strings if no gene data
        }

        # Select columns we need and rename to FLAME format
        pantherSelected <- data.frame(
          Source = results$flame_source,
          Term_ID = results$term_id,
          Function = results$term_label,
          Pvalue = results[[significanceColumnName]],
          Term_Size = results$number_in_reference,
          Query_size = results$query_size,
          Intersection_Size = results$number_in_list,
          Positive_Hits = positive_hits,
          stringsAsFactors = FALSE
        )

        # Rename columns to FLAME's standard names
        colnames(pantherSelected) <- ENRICHMENT_DF_COLNAMES

        allResults <- rbind(allResults, pantherSelected)
      }
    }
  }

  if (nrow(allResults) > 0) {
    # Apply PANTHER-specific data processing
    allResults <- alterPantherSourceKeywords(allResults)
    allResults <- mapPantherTermIds(allResults)
  }

  return(allResults)
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