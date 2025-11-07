runString <- function(userInputList, taxid, user_reference = NULL) {
  # userInputList and user_reference already contain STRING IDs from FLAME's gene conversion
  # See geneConvert() calls and stringPOSTConvertENSP in main.R

  # Send enrichment request to STRING API
  stringResult <- sendStringRequest(userInputList, taxid, user_reference)

  if (isStringResponseValid(stringResult)) {
    # Parse API response into FLAME-compatible format
    stringParsedResult <- parseStringResult(stringResult)

    if (isStringResultValid(stringParsedResult)) {
      # Filter by user's selected data sources (GO, KEGG, etc.)
      stringParsedResult <- filterStringByDataSources(stringParsedResult)

      if (nrow(stringParsedResult) > 0) {
        # Store results in global structure after adding -log10Pvalue and enrichment scores
        enrichmentResults[[currentType_Tool]] <<-
          transformEnrichmentResultTable(stringParsedResult)
      }
    }
  }

  # Always store background size for statistics display
  enrichmentBackgroundSizes[[sprintf("%s_STRING", toupper(currentEnrichmentType))]] <<- getStringBackgroundSize(user_reference)
}

sendStringRequest <- function(userInputList, taxid, user_reference = NULL) {
  url <- "https://string-db.org/api/tsv/enrichment"

  # STRING expects carriage return (%0d) separated gene lists
  identifiers <- paste0(userInputList, collapse = "%0d")

  if (is.null(user_reference)) {
    # Genome-wide background analysis
    params <- list(
      identifiers = identifiers,
      species = taxid
    )
  } else {
    # Custom background analysis - include background_string_identifiers parameter
    background <- paste0(user_reference, collapse = "%0d")
    params <- list(
      identifiers = identifiers,
      background_string_identifiers = background,
      species = taxid
    )
  }

  # Use POST instead of GET to avoid 414 Request-URI Too Large errors with large gene lists
  response <- httr::POST(url, body = params)
  return(response)
}

isStringResponseValid <- function(response) {
  isValid <- TRUE
  if (response$status_code != 200) {
    isValid <- FALSE
    renderWarning("Connection to STRING could not be established. Please try again later.")
  }
  return(isValid)
}

parseStringResult <- function(response) {
  # Parse TSV response from STRING API
  responseBody <- rawToChar(httr::content(response, "raw"))
  stringResult <- read.delim(text = responseBody, header = TRUE)

  if (nrow(stringResult) == 0) {
    return(NULL)
  }

  # Select the p-value column based on user's metric choice
  significanceColumnName <- switch(
    currentSignificanceMetric,
    "False discovery rate" = "fdr",
    "P-value" = "p_value",
    DEFAULT_METRIC_TEXT = "fdr"
  )

  # Extract columns we need from STRING's response
  stringResult <- stringResult[, c(
    "category", "term", "description", significanceColumnName,
    "number_of_genes_in_background", "number_of_genes", "inputGenes"
  )]

  # Calculate query size (total unique genes across all enrichment terms)
  # This is needed because STRING gives per-term gene counts, but we need overall query size
  allGenes <- unique(unlist(strsplit(paste(stringResult$inputGenes, collapse = ","), ",")))
  stringResult$query_size <- length(allGenes)

  # Reorder columns to match FLAME's expected format
  stringResult <- stringResult[, c(
    "category", "term", "description", significanceColumnName,
    "number_of_genes_in_background", "query_size", "number_of_genes", "inputGenes"
  )]

  # Rename columns to FLAME's standard names
  colnames(stringResult) <- ENRICHMENT_DF_COLNAMES

  # Clean up and standardize the data
  stringResult <- parseStringPositiveHits(stringResult)     # Fix gene list formatting
  stringResult <- alterStringSourceKeywords(stringResult)   # Map category names to FLAME standards
  stringResult <- mapStringTermIds(stringResult)            # Standardize term IDs for linking

  return(stringResult)
}

isStringResultValid <- function(stringParsedResult) {
  isValid <- FALSE
  if (!is.null(stringParsedResult) && nrow(stringParsedResult) > 0)
    isValid <- TRUE
  return(isValid)
}

parseStringPositiveHits <- function(stringResult) {
  stringResult$`Positive Hits` <- gsub(",", ",", stringResult$`Positive Hits`)
  return(stringResult)
}

alterStringSourceKeywords <- function(stringResult) {
  # Map STRING's data source names to FLAME's standard names
  # STRING uses different category names than what FLAME expects for display/filtering
  stringResult$Source <- gsub("^Process$", "GO:BP", stringResult$Source)
  stringResult$Source <- gsub("^Function$", "GO:MF", stringResult$Source)
  # Note: Do not confuse "Component" (GO:CC) with "COMPARTMENTS" (also in STRING)
  # "Component" = GO Cellular Component, "COMPARTMENTS" = subcellular
  stringResult$Source <- gsub("^Component$", "GO:CC", stringResult$Source)
  stringResult$Source <- gsub("^KEGG$", "KEGG", stringResult$Source)
  stringResult$Source <- gsub("^RCTM$", "REAC", stringResult$Source)
  stringResult$Source <- gsub("^WikiPathways$", "WP", stringResult$Source)
  stringResult$Source <- gsub("^InterPro$", "INTERPRO", stringResult$Source)
  stringResult$Source <- gsub("^Pfam$", "PFAM", stringResult$Source)
  stringResult$Source <- gsub("^Keyword$", "UNIPROT", stringResult$Source)
  stringResult$Source <- gsub("^PMID$", "PUBMED", stringResult$Source)
  stringResult$Source <- gsub("^DISEASES$", "DO", stringResult$Source)
  stringResult$Source <- gsub("^TISSUES$", "BTO", stringResult$Source)
  stringResult$Source <- gsub("^HPO$", "HP", stringResult$Source)
  return(stringResult)
}

filterStringByDataSources <- function(stringParsedResult) {
  selectedDataSources <- input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]
  if (is.null(selectedDataSources) || length(selectedDataSources) == 0) {
    return(stringParsedResult)
  }

  # Filter results to only include selected data sources
  filteredResult <- stringParsedResult[stringParsedResult$Source %in% selectedDataSources, ]
  return(filteredResult)
}

mapStringTermIds <- function(stringResult) {
  # GO Cellular Component Term IDs from "Component" category are already in correct format
  # STRING's "Component" category returns "GO:0070691" format directly
  # No conversion needed for GO:CC terms

  # Keep KEGG Pathway Term IDs in organism-specific format
  # STRING returns organism-specific KEGG IDs like "eco00260" (E. coli), "hsa05224" (human)
  # These are perfect for KEGG links - we should preserve them as-is since STRING
  # has already solved the organism mapping problem for us
  # No conversion needed - STRING's format is exactly what KEGG pathway URLs expect

  # Fix Reactome Pathway Term IDs
  # Problem: STRING returns "HSA-1643685" but FLAME's attachLinks("REAC", ...)
  # function expects "R-HSA-1643685" format for Reactome links
  # This matches the format that gProfiler produces (removes REAC: prefix from REAC:R-HSA-1643685)
  # Solution: Add "R-" prefix to STRING's HSA-formatted IDs
  # Examples: "HSA-1643685" -> "R-HSA-1643685"
  if (any(stringResult$Source == "REAC")) {
    reac_mask <- stringResult$Source == "REAC"
    # Check if Term_ID starts with HSA- (not already R-HSA-)
    hsa_mask <- reac_mask & grepl("^HSA-", stringResult$Term_ID)
    if (any(hsa_mask)) {
      stringResult[hsa_mask, "Term_ID"] <- paste0("R-", stringResult[hsa_mask, "Term_ID"])
    }
  }

  # Note: Other categories already return Term IDs in the correct format that FLAME 
  # expects, so no mapping needed.

  return(stringResult)
}

getStringBackgroundSize <- function(user_reference) {
  # Return background size for statistical reporting
  # NULL = genome-wide background, number = custom background size
  if (is.null(user_reference)) {
    size <- NULL
  } else {
    size <- length(user_reference)
  }
  return(size)
}
