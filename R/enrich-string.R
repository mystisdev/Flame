# Helper functions used by STRINGStrategy

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


# =============================================================================
# STRINGStrategy - Tool Strategy Implementation
# =============================================================================

STRINGStrategy <- R6::R6Class("STRINGStrategy",

  inherit = ToolStrategy,

  public = list(
    initialize = function() {
      super$initialize("STRING")
    },

    run = function(inputList, organism, backgroundList, params) {
      # STRING expects STRING IDs (already converted upstream by FLAME's gene conversion)
      # Send enrichment request
      response <- private$sendRequest(inputList, organism, backgroundList)

      if (!private$isResponseValid(response)) {
        return(NULL)
      }

      # Parse API response
      result <- private$parseResponse(response, params$metric)

      if (!isEnrichmentResultValid(result)) {
        return(NULL)
      }

      # Filter by user's selected datasources using params directly
      if (!is.null(params$datasources) && length(params$datasources) > 0) {
        result <- result[result$Source %in% params$datasources, ]
      }

      # Filter by threshold
      threshold <- as.numeric(params$threshold)
      result <- result[result$`P-value` <= threshold, ]

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
      # STRING ID conversion is handled upstream by FLAME
      return(geneList)
    },

    getValidDatasources = function(organism) {
      return(DATASOURCES[["STRING"]])
    },

    getDefaultMetric = function(hasBackground) {
      return("fdr")
    }
  ),

  private = list(
    sendRequest = function(inputList, taxid, backgroundList) {
      url <- "https://string-db.org/api/tsv/enrichment"
      identifiers <- paste0(inputList, collapse = "%0d")

      if (is.null(backgroundList)) {
        params <- list(identifiers = identifiers, species = taxid)
      } else {
        background <- paste0(backgroundList, collapse = "%0d")
        params <- list(
          identifiers = identifiers,
          background_string_identifiers = background,
          species = taxid
        )
      }

      return(httr::POST(url, body = params))
    },

    isResponseValid = function(response) {
      if (response$status_code != 200) {
        renderWarning("Connection to STRING could not be established. Please try again later.")
        return(FALSE)
      }
      return(TRUE)
    },

    parseResponse = function(response, metric) {
      responseBody <- rawToChar(httr::content(response, "raw"))
      result <- read.delim(text = responseBody, header = TRUE)

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Select p-value column based on metric
      sigColumn <- switch(metric,
        "False discovery rate" = "fdr",
        "P-value" = "p_value",
        "fdr"  # default
      )

      result <- result[, c(
        "category", "term", "description", sigColumn,
        "number_of_genes_in_background", "number_of_genes", "inputGenes"
      )]

      allGenes <- unique(unlist(strsplit(paste(result$inputGenes, collapse = ","), ",")))
      result$query_size <- length(allGenes)

      result <- result[, c(
        "category", "term", "description", sigColumn,
        "number_of_genes_in_background", "query_size", "number_of_genes", "inputGenes"
      )]

      colnames(result) <- ENRICHMENT_DF_COLNAMES
      result <- parseStringPositiveHits(result)
      result <- alterStringSourceKeywords(result)
      result <- mapStringTermIds(result)

      return(result)
    }
  )
)

# Register the strategy for both enrichment types
# STRING is used in both Functional and Literature enrichment
toolRegistry$register("functional", "STRING", STRINGStrategy$new())
toolRegistry$register("literature", "STRING", STRINGStrategy$new())
