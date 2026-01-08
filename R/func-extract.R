# =============================================================================
# FLAME Extract Functions
# =============================================================================
#
# API wrapper functions for the EXTRACT API (Jensen Lab tagger).
# Used by TextMiningInputSession for extracting gene/protein names from text.
#
# API Documentation: https://extract.jensenlab.org/
#
# Dependencies:
# - httr package (for HTTP requests)
# - httpuv package (for URL encoding)
# - data.table package (for parsing TSV responses)
# - func-general.R (for isPOSTResponseValid)
#
# =============================================================================

# API base URL
EXTRACT_API_URL <- "https://tagger.jensenlab.org"

#' Extract gene/protein entities from text using EXTRACT API
#'
#' @param text Character. The text to analyze.
#' @param species Integer. NCBI taxonomy ID (e.g., 9606 for human).
#' @return Data frame with columns (Name, Type, ID) or NULL if no entities found.
#'
#' @examples
#' \dontrun{
#' result <- extract_entities("TP53 and BRCA1 are tumor suppressors", 9606)
#' }
extract_entities <- function(text, species) {
  tryCatch({
    encoded <- encode_for_extract_api(text, species)
    url <- paste0(EXTRACT_API_URL, "/GetEntities")
    params <- sprintf("document=%s&entity_types=%s&auto_detect=0&format=tsv",
                      encoded$text, encoded$entityTypes)

    request <- httr::POST(url, body = params)

    if (isPOSTResponseValid(request)) {
      result <- data.table::fread(
        sprintf("%s\n", rawToChar(httr::content(request, "raw"))),
        header = FALSE
      )
      names(result) <- c("Name", "Type", "ID")
      # Filter out the species type marker (-3)
      result <- result[result$Type != -3, ]

      if (nrow(result) == 0) {
        return(NULL)
      }
      return(result)
    } else {
      cat("[extract_entities] GetEntities request failed\n")
      return(NULL)
    }
  }, error = function(e) {
    cat(paste("[extract_entities] error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}

#' Get HTML with entities annotated/highlighted using EXTRACT API
#'
#' @param text Character. The text to analyze.
#' @param species Integer. NCBI taxonomy ID (e.g., 9606 for human).
#' @return Character string of HTML or NULL on failure.
#'
#' @examples
#' \dontrun{
#' html <- extract_annotated_html("TP53 and BRCA1 are tumor suppressors", 9606)
#' }
extract_annotated_html <- function(text, species) {
  tryCatch({
    encoded <- encode_for_extract_api(text, species)
    url <- paste0(EXTRACT_API_URL, "/GetHTML")
    params <- sprintf("document=%s&entity_types=%s&auto_detect=0",
                      encoded$text, encoded$entityTypes)

    request <- httr::POST(url, body = params)

    if (isPOSTResponseValid(request)) {
      return(rawToChar(httr::content(request, "raw")))
    } else {
      cat("[extract_annotated_html] GetHTML request failed\n")
      return(NULL)
    }
  }, error = function(e) {
    cat(paste("[extract_annotated_html] error:", conditionMessage(e), "\n"))
    return(NULL)
  })
}

#' Encode text and build entity types string for EXTRACT API requests
#'
#' @param text Character. Raw text input.
#' @param species Integer. NCBI taxonomy ID.
#' @return List with 'text' (URL-encoded) and 'entityTypes' (formatted string).
encode_for_extract_api <- function(text, species) {
  # Clean up text (remove newlines/carriage returns)
  text <- gsub("\n", " ", text)
  text <- gsub("\r", "", text)

  # URL encode the text
  textEncoded <- httpuv::encodeURIComponent(text)

  # Build entity types string
  # -3 is the type code for genes/proteins
  entityTypes <- c(-3)
  entityTypesStr <- paste(entityTypes, sep = "+", collapse = "+")
  entityTypesStr <- sprintf("%s+%s", entityTypesStr, species)

  return(list(
    text = textEncoded,
    entityTypes = entityTypesStr
  ))
}
