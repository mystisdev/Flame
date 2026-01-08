# =============================================================================
# FLAME Conversion Result Handler
# =============================================================================
#
# Adds gene ID conversion results (from g:Convert or g:Orth) to the input lists.
#
# Dependencies:
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - input-analytelist-unranked.R (for UnrankedAnalyteList)
# - infrastructure-config.R (for AnalyteType)
# - analyteListRegistry (global, created in server.R)
#
# =============================================================================

#' Add conversion/orthology results to input lists
#'
#' Takes results from g:Convert or g:Orth and adds them as a new analyte list.
#' Validation (list limits, gene limits) is handled by the entity/registry classes.
#'
#' @param prefix Character. Either "gconvert" or "gorth"
#' @param dtype Character. The data type to extract ("name", "target", "ortholog_name", "ortholog_ensg")
addConversionResultToInput <- function(prefix, dtype) {
  tryCatch({
    shinyjs::hide(paste0(prefix, "_addedInfo"))
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your data.</p>")

    # Extract results based on prefix
    if (prefix == "gconvert") {
      conversionTable <- currentConversionResult
      names <- unlist(conversionTable$name)
      ids <- unlist(conversionTable$target)
    } else {
      conversionTable <- currentOrthologyResult
      names <- unlist(conversionTable$ortholog_name)
      ids <- unlist(conversionTable$ortholog_ensg)
    }

    # Select the appropriate result column
    if (dtype == "name" || dtype == "ortholog_name") {
      result <- names
    } else {
      result <- ids
    }

    # Clean and deduplicate
    result <- unique(as.character(result))
    result <- result[!is.na(result) & result != ""]

    # Validate empty input
    if (length(result) == 0) {
      renderWarning("No valid results to add.")
      return()
    }

    # Generate unique name
    listName <- sprintf("%s_%s", prefix, dtype)
    existingNames <- analyteListRegistry$getNames()
    if (listName %in% existingNames) {
      suffix <- 1
      while (paste0(listName, "_", suffix) %in% existingNames) {
        suffix <- suffix + 1
      }
      listName <- paste0(listName, "_", suffix)
    }

    # Create and add analyte list (entity/registry handle validation)
    analyteList <- UnrankedAnalyteList$new(
      name = listName,
      analyteType = AnalyteType$GENE,
      ids = result
    )
    analyteListRegistry$add(analyteList)

    shinyjs::show(paste0(prefix, "_addedInfo"))
  }, error = function(e) {
    cat(paste("[Conversion] Error:", conditionMessage(e), "\n"))
    renderWarning(conditionMessage(e))
  }, finally = {
    removeModal()
  })
}
