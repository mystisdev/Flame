transformEnrichmentResultTable <- function(enrichmentParsedResult) {
  enrichmentParsedResult$`-log10Pvalue` <- as.numeric(
    format(
      -log10(enrichmentParsedResult$`P-value`),
      format = "e", digits = 3
    )
  )
  # Keep P-value as numeric for DT slider filter; formatSignif() handles display formatting
  enrichmentParsedResult$`P-value` <- as.numeric(enrichmentParsedResult$`P-value`)
  enrichmentParsedResult$`Enrichment Score %` <- calculateEnrichmentScore(
    enrichmentParsedResult$`Intersection Size`,
    enrichmentParsedResult$`Term Size`
  )

  # Define base columns to keep
  baseColumns <- c(
    "Source", "Term_ID", "Function", "P-value", "-log10Pvalue",
    "Term Size", "Query size", "Intersection Size",
    "Enrichment Score %", "Positive Hits"
  )

  # Preserve Term_ID_noLinks if it exists (WebGestalt includes links pre-attached)
  if ("Term_ID_noLinks" %in% colnames(enrichmentParsedResult)) {
    baseColumns <- c(baseColumns, "Term_ID_noLinks")
  }

  enrichmentParsedResult <- enrichmentParsedResult[, baseColumns]
  return(enrichmentParsedResult)
}

calculateEnrichmentScore <- function(hitGenesCount, databaseGenesCount) {
  enrichmentScore <- round((hitGenesCount / databaseGenesCount) * 100, 2)
  return(enrichmentScore)
}

isEnrichmentResultValid <- function(result) {
  if (is.null(result)) return(FALSE)
  if (!is.data.frame(result)) return(FALSE)
  if (nrow(result) == 0) return(FALSE)
  return(TRUE)
}

# Backward compatibility alias for existing callers
isResultValid <- isEnrichmentResultValid

filterEnrichmentByDataSources <- function(result, enrichmentType, warn_on_empty = FALSE) {
  selectedDataSources <- input[[paste0(enrichmentType, "_enrichment_datasources")]]
  if (is.null(selectedDataSources) || length(selectedDataSources) == 0) {
    return(result)
  }

  filteredResult <- result[result$Source %in% selectedDataSources, ]

  if (warn_on_empty && nrow(filteredResult) == 0 && nrow(result) > 0) {
    renderWarning(paste0(
      "Data source filtering removed all results. ",
      "Results had sources: ", paste(unique(result$Source), collapse = ", "),
      " but you selected: ", paste(selectedDataSources, collapse = ", ")
    ))
  }

  return(filteredResult)
}

getSimpleBackgroundSize <- function(user_reference) {
  if (is.null(user_reference)) NULL else length(user_reference)
}

unlistDatasourceCodes <- function(sources, codes) {
  return(
    unlist(lapply(sources, function(sourceName) {
      names(codes[codes == sourceName])
    }))
  )
}

mapKEGGIds <- function(df) {
  if (length(df$Source[which(df$Source == "KEGG")]) > 0) {
    df[df$Source == "KEGG", ]$Term_ID <-
      paste0("map", gsub("[^0-9.-]", "", df[df$Source == "KEGG", ]$Term_ID))
  }
  return(df)
}
