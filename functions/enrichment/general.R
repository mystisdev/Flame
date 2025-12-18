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
  enrichmentParsedResult <- enrichmentParsedResult[, c(
    "Source", "Term_ID", "Function", "P-value", "-log10Pvalue",
    "Term Size", "Query size", "Intersection Size",
    "Enrichment Score %", "Positive Hits"
  )]
  return(enrichmentParsedResult)
}

calculateEnrichmentScore <- function(hitGenesCount, databaseGenesCount) {
  enrichmentScore <- round((hitGenesCount / databaseGenesCount) * 100, 2)
  return(enrichmentScore)
}

isResultValid <- function(result) {
  isValid <- F
  if (!is.null(result) && nrow(result) > 0)
    isValid <- T
  return(isValid)
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
