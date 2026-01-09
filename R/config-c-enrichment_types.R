# ============================================================================
# Enrichment Types Configuration
# ============================================================================

# Central registry for enrichment type behavior and capabilities.
# This enables config-driven architecture for multi-run enrichment handling.
#
# Adding a new enrichment type (e.g., GSEA):
# 1. Add entry to ENRICHMENT_TYPES_CONFIG below
# 2. Add UI panel in views/enrichment.R
# 3. Add tab generation function if UI differs significantly
# 4. Add tool runner functions (e.g., runFgsea())
# NO changes needed to handleEnrichmentRun(), insertEnrichmentTab(), etc.

ENRICHMENT_TYPES_CONFIG <- list(
  functional = list(
    # Identity
    id = "functional",
    label = "Functional Enrichment",

    # UI Configuration
    tabsetPanelId = "toolTabsPanel",
    resultsPanelId = "functionalEnrichmentResultsPanel",
    clearButtonId = "functional_enrichment_all_clear",
    closeEvent = "closeRunTab",

    # Data Configuration
    datasources = ENRICHMENT_DATASOURCES,  # GO:BP, GO:MF, KEGG, etc.
    tools = c("gProfiler", "WebGestalt", "enrichR", "PANTHER", "GeneCodis"),

    # Logic Flags
    supportsCombination = TRUE,
    hasManhattanPlot = TRUE,  # gProfiler-specific

    # Tab Content Generator
    generateTabContent = "generateToolPanelForRun"
  )

  # FUTURE: Add GSEA without modifying existing code
  # gsea = list(
  #   id = "gsea",
  #   label = "Gene Set Enrichment Analysis",
  #   inputType = "ranked_list",  # Different from gene_set
  #   tabsetPanelId = "gseaToolTabsPanel",
  #   resultsPanelId = "gseaEnrichmentResultsPanel",
  #   clearButtonId = "gsea_enrichment_all_clear",
  #   closeEvent = "closeGseaRunTab",
  #   datasources = c("GO", "KEGG", "REACTOME"),
  #   tools = c("fgsea", "clusterProfiler"),
  #   supportsCombination = TRUE,
  #   hasManhattanPlot = FALSE,
  #   requiresRanking = TRUE,
  #   rankingMetrics = c("log2FC", "stat", "pvalue"),
  #   generateTabContent = "generateToolPanelForGseaRun"
  # )
)

# ============================================================================
# Helper Functions
# ============================================================================

#' Get Enrichment Type Configuration
#'
#' Retrieves the configuration for a specific enrichment type.
#'
#' @param type Character string: "functional", etc.
#' @return List containing all configuration properties for the type
#' @examples
#' config <- getEnrichmentConfig("functional")
#' print(config$datasources)  # GO:BP, GO:MF, etc.
getEnrichmentConfig <- function(type) {
  config <- ENRICHMENT_TYPES_CONFIG[[type]]
  if (is.null(config)) {
    stop(paste("Unknown enrichment type:", type))
  }
  return(config)
}

#' Get Valid Datasources for Enrichment Type
#'
#' Returns the list of valid datasources for a specific enrichment type.
#' This is used by plot datasource pickers to show appropriate options.
#'
#' @param type Character string: "functional", etc.
#' @return Character vector of valid datasource names
#' @examples
#' getValidDatasources("functional")  # c("GO:BP", "GO:MF", "KEGG", ...)
getValidDatasources <- function(type) {
  return(getEnrichmentConfig(type)$datasources)
}

#' Check if Enrichment Type Supports Combination
#'
#' @param type Character string: "functional", etc.
#' @return Logical TRUE if combination tab is supported
supportsCombination <- function(type) {
  config <- getEnrichmentConfig(type)
  return(isTRUE(config$supportsCombination))
}

#' Get All Registered Enrichment Types
#'
#' @return Character vector of registered type IDs
getEnrichmentTypes <- function() {
  return(names(ENRICHMENT_TYPES_CONFIG))
}
