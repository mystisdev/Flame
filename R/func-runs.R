# =============================================================================
# RUN MANAGEMENT - Parameter Comparison and Run Key Parsing
# =============================================================================
# This file contains utilities for run identification and parameter comparison.
#
# NOTE: Most run management functions have moved to EnrichmentSessionRegistry.
# See: R/enrich-session-registry.R
#
# Functions moved to registry:
# - getNextUniqueId() -> enrichmentSessionRegistry$getNextUniqueId()
# - generateDisplayNumber() -> enrichmentSessionRegistry$generateDisplayNumber()
# - resetDisplayCounter() -> enrichmentSessionRegistry$resetDisplayCounter()
# - countByTool() -> enrichmentSessionRegistry$countByTool()
#
# Functions moved to EnrichmentFormSession:
# - captureRunParameters() -> EnrichmentFormSession$captureRunParameters()
# - findMatchingRun() -> EnrichmentFormSession$findMatchingRun()
# =============================================================================

# Parse a full run key into its components
# Format: enrichmentType_toolName_uniqueId (e.g., "functional_gProfiler_5")
parseFullRunKey <- function(fullRunKey) {
  parts <- strsplit(fullRunKey, "_")[[1]]
  # Format: enrichmentType_toolName_runNumber
  # - First part is enrichmentType (functional)
  # - Last part is runNumber/uniqueId
  # - Everything in between is toolName
  enrichmentType <- parts[1]
  runNumber <- as.integer(parts[length(parts)])
  toolName <- paste(parts[2:(length(parts) - 1)], collapse = "_")

  return(list(
    enrichmentType = enrichmentType,
    toolName = toolName,
    runNumber = runNumber,
    runId = paste(toolName, runNumber, sep = "_")
  ))
}

# Check if two parameter sets match (including datasources)
# Used for exact match detection (no new run needed)
parametersMatch <- function(params1, params2) {
  if (is.null(params1) || is.null(params2)) return(FALSE)

  # Compare each parameter
  identical(params1$geneListName, params2$geneListName) &&
    identical(params1$organism, params2$organism) &&
    identical(params1$threshold, params2$threshold) &&
    identical(sort(params1$datasources), sort(params2$datasources)) &&
    identical(params1$namespace, params2$namespace) &&
    identical(params1$backgroundMode, params2$backgroundMode) &&
    identical(params1$backgroundList, params2$backgroundList) &&
    identical(params1$metric, params2$metric)
}

# Check if two parameter sets match IGNORING datasources
# Used to detect when only datasources changed (update existing tab)
parametersMatchIgnoringDatasources <- function(params1, params2) {
  if (is.null(params1) || is.null(params2)) return(FALSE)

  # Compare all parameters EXCEPT datasources
  identical(params1$geneListName, params2$geneListName) &&
    identical(params1$organism, params2$organism) &&
    identical(params1$threshold, params2$threshold) &&
    # NOTE: datasources intentionally NOT compared
    identical(params1$namespace, params2$namespace) &&
    identical(params1$backgroundMode, params2$backgroundMode) &&
    identical(params1$backgroundList, params2$backgroundList) &&
    identical(params1$metric, params2$metric)
}
