# Run Management for Multi-Run Enrichment Architecture
# Handles run identification, parameter comparison, and run lifecycle

# Get the next display number for a tool (can reset on Clear All)
getNextRunNumber <- function(toolName) {
  if (is.null(runCounters[[toolName]]) || length(runCounters[[toolName]]) == 0) {
    runCounters[[toolName]] <<- 1
  }
  runNumber <- runCounters[[toolName]]
  runCounters[[toolName]] <<- runNumber + 1
  return(runNumber)
}

# Get the next unique ID for a tool (NEVER resets - prevents Shiny caching bugs)
getNextUniqueId <- function(toolName) {
  if (is.null(uniqueIdCounters[[toolName]]) || length(uniqueIdCounters[[toolName]]) == 0) {
    uniqueIdCounters[[toolName]] <<- 1
  }
  uniqueId <- uniqueIdCounters[[toolName]]
  uniqueIdCounters[[toolName]] <<- uniqueId + 1
  return(uniqueId)
}

# Create a run ID from tool name and run number (e.g., "gProfiler_1")
createRunId <- function(toolName, runNumber) {
  paste(toolName, runNumber, sep = "_")
}

# Create the full run key including enrichment type (e.g., "functional_gProfiler_1")
getFullRunKey <- function(enrichmentType, runId) {
  paste(enrichmentType, runId, sep = "_")
}

# Parse a full run key into its components
parseFullRunKey <- function(fullRunKey) {
  parts <- strsplit(fullRunKey, "_")[[1]]
  # Format: enrichmentType_toolName_runNumber
  # - First part is enrichmentType (functional or literature)
  # - Last part is runNumber
  # - Everything in between is toolName
  enrichmentType <- parts[1]
  runNumber <- as.integer(parts[length(parts)])
  toolName <- paste(parts[2:(length(parts)-1)], collapse = "_")

  return(list(
    enrichmentType = enrichmentType,
    toolName = toolName,
    runNumber = runNumber,
    runId = paste(toolName, runNumber, sep = "_")
  ))
}

# Capture current enrichment parameters from input
# Resolves "Default" placeholders to tool-specific values for consistent comparison
captureRunParameters <- function() {
  # Get raw input values
  rawNamespace <- input[[paste0(currentEnrichmentType, "_enrichment_namespace")]]
  rawMetric <- input[[paste0(currentEnrichmentType, "_enrichment_metric")]]

  # Resolve namespace default to tool-specific value
  resolvedNamespace <- if (rawNamespace == DEFAULT_NAMESPACE_TEXT) {
    getDefaultTargetNamespace()
  } else {
    rawNamespace
  }

  # Resolve metric default to tool-specific value
  resolvedMetric <- if (rawMetric == DEFAULT_METRIC_TEXT) {
    backgroundMode <- input[[paste0(currentEnrichmentType, "_enrichment_background_choice")]]
    if (backgroundMode == "genome") {
      DEFAULT_METRICS_GENOME[[toupper(currentEnrichmentTool)]]
    } else {
      DEFAULT_METRICS_USERBACKGROUND[[toupper(currentEnrichmentTool)]]
    }
  } else {
    rawMetric
  }

  list(
    geneListName = input[[paste0(currentEnrichmentType, "_enrichment_file")]],
    organism = input[[paste0(currentEnrichmentType, "_enrichment_organism")]],
    threshold = input[[paste0(currentEnrichmentType, "_enrichment_threshold")]],
    datasources = sort(input[[paste0(currentEnrichmentType, "_enrichment_datasources")]]),
    namespace = resolvedNamespace,
    backgroundMode = input[[paste0(currentEnrichmentType, "_enrichment_background_choice")]],
    backgroundList = if (input[[paste0(currentEnrichmentType, "_enrichment_background_choice")]] == "genome") {
      NULL
    } else {
      input[[paste0(currentEnrichmentType, "_enrichment_background_list")]]
    },
    metric = resolvedMetric
  )
}

# Check if two parameter sets match (including datasources)
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

# Find an existing run with matching parameters for a given tool
# Returns: list(matchType = "exact"|"datasources_differ"|NULL, fullRunKey = key|NULL)
findMatchingRun <- function(enrichmentType, toolName, params) {
  for (fullRunKey in names(activeRuns)) {
    run <- activeRuns[[fullRunKey]]
    if (run$enrichmentType == enrichmentType && run$toolName == toolName) {
      if (parametersMatch(run$parameters, params)) {
        return(list(matchType = "exact", fullRunKey = fullRunKey))
      }
      if (parametersMatchIgnoringDatasources(run$parameters, params)) {
        return(list(matchType = "datasources_differ", fullRunKey = fullRunKey))
      }
    }
  }
  return(list(matchType = NULL, fullRunKey = NULL))
}

# Update stored parameters for a run (used when datasources change)
updateRunParameters <- function(fullRunKey, newParams) {
  activeRuns[[fullRunKey]]$parameters <<- newParams
  activeRuns[[fullRunKey]]$timestamp <<- Sys.time()
}

# Register a new run in activeRuns
# uniqueId: used for internal Shiny IDs (never reused)
# displayNumber: shown to user in tab title (can reset)
registerRun <- function(enrichmentType, toolName, uniqueId, displayNumber, params) {
  runId <- createRunId(toolName, uniqueId)
  fullRunKey <- getFullRunKey(enrichmentType, runId)

  activeRuns[[fullRunKey]] <<- list(
    runId = runId,
    toolName = toolName,
    uniqueId = uniqueId,
    runNumber = displayNumber,  # Keep as runNumber for backward compatibility
    enrichmentType = enrichmentType,
    timestamp = Sys.time(),
    parameters = params
  )

  return(fullRunKey)
}

# Unregister a run from activeRuns
unregisterRun <- function(fullRunKey) {
  activeRuns[[fullRunKey]] <<- NULL
}

# Get all active runs for a specific enrichment type
getActiveRunsByType <- function(enrichmentType) {
  runs <- list()
  for (fullRunKey in names(activeRuns)) {
    run <- activeRuns[[fullRunKey]]
    if (run$enrichmentType == enrichmentType) {
      runs[[fullRunKey]] <- run
    }
  }
  return(runs)
}

# Get all active functional runs
getActiveFunctionalRuns <- function() {
  getActiveRunsByType("functional")
}

# Get count of active functional runs
getActiveFunctionalRunCount <- function() {
  length(getActiveFunctionalRuns())
}

# Initialize run counters for all tools
initializeRunCounters <- function() {
  lapply(ENRICHMENT_TOOLS, function(tool) {
    runCounters[[tool]] <<- 1
  })
}

# Count how many tabs exist for a tool
countActiveRunsForTool <- function(toolName) {
  count <- 0
  for (key in names(activeRuns)) {
    if (activeRuns[[key]]$toolName == toolName) count <- count + 1
  }
  return(count)
}

# Reset counter for a specific tool (called when all tabs for that tool are closed)
resetRunCounterForTool <- function(toolName) {
  runCounters[[toolName]] <<- 1
}
