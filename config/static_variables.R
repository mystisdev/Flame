# Input
userInputLists <- list()
checkedListNames <- list() # in combination with js_listNames
volcanoSelectedItems <- c()
reductionSelectedItems <- c()

# Enrichment
enrichmentResults <- list()
arenaEdgelist <- list()
gprofilerResult <- list() # for gprofiler ManhattanPlot only
gprofilerResults <- list() # per-run gProfiler results cache (for Manhattan)
combinationResult <- data.frame()
enrichmentBackgroundSizes <- list()

# Multi-run tracking (for functional enrichment)
activeRuns <- list()        # Metadata for each active run (key: fullRunKey)
runCounters <- list()       # Display numbers per tool (can reset on Clear All)
uniqueIdCounters <- list()  # Unique ID numbers per tool (NEVER reset - prevents Shiny caching bugs)
runsPendingControlUpdate <- character(0)  # Run IDs needing deferred control updates

# STRING Network
STRINGNetworkData <- list()


# Current
currentVariantResults <- data.frame()
currentTextminingResult <- c()
currentVolcano <- data.frame()
volcanoPlotData <- data.frame()  # Prepared data for volcano rendering (standardized column names)
currentReduction <- data.frame()
currentUpsetMode <- ""
currentConversionResult <- data.frame()
currentOrthologyResult <- data.frame()

currentUserList <- c()
currentEnrichmentType <- ""
currentOrganism <- ""
currentEnrichmentTool <- ""
currentType_Tool <- ""
currentFullRunKey <- ""      # For multi-run: includes run number (e.g., "functional_gProfiler_1")
currentRunNumber <- 0        # Current display number (shown in tab title)
currentUniqueId <- 0         # Current unique ID (used for internal Shiny IDs, never reused)
currentNamespace <- ""
currentSignificanceMetric <- ""
currentBackgroundList <- c()