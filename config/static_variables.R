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

# === UI Panel State ===
# Set by generateToolPanel() for UI generation and used as default parameter values
# in various printing and update functions
currentType_Tool <- ""