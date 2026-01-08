# Input
userInputLists <- list()
# Note: volcanoSelectedItems and reductionSelectedItems moved to private fields in
# VolcanoInputSession and ReductionInputSession respectively

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
# Note: currentVariantResults, currentTextminingResult, currentVolcano, volcanoPlotData,
# currentReduction, and currentUpsetMode moved to private fields in their respective InputSession classes
currentConversionResult <- data.frame()
currentOrthologyResult <- data.frame()

# === UI Panel State ===
# Set by generateToolPanel() for UI generation and used as default parameter values
# in various printing and update functions
currentType_Tool <- ""