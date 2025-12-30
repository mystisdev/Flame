# Main server function for FLAME Shiny application
# This function sets up all observers and reactive components
#
# This is called from inst/app/server.R
# All other R/ files in this package are loaded as package functions

serverMain <- function(input, output, session) {
  # Initialize package-level state for this session
  # These are now local to the server function (per-session)

  # Input state
  userInputLists <- list()
  checkedListNames <- list()
  volcanoSelectedItems <- c()
  reductionSelectedItems <- c()

  # Enrichment state
  enrichmentResults <- list()
  arenaEdgelist <- list()
  gprofilerResult <- list()
  gprofilerResults <- list()
  combinationResult <- data.frame()
  enrichmentBackgroundSizes <- list()

  # Multi-run tracking
  activeRuns <- list()
  runCounters <- list()
  uniqueIdCounters <- list()

  # STRING Network
  STRINGNetworkData <- list()

  # Current state
  currentVariantResults <- data.frame()
  currentTextminingResult <- c()
  currentVolcano <- data.frame()
  volcanoPlotData <- data.frame()
  currentReduction <- data.frame()
  currentUpsetMode <- ""
  currentConversionResult <- data.frame()
  currentOrthologyResult <- data.frame()

  # UI Panel State
  currentType_Tool <- ""

  # Plot state
  plotOriginalData <- list()
  plotCurrentView <- list()
  plotRenderedData <- list()
  selectedTermIds <- list()
  selectedPairs <- list()
  plotUpdateTracker <- list()

  # Tab tracking
  currentSelectedToolTab <- NULL
  currentSelectedLiteratureTab <- NULL

  # Output Registry for cleanup management (per-session)
  outputRegistry <- OutputRegistry$new()

  # Observer Registry for cleanup management (per-session)
  observerRegistry <- ObserverRegistry$new()

  # Create an environment to hold session state that functions can access
  sessionEnv <- new.env(parent = emptyenv())
  sessionEnv$userInputLists <- userInputLists
  sessionEnv$checkedListNames <- checkedListNames
  sessionEnv$volcanoSelectedItems <- volcanoSelectedItems
  sessionEnv$reductionSelectedItems <- reductionSelectedItems
  sessionEnv$enrichmentResults <- enrichmentResults
  sessionEnv$arenaEdgelist <- arenaEdgelist
  sessionEnv$gprofilerResult <- gprofilerResult
  sessionEnv$gprofilerResults <- gprofilerResults
  sessionEnv$combinationResult <- combinationResult
  sessionEnv$enrichmentBackgroundSizes <- enrichmentBackgroundSizes
  sessionEnv$activeRuns <- activeRuns
  sessionEnv$runCounters <- runCounters
  sessionEnv$uniqueIdCounters <- uniqueIdCounters
  sessionEnv$STRINGNetworkData <- STRINGNetworkData
  sessionEnv$currentVariantResults <- currentVariantResults
  sessionEnv$currentTextminingResult <- currentTextminingResult
  sessionEnv$currentVolcano <- currentVolcano
  sessionEnv$volcanoPlotData <- volcanoPlotData
  sessionEnv$currentReduction <- currentReduction
  sessionEnv$currentUpsetMode <- currentUpsetMode
  sessionEnv$currentConversionResult <- currentConversionResult
  sessionEnv$currentOrthologyResult <- currentOrthologyResult
  sessionEnv$currentType_Tool <- currentType_Tool
  sessionEnv$plotOriginalData <- plotOriginalData
  sessionEnv$plotCurrentView <- plotCurrentView
  sessionEnv$plotRenderedData <- plotRenderedData
  sessionEnv$selectedTermIds <- selectedTermIds
  sessionEnv$selectedPairs <- selectedPairs
  sessionEnv$plotUpdateTracker <- plotUpdateTracker
  sessionEnv$currentSelectedToolTab <- currentSelectedToolTab
  sessionEnv$currentSelectedLiteratureTab <- currentSelectedLiteratureTab
  sessionEnv$outputRegistry <- outputRegistry
  sessionEnv$observerRegistry <- observerRegistry
  sessionEnv$input <- input
  sessionEnv$output <- output
  sessionEnv$session <- session

  # Store session environment in the session for access by other functions
  session$userData$flame <- sessionEnv

  # API handling
  observeEvent(session$clientData$url_search, {
    resolveAPI(session)
  })

  # Initialize server app
  initializeServerApp(session)

  # Welcome page observers
  observeEvent(input$link_to_fileinput, {
    updateTabItems(session, "sideBarId", selected = "file_handler")
  }, ignoreInit = TRUE)

  # Note: The rest of the observers would be added here
  # For now, this is a placeholder structure
  # The full implementation will be completed as we refactor

  message("FLAME server initialized")
}
