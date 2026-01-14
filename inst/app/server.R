# FLAME Server Function

function(input, output, session) {
  # Determine package root and source required files
  pkgRoot <- normalizePath(file.path(getwd(), "..", ".."))
  source(file.path(pkgRoot, "R", "aaa-helpers.R"), local = TRUE)

  # Source R6 infrastructure classes
  source(file.path(pkgRoot, "R", "infrastructure-config.R"), local = TRUE)

  # Source AnalyteList classes (in dependency order)
  source(file.path(pkgRoot, "R", "input-analytelist.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-analytelist-unranked.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-analytelist-ranked.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-analytelist-registry.R"), local = TRUE)

  # Source utility functions (used by input sessions)
  source(file.path(pkgRoot, "R", "func-extract.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-gsnpense.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-gconvert.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-gorth.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "func-string-network.R"), local = TRUE)

  # Source input session classes
  source(file.path(pkgRoot, "R", "input-session-base.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-list.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-volcano.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-reduction.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-snps.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "input-session-textmining.R"), local = TRUE)

  # Source list management session classes (depends on InputSession)
  source(file.path(pkgRoot, "R", "listmgmt-session-manager.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "listmgmt-session-setops.R"), local = TRUE)

  # Source utilities session classes
  source(file.path(pkgRoot, "R", "utilities-session-conversion.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "utilities-session-orthology.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "utilities-session-network.R"), local = TRUE)

  # Source configuration files (letter prefixes ensure correct load order)
  # config-a DELETED - shiny option moved to run_app.R
  source(file.path(pkgRoot, "R", "config-b-global_variables.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-c-enrichment_types.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "config-d-server_variables.R"), local = TRUE)
  # config-e DELETED - was only migration documentation
  # config-f DELETED - constants moved to their owning classes

  # Source core functions
  source(file.path(pkgRoot, "R", "aaa-utilities.R"), local = TRUE)
  # func-init.R DELETED - initializeDatasources() moved to config-d, hide calls inlined
  # func-reset.R DELETED - was only comments
  # func-runs.R DELETED - parseFullRunKey() removed (sessions own their identity)
  source(file.path(pkgRoot, "R", "core-tool_registry.R"), local = TRUE)
  # func-observers.R DELETED - all observers now handled by OutputSession classes

  # Source input functions
  # Note: input-main.R replaced by ListInputSession
  #       input-volcano.R replaced by VolcanoInputSession
  #       input-reduction.R replaced by ReductionInputSession
  #       input-snps.R replaced by SNPsInputSession
  #       input-text_mining.R replaced by TextMiningInputSession
  #       input-upset.R replaced by AnalyteListSetOperationsSession
  #       input-conversion.R replaced by ConversionSession/OrthologySession

  # Source enrichment session classes (in dependency order)
  source(file.path(pkgRoot, "R", "enrich-session-registry.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-session-base.R"), local = TRUE)
  # Output sessions must be sourced BEFORE ORAEnrichmentSession (which creates them)
  source(file.path(pkgRoot, "R", "output-session-base.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "output-session-barchart.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "output-session-scatter.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "output-session-dotplot.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "output-session-heatmap.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "output-session-network.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-session-ora.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-session-combination.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-controller.R"), local = TRUE)

  # Source enrichment functions
  source(file.path(pkgRoot, "R", "enrich-inputs_panel.R"), local = TRUE)
  # enrich-main.R REMOVED - functions moved to ORAEnrichmentSession and EnrichmentController
  source(file.path(pkgRoot, "R", "enrich-general.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-gprofiler.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-webgestalt.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-enrichr.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-string.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-panther.R"), local = TRUE)
  source(file.path(pkgRoot, "R", "enrich-genecodis.R"), local = TRUE)
  # enrich-combination.R REMOVED - replaced by CombinationSession

  # Note: All plot-*.R files REMOVED - now handled by OutputSession classes
  source(file.path(pkgRoot, "R", "func-arena.R"), local = TRUE)

  # Source remaining functions
  # Note: func-stringNetwork.R replaced by NetworkAnalysisSession
  # Note: func-conversion.R replaced by ConversionSession/OrthologySession
  # func-tabGeneration.R DELETED - UI generation moved to ORAEnrichmentSession.ui()
  # func-registry.R DELETED - OutputRegistry/ObserverRegistry no longer needed
  # Sessions now track their own outputs and observers

  # AnalyteList Registry for managing input lists (per-session)
  # Must be created in reactive context (server function)
  analyteListRegistry <- AnalyteListRegistry$new()

  # Enrichment Session Registry for managing enrichment runs (per-session)
  # Must be created in reactive context (server function)
  enrichmentSessionRegistry <- EnrichmentSessionRegistry$new()

  # AnalyteList Manager Session - manages sidebar and view panel for ALL lists
  analyteListManager <- AnalyteListManagerSession$new(
    ModuleIds$LISTMGMT_MANAGER, analyteListRegistry
  )
  analyteListManager$server(input, session)

  # List Input Session - manages Upload tab only (creating lists from text/files)
  listInputSession <- ListInputSession$new(ModuleIds$INPUT_LIST, analyteListRegistry)
  listInputSession$server(input, session)

  # Volcano Input Session - manages Volcano tab and plot panel
  volcanoInputSession <- VolcanoInputSession$new(ModuleIds$INPUT_VOLCANO, analyteListRegistry)
  volcanoInputSession$server(input, session)

  # Reduction Input Session - manages 2D Reduction tab and plot panel
  reductionInputSession <- ReductionInputSession$new(ModuleIds$INPUT_REDUCTION, analyteListRegistry)
  reductionInputSession$server(input, session)

  # SNPs Input Session - manages SNPs tab for SNP to gene conversion
  snpsInputSession <- SNPsInputSession$new(ModuleIds$INPUT_SNPS, analyteListRegistry)
  snpsInputSession$server(input, session)

  # Text Mining Input Session - manages Text-mining tab for extracting genes from text
  textMiningInputSession <- TextMiningInputSession$new(ModuleIds$INPUT_TEXTMINING, analyteListRegistry)
  textMiningInputSession$server(input, session)

  # AnalyteList Operations Session - manages UpSet plot panel for set operations
  upsetSession <- AnalyteListSetOperationsSession$new(
    ModuleIds$LISTMGMT_SETOPS,
    analyteListRegistry,
    analyteListManager
  )
  upsetSession$server(input, session)

  # Conversion Session - manages Gene ID Conversion tab (g:Convert)
  conversionSession <- ConversionSession$new(ModuleIds$UTILITIES_CONVERSION, analyteListRegistry)
  conversionSession$server(input, session)

  # Orthology Session - manages Orthology Search tab (g:Orth)
  orthologySession <- OrthologySession$new(ModuleIds$UTILITIES_ORTHOLOGY, analyteListRegistry)
  orthologySession$server(input, session)

  # Network Analysis Session - manages STRING Network tab
  networkSession <- NetworkAnalysisSession$new(ModuleIds$UTILITIES_NETWORK, analyteListRegistry)
  networkSession$server(input, session)

  # Enrichment Controller - manages enrichment form and creates run sessions
  enrichmentController <- EnrichmentController$new(
    ModuleIds$ENRICH_FORM,
    enrichmentSessionRegistry,
    analyteListRegistry
  )
  enrichmentController$server(session)

  # Combination Session - manages combination tab for comparing enrichment runs
  combinationSession <- CombinationSession$new(enrichmentSessionRegistry)
  combinationSession$server(input, output, session)

  # Connect EnrichmentController to CombinationSession (deferred to avoid circular deps)
  enrichmentController$setCombinationSession(combinationSession)

  # Clean up all session objects when the Shiny session ends
  # Order: dependent sessions first, then sessions they depend on
  session$onSessionEnded(function() {
    # upsetSession depends on analyteListManager, so clean it up first
    upsetSession$cleanup()
    listInputSession$cleanup()
    volcanoInputSession$cleanup()
    reductionInputSession$cleanup()
    snpsInputSession$cleanup()
    textMiningInputSession$cleanup()
    conversionSession$cleanup()
    orthologySession$cleanup()
    networkSession$cleanup()
    combinationSession$cleanup()
    enrichmentController$cleanup()
    analyteListManager$cleanup()
  })

  # Cross-module reactive updates: update selectors when registry changes

  # Helper to update selectInput while preserving current selection if still valid
  updateSelectPreserving <- function(inputId, choices) {
    currentSelection <- input[[inputId]]
    selected <- if (!is.null(currentSelection) && currentSelection %in% choices) {
      currentSelection
    } else {
      NULL
    }
    updateSelectInput(session, inputId, choices = choices, selected = selected)
  }

  # This replaces the imperative updateListBoxes() function with reactive updates
  observe({
    # Get list names reactively - this will re-run when registry changes
    listNames <- analyteListRegistry$getNamesReactive()

    # NOTE: userInputLists sync REMOVED - was never read, only written
    # All code now uses analyteListRegistry directly

    # Update enrichment form selectors (now handled by EnrichmentController)
    enrichmentController$updateFileChoices(listNames)

    # Update utility selectors (preserving current selection if still valid)
    updateSelectPreserving("selectUpset", listNames)
    # NOTE: gconvert_select handled by ConversionSession (namespaced as gconvert-select)
    # NOTE: gorth_select handled by OrthologySession (namespaced as gorth-select)
    # NOTE: string_network-select handled by NetworkAnalysisSession (namespaced)

    # UpSet tab visibility is now managed by AnalyteListSetOperationsSession
  })

  # Initialize server app - hide enrichment results panel until first run
  # (was func-init.R, now inlined)
  shinyjs::hide("functionalEnrichmentResultsPanel")
  hideTab(inputId = "toolTabsPanel", target = "Combination")

  # Welcome page observers
  observeEvent(input$link_to_fileinput, {
    updateTabItems(session, "sideBarId", selected = "file_handler")
  }, ignoreInit = TRUE)

  # INPUT observers for list management are now handled by ListInputSession
  # (see listInputSession$server() call above)

  # Text-mining observers are now handled by TextMiningInputSession
  # (see textMiningInputSession$server() call above)

  # UpSet observers are now handled by AnalyteListSetOperationsSession
  # (see upsetSession$server() call above)

  # SNPs observers are now handled by SNPsInputSession
  # (see snpsInputSession$server() call above)

  # Volcano observers are now handled by VolcanoInputSession
  # (see volcanoInputSession$server() call above)

  # 2D Reduction observers are now handled by ReductionInputSession
  # (see reductionInputSession$server() call above)

  # ENRICHMENT observers are now handled by EnrichmentController
  # (see enrichmentController$server() call above)
  # This includes: organism cascade, tool cascade, file change, background mode,
  # submit button, and clear all button.

  # Close individual run tab (via X button)
  # NOTE: This remains here because it's triggered by the tab close button,
  # not by the form. The clear all button is handled by EnrichmentController.
  observeEvent(input$closeRunTab, {
    runId <- input$closeRunTab
    fullRunKey <- paste("functional", runId, sep = "_")

    # Get toolName from session (not by parsing the key string)
    enrichSession <- enrichmentSessionRegistry$get(fullRunKey)
    toolName <- if (!is.null(enrichSession)) enrichSession$toolName else NULL

    enrichmentController$clearRun(fullRunKey)

    # Use registry for counting
    if (!is.null(toolName) && enrichmentSessionRegistry$countByTool(toolName) == 0) {
      enrichmentSessionRegistry$resetDisplayCounter(toolName)
    }
    if (enrichmentSessionRegistry$count() == 0) {
      shinyjs::hide("functionalEnrichmentResultsPanel")
      # Hide the clear all button using the namespaced ID
      shinyjs::hide(paste0(ModuleIds$ENRICH_FORM, "-enrichment_all_clear"))
    }
    # Update combination tab (refresh from remaining sessions)
    combinationSession$refresh()
    combinationSession$updateUI(session)
  }, ignoreInit = TRUE)

  # NOTE: Combination observers now handled by CombinationSession
  # (see combinationSession$server() call above)

  # NOTE: Barchart, Scatter, DotPlot observers now inside OutputSession classes
  # NOTE: Heatmap observers now inside HeatmapOutputSession classes

  # NOTE: STRING observers replaced by NetworkAnalysisSession (utilities-session-network.R)
  # NOTE: Conversion observers replaced by ConversionSession (utilities-session-conversion.R)
  # NOTE: Orthology observers replaced by OrthologySession (utilities-session-orthology.R)
}
