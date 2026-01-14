# =============================================================================
# FLAME Enrichment Controller
# =============================================================================
#
# Controls the enrichment workflow: form handling, session creation, and
# coordination. One-to-many pattern: creates multiple ORAEnrichmentSession
# instances from a single form.
#
# Responsibilities:
# - Form UI definition (enrichmentFormUI)
# - Input cascade logic (organism -> tools -> datasources -> namespaces -> metrics)
# - Form validation and submission handling
# - Run matching (exact match -> pulse, datasources differ -> update, no match -> new)
# - Session lifecycle coordination (create, update, clear)
# - UI orchestration (tab insertion, panel visibility)
#
# Pattern:
# - Static UI: enrichmentFormUI() standalone function
# - Controller: EnrichmentController R6 class with moduleServer()
#
# Dependencies:
# - enrich-session-registry.R (for EnrichmentSessionRegistry)
# - enrich-session-ora.R (for ORAEnrichmentSession)
# - input-analytelist-registry.R (for AnalyteListRegistry)
#
# =============================================================================

# =============================================================================
# STATIC UI FUNCTION
# =============================================================================

#' Enrichment Form UI
#'
#' Generates the enrichment form control panel.
#' Called from view-enrichment.R at app startup.
#'
#' @param id Character. Module namespace ID.
#' @return Shiny UI elements (the form control panel)
enrichmentFormUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("enrichment_file"),
          label = "1. Select list:",
          choices = NULL,
          width = "80%"
        ),
        shiny::selectizeInput(
          inputId = ns("enrichment_organism"),
          label = "2. Select organism:",
          choices = NULL,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          multiple = FALSE,
          width = "80%",
          options = list(placeholder = "Select an option or start typing...")
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("enrichment_background_choice"),
          label = "3. Select background:",
          choiceNames = c("Whole genome", "Custom background list"),
          choiceValues = c("genome", "user_list"),
          selected = "genome",
          justified = TRUE,
          width = "80%"
        ),
        shiny::tags$div(
          id = ns("enrichment_background_container"),
          style = "display:none",
          shiny::selectInput(
            inputId = ns("enrichment_background_list"),
            label = "Select background list:",
            choices = NULL,
            width = "80%"
          )
        )
      ),
      shiny::column(
        4,
        shinyWidgets::pickerInput(
          inputId = ns("enrichment_tool"),
          label = "4. Select enrichment tool:",
          choices = ENRICHMENT_TOOLS,
          selected = DEFAULT_TOOL,
          multiple = TRUE,
          width = "89.5%",
          options = list("actions-box" = TRUE)
        ),
        shinyWidgets::pickerInput(
          inputId = ns("enrichment_datasources"),
          label = "5. Select datasources:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          width = "89.5%",
          options = list("actions-box" = TRUE)
        ),
        shiny::selectInput(
          inputId = ns("enrichment_namespace"),
          label = "6. Select namespace conversion:",
          choices = NAMESPACES[["STRING"]],
          width = "80%"
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Default tool namespace conversions:
gProfiler: User Input
WebGestalt: Entrez Gene Accession
enrichR: Entrez Gene Name
STRING: ENSEMBL Protein ID
PANTHER: PANTHER Accession
GeneCodis: User Input"
              )
          )
      ),
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("enrichment_metric"),
          label = "7. Select significance metric:",
          choices = NULL,
          width = "80%"
        ),
        shiny::selectInput(
          inputId = ns("enrichment_threshold"),
          label = "8. Select significance threshold:",
          choices = list("0.05" = 0.05, "0.01" = 0.01, "No Threshold (return all results)" = 1),
          width = "80%"
        ),
        shiny::radioButtons(
          inputId = ns("enrichment_inputConversion"),
          label = "9. Select result namespace:",
          choices = c("Original input names", "Converted input names"),
          inline = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          inputId = ns("enrichment_run"),
          label = "Run analysis",
          shiny::icon("paper-plane"),
          class = "submit_button"
        ),
        shiny::actionButton(
          inputId = ns("enrichment_all_clear"),
          label = "Clear All",
          shiny::icon("broom"),
          style = "display:none"
        )
      )
    )
  )
}

# =============================================================================
# R6 CONTROLLER CLASS
# =============================================================================

#' Enrichment Controller Class
#'
#' Controls the enrichment workflow: handles form input, creates enrichment
#' sessions, and coordinates UI updates. Creates ORAEnrichmentSession
#' instances on submit and handles run matching.
#'
#' @section Lifecycle:
#' 1. Instantiated once per user session in server.R
#' 2. server() sets up form observers via moduleServer()
#' 3. On submit: creates ORAEnrichmentSession, executes, registers
#'
EnrichmentController <- R6::R6Class(
  "EnrichmentController",

  public = list(
    #' @field id Module namespace ID
    id = NULL,

    #' Initialize an EnrichmentController
    #' @param id Character. Module namespace ID.
    #' @param enrichmentRegistry EnrichmentSessionRegistry. Registry for sessions.
    #' @param analyteListRegistry AnalyteListRegistry. Registry for gene lists.
    #' @param combinationSession CombinationSession. Session for combination tab (optional, set later via setCombinationSession).
    initialize = function(id, enrichmentRegistry, analyteListRegistry,
                          combinationSession = NULL) {
      if (!inherits(enrichmentRegistry, "EnrichmentSessionRegistry")) {
        stop("enrichmentRegistry must be an EnrichmentSessionRegistry")
      }
      if (!inherits(analyteListRegistry, "AnalyteListRegistry")) {
        stop("analyteListRegistry must be an AnalyteListRegistry")
      }

      self$id <- id
      private$.enrichmentRegistry <- enrichmentRegistry
      private$.analyteListRegistry <- analyteListRegistry
      private$.combinationSession <- combinationSession
      private$.observers <- list()
    },

    #' Set the combination session (for deferred initialization)
    #' @param combinationSession CombinationSession instance
    setCombinationSession = function(combinationSession) {
      if (!inherits(combinationSession, "CombinationSession")) {
        stop("combinationSession must be a CombinationSession")
      }
      private$.combinationSession <- combinationSession
    },

    #' Set up server logic using moduleServer
    #'
    #' Registers observers for:
    #' - Organism selection cascade (-> tools)
    #' - Tool selection cascade (-> datasources, namespaces, metrics)
    #' - Background mode selection
    #' - Submit button
    #' - Clear all button
    #'
    #' @param parentSession Shiny session object (from parent/root)
    server = function(parentSession) {
      # Store parent session for UI operations outside moduleServer scope
      # (e.g., updateTabsetPanel, sendCustomMessage)
      private$.parentSession <- parentSession

      shiny::moduleServer(self$id, function(input, output, session) {
        # Store module session
        private$.moduleSession <- session

        # Initialize organism choices
        shiny::updateSelectizeInput(
          session, "enrichment_organism",
          choices = ORGANISMS$print_name,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          server = TRUE
        )

        # === ORGANISM CHANGE -> UPDATE TOOLS ===
        private$.observers$organism <- shiny::observeEvent(input$enrichment_organism, {
          private$updateAvailableTools(input, session)
        }, ignoreInit = TRUE)

        # === TOOL CHANGE -> UPDATE DATASOURCES, NAMESPACES, METRICS ===
        private$.observers$tool <- shiny::observeEvent(input$enrichment_tool, {
          private$updateAvailableDatasources(input, session)
          private$updateAvailableNamespaces(input, session)
          private$updateAvailableSignificanceMetrics(input, session)
        }, ignoreInit = TRUE)

        # === BACKGROUND MODE CHANGE ===
        private$.observers$backgroundMode <- shiny::observeEvent(input$enrichment_background_choice, {
          private$updateBackgroundMode(input$enrichment_background_choice, input, session)
        }, ignoreInit = TRUE)

        # === FILE SELECTION CHANGE -> UPDATE BACKGROUND LIST CHOICES ===
        private$.observers$file <- shiny::observeEvent(input$enrichment_file, {
          private$updateBackgroundListChoices(input, session)
        }, ignoreInit = TRUE)

        # === SUBMIT BUTTON ===
        private$.observers$submit <- shiny::observeEvent(input$enrichment_run, {
          private$handleSubmit(input, output, session)
        }, ignoreInit = TRUE)

        # === CLEAR ALL BUTTON ===
        private$.observers$clearAll <- shiny::observeEvent(input$enrichment_all_clear, {
          private$handleClearAll(input, session)
        }, ignoreInit = TRUE)
      })
    },

    #' Update file selection choices
    #' Called from outside when analyte lists change
    #' @param listNames Character vector. Available list names.
    updateFileChoices = function(listNames) {
      if (!is.null(private$.moduleSession)) {
        shiny::updateSelectInput(
          private$.moduleSession,
          "enrichment_file",
          choices = listNames,
          selected = if (length(listNames) > 0) listNames[1] else NULL
        )
        shiny::updateSelectInput(
          private$.moduleSession,
          "enrichment_background_list",
          choices = listNames
        )
      }
    },

    #' Clear an enrichment run
    #'
    #' Public method to clear a specific run. Called from server.R when
    #' the tab close button is clicked.
    #'
    #' @param fullRunKey Full run key (e.g., "functional_gProfiler_5")
    clearRun = function(fullRunKey) {
      private$clearRunInternal(fullRunKey)
    },

    #' Clean up observers
    cleanup = function() {
      for (obs in private$.observers) {
        if (!is.null(obs)) {
          tryCatch(obs$destroy(), error = function(e) NULL)
        }
      }
      private$.observers <- list()
    }
  ),

  private = list(
    # Registry references
    .enrichmentRegistry = NULL,
    .analyteListRegistry = NULL,

    # Session references
    .parentSession = NULL,  # Parent/root session for UI operations (updateTabsetPanel, etc.)
    .moduleSession = NULL,  # Module session from moduleServer()
    .combinationSession = NULL,  # CombinationSession for managing combination tab

    # List of observers for cleanup
    .observers = list(),

    # =========================================================================
    # CASCADE UPDATE METHODS
    # =========================================================================

    #' Update available tools based on selected organism
    updateAvailableTools = function(input, session) {
      inputOrganism <- input$enrichment_organism
      if (!is.null(inputOrganism) && inputOrganism != "") {
        # Clear first to trigger change event
        shinyWidgets::updatePickerInput(session, "enrichment_tool",
                                        choices = NULL, selected = character(0))

        taxid <- ORGANISMS[ORGANISMS$print_name == inputOrganism, ]$taxid
        organismShortName <- ORGANISMS[ORGANISMS$print_name == inputOrganism, ]$short_name
        choices <- names(which(sapply(TOOL_ORGANISMS, function(tool) taxid %in% tool)))
        selected <- choices[1]

        if (private$isSpecialOrganism(organismShortName)) {
          selected <- SPECIAL_PREFERRED_TOOL[[organismShortName]]
        }

        shinyWidgets::updatePickerInput(session, "enrichment_tool",
                                        choices = choices, selected = selected)
      }
    },

    #' Check if organism is a "special" organism with specific tool preferences
    isSpecialOrganism = function(organismShortName) {
      return(organismShortName %in% SPECIAL_ORGANISMS)
    },

    #' Update available datasources based on selected tools
    updateAvailableDatasources = function(input, session) {
      toolCapitalNames <- toupper(input$enrichment_tool)
      choices <- private$getChoicesUnion(toolCapitalNames, input)
      shinyWidgets::updatePickerInput(session, "enrichment_datasources",
                                      choices = choices, selected = DATASOURCES_DEFAULT_SELECTED)
    },

    #' Get union of datasources for selected tools
    getChoicesUnion = function(toolCapitalNames, input) {
      choices <- c()
      for (tool in toolCapitalNames) {
        prefix <- ""
        if (tool == "ENRICHR") {
          prefix <- private$getEnrichrVariablePrefix(input)
        }
        if (tool == "GENECODIS") {
          prefix <- private$getGeneCodisVariablePrefix(input)
        }
        choices <- c(choices, DATASOURCES[[paste0(prefix, tool)]])
      }
      choices <- private$filterDatasourcePrintChoices(unique(choices))
      return(choices)
    },

    #' Get enrichR variable prefix based on organism
    getEnrichrVariablePrefix = function(input) {
      organismShortName <- ORGANISMS[ORGANISMS$print_name == input$enrichment_organism, ]$short_name
      prefix <- switch(
        organismShortName,
        "mmusculus" = "MOUSE_",
        "drerio" = "FISH_",
        "dmelanogaster" = "FLY_",
        "scerevisiae" = "YEAST_",
        "celegans" = "WORM_",
        "btaurus" = "OX_",
        ""
      )
      if (is.null(prefix)) prefix <- ""
      return(prefix)
    },

    #' Get GeneCodis variable prefix based on organism
    getGeneCodisVariablePrefix = function(input) {
      organismShortName <- ORGANISMS[ORGANISMS$print_name == input$enrichment_organism, ]$short_name
      prefix <- switch(
        organismShortName,
        "athaliana" = "PLANT_",
        ""
      )
      if (is.null(prefix)) prefix <- ""
      return(prefix)
    },

    #' Filter datasource choices to only those available
    filterDatasourcePrintChoices = function(choices) {
      filtered_DATASOURCES_PRINT <- DATASOURCES_PRINT
      for (i in length(DATASOURCES_PRINT):1) {
        filtered_DATASOURCES_PRINT[[i]][!filtered_DATASOURCES_PRINT[[i]] %in% choices] <- NULL
        if (length(filtered_DATASOURCES_PRINT[[i]]) == 0)
          filtered_DATASOURCES_PRINT[[i]] <- NULL
      }
      return(filtered_DATASOURCES_PRINT)
    },

    #' Update available namespaces based on selected tools
    updateAvailableNamespaces = function(input, session) {
      organismShortName <- ORGANISMS[ORGANISMS$print_name == input$enrichment_organism, ]$short_name
      if (!is.null(organismShortName) && !is.na(organismShortName)) {
        toolCapitalNames <- toupper(input$enrichment_tool)
        choices <- private$getNamespaceChoices(toolCapitalNames, input, session)
        selected <- choices[1]
        if (private$isSpecialOrganism(organismShortName) &&
            all(toolCapitalNames == "ENRICHR")) {
          selected <- SPECIAL_PREFERRED_NAMESPACE[[organismShortName]]
        }
        shiny::updateSelectInput(session, "enrichment_namespace",
                                 choices = choices, selected = selected)
      } else {
        shinyjs::disable(session$ns("enrichment_namespace"))
        shiny::updateSelectInput(session, "enrichment_namespace",
                                 choices = c("ENSEMBL Protein ID" = "ENSP"),
                                 selected = "ENSP")
      }
    },

    #' Get namespace choices for selected tools
    getNamespaceChoices = function(toolCapitalNames, input, session) {
      if (length(toolCapitalNames) == 1) {
        shinyjs::enable(session$ns("enrichment_namespace"))
        prefix <- private$getNamespacePrefix(toolCapitalNames, input)
        choices <- NAMESPACES[[paste0(prefix, toolCapitalNames)]]

        # If tool doesn't have specific namespaces, use CORE namespaces
        if (is.null(choices)) {
          choices <- NAMESPACES[["CORE"]]
        }

        organismShortName <- ORGANISMS[ORGANISMS$print_name == input$enrichment_organism, ]$short_name
        if (private$isSpecialOrganism(organismShortName) && toolCapitalNames == "GPROFILER") {
          choices <- c(NAMESPACES[["SPECIAL"]][[organismShortName]], choices)
        }
      } else {
        shinyjs::disable(session$ns("enrichment_namespace"))
        choices <- DEFAULT_NAMESPACE_TEXT
      }
      return(choices)
    },

    #' Get namespace prefix for a tool
    getNamespacePrefix = function(toolCapitalNames, input) {
      prefix <- ""
      if (toolCapitalNames == "ENRICHR") {
        organismShortName <- ORGANISMS[ORGANISMS$print_name == input$enrichment_organism, ]$short_name
        prefix <- switch(
          organismShortName,
          "dmelanogaster" = "FLY_",
          "scerevisiae" = "YEAST_",
          ""
        )
        if (is.null(prefix)) prefix <- ""
      }
      return(prefix)
    },

    #' Update available significance metrics based on selected tools
    updateAvailableSignificanceMetrics = function(input, session) {
      toolCapitalNames <- toupper(input$enrichment_tool)
      options <- private$getAvailableSignificanceMetrics(toolCapitalNames, input, session)
      shiny::updateSelectInput(session, "enrichment_metric",
                               choices = options[["choices"]], selected = options[["selected"]])
    },

    #' Get available significance metrics for selected tools
    getAvailableSignificanceMetrics = function(toolCapitalNames, input, session) {
      if (length(toolCapitalNames) == 1) {
        shinyjs::enable(session$ns("enrichment_metric"))
        choices <- METRICS[[toolCapitalNames]]
        if (input$enrichment_background_choice == "genome") {
          selected <- DEFAULT_METRICS_GENOME[[toolCapitalNames[1]]]
        } else {
          selected <- DEFAULT_METRICS_USERBACKGROUND[[toolCapitalNames[1]]]
        }
      } else {
        shinyjs::disable(session$ns("enrichment_metric"))
        choices <- DEFAULT_METRIC_TEXT
        selected <- DEFAULT_METRIC_TEXT
      }
      return(list("choices" = choices, "selected" = selected))
    },

    #' Update UI based on background mode selection
    updateBackgroundMode = function(choice, input, session) {
      if (choice == "genome") {
        shinyjs::hide(session$ns("enrichment_background_container"))
        # Genome background: All enrichment tools are available
        shinyWidgets::updatePickerInput(session, "enrichment_tool",
                                        choices = ENRICHMENT_TOOLS, selected = DEFAULT_TOOL)
      } else {
        shinyjs::show(session$ns("enrichment_background_container"))
        # Custom background: Only tools that support user-provided background lists
        # enrichR is excluded because runEnrichr() does not accept user_reference parameter
        shinyWidgets::updatePickerInput(session, "enrichment_tool",
                                        choices = c("gProfiler", "WebGestalt", "STRING", "PANTHER", "GeneCodis"),
                                        selected = DEFAULT_TOOL)
      }
      private$updateAvailableSignificanceMetrics(input, session)
    },

    #' Update background list choices (exclude current input list)
    updateBackgroundListChoices = function(input, session) {
      userInputVal <- input$enrichment_file
      listNames <- private$.analyteListRegistry$getNames()
      backgroundChoices <- listNames[listNames != userInputVal]
      shiny::updateSelectInput(session, "enrichment_background_list", choices = backgroundChoices)
    },

    # =========================================================================
    # SUBMIT HANDLING
    # =========================================================================

    #' Handle form submission
    handleSubmit = function(input, output, session) {
      # Validate inputs
      if (!private$validateInputs(input)) {
        return(invisible(NULL))
      }

      # Get form values
      toolNames <- input$enrichment_tool
      organism <- ORGANISMS[ORGANISMS$print_name == input$enrichment_organism, ]$taxid
      geneListName <- input$enrichment_file
      datasources <- input$enrichment_datasources
      threshold <- input$enrichment_threshold
      namespace <- input$enrichment_namespace
      metric <- input$enrichment_metric
      backgroundMode <- input$enrichment_background_choice
      backgroundListName <- input$enrichment_background_list

      # Get config
      config <- getEnrichmentConfig("functional")

      # Process each selected tool
      lapply(toolNames, function(toolName) {
        # Capture current parameters for comparison
        currentParams <- private$captureRunParameters(input, toolName, organism)

        # Check for matching run
        matchResult <- private$findMatchingRun(toolName, currentParams)

        if (!is.null(matchResult$matchType) && matchResult$matchType == "exact") {
          # EXACT MATCH: Just select tab and pulse it (no API call)
          private$handleExactMatch(matchResult$session, config)
          return()
        }

        if (!is.null(matchResult$matchType) && matchResult$matchType == "datasources_differ") {
          # DATASOURCES DIFFER: Update existing tab with new datasources
          private$handleDatasourcesDiffer(matchResult$session, currentParams, config, input)
          return()
        }

        # NO MATCH: Create new session
        private$createNewSession(toolName, organism, geneListName, backgroundListName,
                                 currentParams, config, input)
      })

      # Update combination tab after UI is flushed
      if (config$supportsCombination && !is.null(private$.combinationSession)) {
        private$.parentSession$onFlushed(function() {
          private$.combinationSession$refresh()
          private$.combinationSession$updateUI(private$.parentSession)
        }, once = TRUE)
      }
    },

    #' Validate form inputs before submission
    validateInputs = function(input) {
      # Check if gene lists exist
      if (private$.analyteListRegistry$count() == 0) {
        renderWarning("Upload an input list first.")
        return(FALSE)
      }

      # Check organism
      if (is.null(input$enrichment_organism) || input$enrichment_organism == "") {
        renderWarning("Select an organism from the list.")
        return(FALSE)
      }

      # Check tool
      if (is.null(input$enrichment_tool) || length(input$enrichment_tool) == 0) {
        renderWarning("Select at least one enrichment tool.")
        return(FALSE)
      }

      # Check datasources
      if (is.null(input$enrichment_datasources) || length(input$enrichment_datasources) == 0) {
        renderWarning("Select at least one datasource.")
        return(FALSE)
      }

      return(TRUE)
    },

    #' Capture current run parameters for comparison
    captureRunParameters = function(input, toolName, organism) {
      # Resolve namespace default to tool-specific value
      rawNamespace <- input$enrichment_namespace
      resolvedNamespace <- if (rawNamespace == DEFAULT_NAMESPACE_TEXT) {
        getDefaultTargetNamespace(toolName, organism)
      } else {
        rawNamespace
      }

      # Resolve metric default to tool-specific value
      backgroundMode <- input$enrichment_background_choice
      rawMetric <- input$enrichment_metric
      resolvedMetric <- if (rawMetric == DEFAULT_METRIC_TEXT) {
        if (backgroundMode == "genome") {
          DEFAULT_METRICS_GENOME[[toupper(toolName)]]
        } else {
          DEFAULT_METRICS_USERBACKGROUND[[toupper(toolName)]]
        }
      } else {
        rawMetric
      }

      list(
        geneListName = input$enrichment_file,
        organism = input$enrichment_organism,
        threshold = input$enrichment_threshold,
        datasources = sort(input$enrichment_datasources),
        namespace = resolvedNamespace,
        backgroundMode = backgroundMode,
        backgroundList = if (backgroundMode == "genome") NULL else input$enrichment_background_list,
        metric = resolvedMetric
      )
    },

    #' Find a matching run in the registry
    findMatchingRun = function(toolName, currentParams) {
      sessions <- private$.enrichmentRegistry$queryByTool(toolName)

      for (existingSession in sessions) {
        existingParams <- existingSession$getParameters()
        diffs <- private$findParameterDifferences(existingParams, currentParams)

        if (length(diffs) == 0) {
          # Exact match - no differences
          return(list(matchType = "exact", session = existingSession))
        }

        if (identical(diffs, "datasources")) {
          # Only datasources differ - can update existing tab
          return(list(matchType = "datasources_differ", session = existingSession))
        }
      }

      return(list(matchType = NULL, session = NULL))
    },

    #' Handle exact match - pulse existing tab
    handleExactMatch = function(existingSession, config) {
      shiny::updateTabsetPanel(private$.parentSession, config$tabsetPanelId,
                               selected = existingSession$runId)
      private$.parentSession$sendCustomMessage("handler_pulseTab", existingSession$runId)
    },

    #' Handle datasources differ - update existing session
    handleDatasourcesDiffer = function(existingSession, newParams, config, input) {
      renderModal(paste0(
        "<h2>Please wait.</h2><br />",
        "<p>Updating ", config$label, " with ", existingSession$toolName,
        " (Run ", existingSession$displayNumber, ") - datasources changed.</p>"
      ))

      # Clear existing results (but keep OutputSessions alive - they have moduleServer bindings)
      existingSession$clearResults()

      # Replace tab content with new UI (handles output clearing, parameter update, and UI replacement)
      existingSession$updateContent(newParams, private$.parentSession$output, private$.parentSession)

      # Re-execute enrichment with new datasources
      existingSession$execute()

      # Check for results
      if (!existingSession$hasResults()) {
        renderWarning(paste0(
          "Functional enrichment with ", existingSession$toolName,
          " (Run ", existingSession$displayNumber, ") could not return any valid results with new datasources."))
        removeModal()
        return()
      }

      # Re-render display elements that depend on results
      # (Parameters show datasources from results, no-hit genes depend on result hits)
      local({
        sessionForUI <- existingSession
        parentOutput <- private$.parentSession$output
        # Determine check list based on rollback setting
        rollbackChoice <- input$enrichment_inputConversion
        noHitCheckList <- if (rollbackChoice == "Original input names") {
          sessionForUI$rollbackNames()
        } else {
          sessionForUI$getConvertedIds()
        }
        listName <- newParams$geneListName

        private$.parentSession$onFlushed(function() {
          # Update parameters (datasources display changes)
          sessionForUI$printParameters(listName)
          # Update no-hit genes (different results = different hits)
          sessionForUI$printNoHitGenes(noHitCheckList)
          # Render result tables
          sessionForUI$renderResultsTables(parentOutput)
          # Refresh output sessions (clear state, update controls) - NO destroy/recreate
          # This keeps moduleServer bindings intact and avoids Shiny rebinding issues
          sessionForUI$refreshOutputSessions()
          # NOTE: updatePlotControlPanelsForRun() REMOVED - OutputSessions update their own controls
        }, once = TRUE)
      })

      # Select the tab and pulse it
      shiny::updateTabsetPanel(private$.parentSession, config$tabsetPanelId,
                               selected = existingSession$runId)
      private$.parentSession$sendCustomMessage("handler_pulseTab", existingSession$runId)

      removeModal()
    },

    #' Create a new enrichment session
    createNewSession = function(toolName, organism, geneListName, backgroundListName,
                                params, config, input) {
      # Get gene list from registry
      geneList <- private$.analyteListRegistry$get(geneListName)
      if (is.null(geneList)) {
        renderWarning("Selected gene list not found.")
        return()
      }
      userList <- geneList$getIds()

      # Get background list if specified
      backgroundList <- NULL
      if (params$backgroundMode != "genome" && !is.null(backgroundListName)) {
        bgList <- private$.analyteListRegistry$get(backgroundListName)
        if (!is.null(bgList)) {
          backgroundList <- bgList$getIds()
        }
      }

      # Create analyte list objects
      inputAnalyteList <- UnrankedAnalyteList$new(
        name = geneListName,
        analyteType = AnalyteType$GENE,
        ids = userList
      )

      backgroundAnalyteList <- if (!is.null(backgroundList)) {
        UnrankedAnalyteList$new(
          name = backgroundListName,
          analyteType = AnalyteType$GENE,
          ids = backgroundList
        )
      } else {
        NULL
      }

      # Generate IDs
      uniqueId <- private$.enrichmentRegistry$getNextUniqueId()
      displayNumber <- private$.enrichmentRegistry$generateDisplayNumber(toolName)
      sessionId <- paste("functional", toolName, uniqueId, sep = "_")
      runId <- paste0(toolName, "_", uniqueId)

      # Create ORAEnrichmentSession
      enrichSession <- ORAEnrichmentSession$new(
        id = sessionId,
        runId = runId,
        uniqueId = uniqueId,
        displayNumber = displayNumber,
        toolName = toolName,
        organism = organism,
        input = inputAnalyteList,
        background = backgroundAnalyteList,
        parameters = params
      )

      renderModal(paste0(
        "<h2>Please wait.</h2><br />",
        "<p>Executing ", config$label, " with ", toolName,
        " (Run ", displayNumber, ").</p>"
      ))

      # Execute enrichment
      enrichSession$execute()

      # Check for results
      if (!enrichSession$hasResults()) {
        renderWarning(paste0(
          "Functional enrichment with ", toolName,
          " (Run ", displayNumber, ") could not return any valid results."))
        removeModal()
        return()
      }

      # Register in session registry
      private$.enrichmentRegistry$add(enrichSession)

      # Handle UI insertion and printing
      private$handleNewSessionUI(enrichSession, config, geneListName, input)

      removeModal()
    },

    #' Handle UI for a new session
    handleNewSessionUI = function(enrichSession, config, listName, input) {
      # Insert dynamic tab for this session (session owns its UI)
      private$insertTab(enrichSession, config)

      # No longer need registerOutputsForRun() - session tracks its own outputs

      # Show the results panel if this is the first run
      if (private$.enrichmentRegistry$count() == 1) {
        shinyjs::show(id = config$resultsPanelId, asis = TRUE)
        shinyjs::show("enrichment_all_clear")
        # Ensure Combination tab stays hidden initially
        if (config$supportsCombination) {
          shiny::hideTab(inputId = config$tabsetPanelId, target = "Combination",
                         session = private$.parentSession)
        }
      }

      # Create output sessions AFTER UI is flushed
      local({
        sessionForOutputs <- enrichSession
        private$.parentSession$onFlushed(function() {
          # Create output sessions (uses moduleServer internally)
          sessionForOutputs$createOutputSessions()
          # NOTE: registerObserversForRun() REMOVED - OutputSessions handle all observers now
        }, once = TRUE)
      })

      # Handle namespace rollback
      rollbackChoice <- input$enrichment_inputConversion
      if (rollbackChoice == "Original input names") {
        noHitGenesCheckList <- enrichSession$rollbackNames()
      } else {
        noHitGenesCheckList <- enrichSession$getConvertedIds()
      }

      # Print parameters - MUST wait for UI flush since tab was just inserted
      local({
        sessionForParams <- enrichSession
        listNameForParams <- listName
        private$.parentSession$onFlushed(function() {
          sessionForParams$printParameters(listNameForParams)
        }, once = TRUE)
      })

      # NOTE: hideSourceTabs() removed - tabs are now created only for selected datasources

      # Print conversion tables and unconverted genes
      namespace <- input$enrichment_namespace
      if (namespace != "USERINPUT") {
        local({
          sessionForConv <- enrichSession
          private$.parentSession$onFlushed(function() {
            shinyjs::show(paste(sessionForConv$id, "conversionBoxes", sep = "_"))
            sessionForConv$printUnconvertedGenes()
            sessionForConv$printConversionTables()
          }, once = TRUE)
        })
      }

      # Print no-hit genes and result tables - MUST wait for UI flush since tab was just inserted
      local({
        sessionForTables <- enrichSession
        noHitCheckList <- noHitGenesCheckList
        parentOutput <- private$.parentSession$output
        private$.parentSession$onFlushed(function() {
          sessionForTables$printNoHitGenes(noHitCheckList)
          sessionForTables$renderResultsTables(parentOutput)
        }, once = TRUE)
      })

      # NOTE: updatePlotControlPanelsForRun() REMOVED - OutputSessions update their own controls
      # when created via createOutputSessions() which calls their server() methods

      # Select the new tab
      shiny::updateTabsetPanel(private$.parentSession, config$tabsetPanelId,
                               selected = enrichSession$runId)

      # Pulse the tab
      private$.parentSession$sendCustomMessage("handler_pulseTab", enrichSession$runId)
    },

    #' Handle Clear All button
    handleClearAll = function(input, session) {
      # Reset combination session
      if (!is.null(private$.combinationSession)) {
        private$.combinationSession$reset(private$.parentSession)
      }

      # Clear ALL active functional runs
      allSessions <- private$.enrichmentRegistry$getAll()
      for (fullRunKey in names(allSessions)) {
        if (startsWith(fullRunKey, "functional_")) {
          private$clearRunInternal(fullRunKey)
        }
      }

      # Reset display counters for all tools
      for (tool in ENRICHMENT_TOOLS) {
        if (private$.enrichmentRegistry$countByTool(tool) == 0) {
          private$.enrichmentRegistry$resetDisplayCounter(tool)
        }
      }

      # Hide the results panel and clear all button
      shinyjs::hide("functionalEnrichmentResultsPanel")
      shinyjs::hide(session$ns("enrichment_all_clear"))

      # Update combination tab (will hide since no sessions remain)
      if (!is.null(private$.combinationSession)) {
        private$.combinationSession$refresh()
        private$.combinationSession$updateUI(private$.parentSession)
      }
    },

    # =========================================================================
    # TAB MANAGEMENT
    # =========================================================================

    #' Insert enrichment tab for a session
    #'
    #' Creates and inserts the results tab for an enrichment session.
    #'
    #' @param enrichSession ORAEnrichmentSession
    #' @param config Enrichment config from getEnrichmentConfig()
    insertTab = function(enrichSession, config) {
      runId <- enrichSession$runId
      tabTitle <- paste0(enrichSession$toolName, " (", enrichSession$displayNumber, ")")

      # Generate tab content - session owns its UI
      tabContent <- enrichSession$ui()

      # Create tab title with close button
      tabTitleHtml <- shiny::tags$span(
        tabTitle,
        shiny::tags$button(
          class = "close-run-tab",
          type = "button",
          onclick = paste0("event.stopPropagation(); Shiny.setInputValue('",
                           config$closeEvent, "', '", runId, "', {priority: 'event'});"),
          shiny::icon("times")
        )
      )

      # Insert the tab - must use parentSession since toolTabsPanel is in parent UI
      shiny::insertTab(
        inputId = config$tabsetPanelId,
        tab = shiny::tabPanel(
          title = tabTitleHtml,
          value = runId,
          tabContent
        ),
        select = TRUE,
        session = private$.parentSession
      )
    },

    #' Clear an enrichment run completely (internal implementation)
    #'
    #' Clears all data, destroys observers, removes UI tab.
    #'
    #' @param fullRunKey Full run key (e.g., "functional_gProfiler_5")
    clearRunInternal = function(fullRunKey) {
      # Get session from registry - session owns all identity information
      enrichSession <- private$.enrichmentRegistry$get(fullRunKey)
      if (is.null(enrichSession)) return()

      # Get identity from session (not by parsing the key string)
      config <- getEnrichmentConfig(enrichSession$enrichmentType)
      runId <- enrichSession$runId

      # Session handles its own cleanup
      enrichSession$cleanup(private$.parentSession$output)

      # Remove the tab (using config for panel ID)
      shiny::removeTab(inputId = config$tabsetPanelId, target = runId,
                       session = private$.parentSession)

      # Remove from enrichment session registry
      private$.enrichmentRegistry$remove(fullRunKey)
    },

    # =========================================================================
    # PARAMETER COMPARISON
    # =========================================================================

    #' Find which parameters differ between two parameter sets
    #'
    #' Returns a character vector of parameter names that differ.
    #' Empty vector = exact match. Single "datasources" = can update existing tab.
    #'
    #' @param params1 First parameter set
    #' @param params2 Second parameter set
    #' @return Character vector of differing parameter names
    findParameterDifferences = function(params1, params2) {
      if (is.null(params1) || is.null(params2)) return("null_params")

      diffs <- character(0)

      if (!identical(params1$geneListName, params2$geneListName))
        diffs <- c(diffs, "geneListName")
      if (!identical(params1$organism, params2$organism))
        diffs <- c(diffs, "organism")
      if (!identical(params1$threshold, params2$threshold))
        diffs <- c(diffs, "threshold")
      if (!identical(sort(params1$datasources), sort(params2$datasources)))
        diffs <- c(diffs, "datasources")
      if (!identical(params1$namespace, params2$namespace))
        diffs <- c(diffs, "namespace")
      if (!identical(params1$backgroundMode, params2$backgroundMode))
        diffs <- c(diffs, "backgroundMode")
      if (!identical(params1$backgroundList, params2$backgroundList))
        diffs <- c(diffs, "backgroundList")
      if (!identical(params1$metric, params2$metric))
        diffs <- c(diffs, "metric")

      return(diffs)
    }
  )
)
