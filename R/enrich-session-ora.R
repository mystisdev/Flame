# =============================================================================
# FLAME ORA Enrichment Session
# =============================================================================
#
# Concrete implementation of EnrichmentSession for Over-Representation Analysis.
# Each instance represents one ORA enrichment run and owns ALL its data.
#
# Lifecycle:
# 1. Created by EnrichmentController on submit
# 2. execute() runs full enrichment flow:
#    - Gene conversion
#    - Strategy execution via toolRegistry
#    - Result transformation
#    - DB link attachment
# 3. EnrichmentController$insertTab() adds tab to results panel
# 4. server() sets up observers for plots/tables
# 5. cleanup() destroys observers when tab closes
#
# Data Ownership (Option B - no globals):
# - Results stored in private$.results (NOT enrichmentResults global)
# - Background size in private$.backgroundSize
# - Arena edgelists in private$.arenaEdgelists
#
# Dependencies:
# - enrich-session-base.R (for EnrichmentSession)
# - core-tool_registry.R (for toolRegistry)
# - func-tabGeneration.R (for UI generation, temporarily)
#
# =============================================================================

#' ORA Enrichment Session Class
#'
#' Handles Over-Representation Analysis (ORA) enrichment runs.
#' Owns all its data - results are stored in session, NOT globals.
#'
#' @section Results Storage:
#' Results are stored in private$.results (NOT globals).
#' Access via getResults(), hasResults().
#'
ORAEnrichmentSession <- R6::R6Class(
 "ORAEnrichmentSession",
  inherit = EnrichmentSession,

  public = list(
    #' @field background Optional background gene list (AnalyteList)
    background = NULL,

    #' Initialize an ORA Enrichment Session
    #' @param id Character. Unique session ID.
    #' @param runId Character. Run ID for tab panel.
    #' @param uniqueId Integer. Unique counter for Shiny IDs.
    #' @param displayNumber Integer. Display number for tab title.
    #' @param toolName Character. Tool name.
    #' @param organism Integer. Organism taxid.
    #' @param input AnalyteList. Input gene list.
    #' @param background AnalyteList. Optional background gene list.
    #' @param parameters List. Enrichment parameters (datasources, threshold, etc.).
    initialize = function(id, runId, uniqueId, displayNumber, toolName, organism,
                          input, background = NULL, parameters = list()) {
      super$initialize(
        id = id,
        runId = runId,
        uniqueId = uniqueId,
        displayNumber = displayNumber,
        toolName = toolName,
        organism = organism,
        input = input,
        parameters = parameters
      )
      self$background <- background
      private$.arenaEdgelists <- list()
    },

    #' Execute the ORA enrichment
    #'
    #' Full enrichment flow:
    #' 1. Convert gene IDs to tool-specific format
    #' 2. Call toolRegistry strategy
    #' 3. Transform results (add columns, format)
    #' 4. Sort by -log10Pvalue
    #' 5. Attach database links
    #' 6. Store everything in private fields
    #'
    #' @return Invisible self for chaining
    execute = function() {
      # Get parameters
      params <- private$.parameters
      namespace <- params$namespace

      # Step 1: Convert gene IDs to tool-specific format
      geneIds <- private$.input$getIds()
      inputConversionTable <- private$convertGeneIds(geneIds, namespace)

      if (is.null(inputConversionTable) || nrow(inputConversionTable) == 0) {
        warning(sprintf("Gene conversion failed for %s", self$toolName))
        return(invisible(self))
      }

      # Store conversion table and namespace
      private$.conversionTable <- inputConversionTable
      resolvedNamespace <- attr(inputConversionTable, "namespace")
      private$.parameters$namespace <- if (!is.null(resolvedNamespace)) resolvedNamespace else namespace

      # Convert background if provided
      backgroundConversionTable <- NULL
      convertedBackgroundIds <- NULL
      if (!is.null(self$background)) {
        backgroundIds <- self$background$getIds()
        backgroundConversionTable <- private$convertGeneIds(backgroundIds, namespace)
        if (!is.null(backgroundConversionTable)) {
          convertedBackgroundIds <- backgroundConversionTable$target
          private$.backgroundConversionTable <- backgroundConversionTable
        }
      }

      # Step 2: Call toolRegistry strategy
      convertedInputIds <- inputConversionTable$target

      # Check if we have a strategy
      if (!toolRegistry$hasStrategy("functional", self$toolName)) {
        warning(sprintf("No strategy registered for %s", self$toolName))
        return(invisible(self))
      }

      strategyResult <- tryCatch({
        strategy <- toolRegistry$get("functional", self$toolName)
        strategy$run(
          convertedInputIds,
          self$organism,
          convertedBackgroundIds,
          params
        )
      }, error = function(e) {
        warning(sprintf("Enrichment failed for %s: %s", self$toolName, e$message))
        NULL
      })

      if (is.null(strategyResult) || is.null(strategyResult$result) ||
          nrow(strategyResult$result) == 0) {
        return(invisible(self))
      }

      private$.backgroundSize <- strategyResult$backgroundSize

      # Step 3: Transform results
      results <- transformEnrichmentResultTable(strategyResult$result)

      # Step 4: Sort by -log10Pvalue descending
      results <- results[order(-results$`-log10Pvalue`), ]

      # Store Term_ID_noLinks before link attachment
      if (is.null(results$Term_ID_noLinks)) {
        results$Term_ID_noLinks <- results$Term_ID
      }

      # Step 5: Attach database links
      results <- private$attachDBLinks(results)

      # Step 6: Store final results
      private$.results <- results

      invisible(self)
    },

    #' Rollback converted names to original input symbols
    #'
    #' Converts Positive Hits from tool-specific IDs back to original gene symbols.
    #' Called when user selects "Original input names" option.
    #'
    #' @return Character vector of IDs to use for no-hit calculation
    rollbackNames = function() {
      if (!self$hasResults()) return(character(0))

      conversionTable <- private$.conversionTable
      if (is.null(conversionTable)) return(character(0))

      results <- private$.results

      # Check if all Positive Hits are empty (safety check)
      allEmpty <- all(results$`Positive Hits` == "" | is.na(results$`Positive Hits`))
      if (allEmpty) {
        return(conversionTable$input)
      }

      # Rollback the names
      results <- tidyr::separate_rows(results, `Positive Hits`, sep = ",\\s*")
      results <- merge(results, conversionTable,
                       by.x = "Positive Hits", by.y = "target")
      results <- results[, !(names(results) %in% c("Positive Hits", "name"))]
      colnames(results)[match("input", colnames(results))] <- "Positive Hits"
      results <- results %>%
        dplyr::group_by(Term_ID) %>%
        dplyr::mutate(`Positive Hits` = paste(`Positive Hits`, collapse = ","))

      # Reorder columns to match expected structure (required for ALL tools)
      # After merge, column order changes - Positive Hits ends up at the end
      # Must restore to: Source, Term_ID, Function, P-value, -log10Pvalue,
      #                  Term Size, Query size, Intersection Size, Enrichment Score %,
      #                  Positive Hits, Term_ID_noLinks
      expectedCols <- c("Source", "Term_ID", "Function", "P-value",
                        "-log10Pvalue", "Term Size", "Query size",
                        "Intersection Size", "Enrichment Score %",
                        "Positive Hits", "Term_ID_noLinks")
      # Only include columns that exist
      existingCols <- expectedCols[expectedCols %in% colnames(results)]
      results <- results[, existingCols]
      results <- as.data.frame(dplyr::distinct(results))

      # Update stored results
      private$.results <- results

      # Return original input IDs for no-hit calculation
      return(conversionTable$input)
    },

    #' Get IDs for no-hit calculation without rollback
    #' @return Character vector of converted IDs
    getConvertedIds = function() {
      conversionTable <- private$.conversionTable
      if (is.null(conversionTable)) return(character(0))
      return(conversionTable$target)
    },

    #' Generate the complete tab UI for this session
    #'
    #' Creates the entire tab content including Results panel (datasource tabs,
    #' conversion boxes) and Plots panel (containers for OutputSessions).
    #' Tracks all output IDs in private$.outputIds for cleanup.
    #'
    #' @return Shiny UI tagList
    ui = function() {
      runKey <- self$id

      # Initialize output ID tracking
      private$.outputIds <- list(
        tables = character(),
        text = character(),
        conversion = character()
      )

      # Wrapper div stays as anchor for content replacement
      shiny::tags$div(
        id = paste0(runKey, "_content_wrapper"),
        shiny::tags$div(
          id = paste0(runKey, "_content"),
          shiny::tags$br(),
          private$generateParametersBox(),
          shiny::tabsetPanel(
            private$generateResultsPanel(),
            private$generatePlotsPanel()
          )
        )
      )
    },

    #' Clear all Shiny outputs owned by this session
    #'
    #' Clears result tables, text outputs, and conversion tables.
    #' Called by cleanup() and when datasources change.
    #'
    #' @param output Shiny output object
    clearOutputs = function(output) {
      # Clear result tables
      for (tableId in private$.outputIds$tables) {
        tryCatch({
          output[[tableId]] <- DT::renderDataTable(NULL)
        }, error = function(e) NULL)
      }

      # Clear text outputs
      for (textId in private$.outputIds$text) {
        tryCatch({
          output[[textId]] <- shiny::renderText("")
        }, error = function(e) NULL)
      }

      # Clear conversion tables
      for (convId in private$.outputIds$conversion) {
        tryCatch({
          output[[convId]] <- DT::renderDataTable(NULL)
        }, error = function(e) NULL)
      }

      invisible(self)
    },

    #' Update content when datasources change
    #'
    #' Replaces the entire tab content with new UI reflecting new datasources.
    #' Used when user re-runs enrichment with same parameters except datasources.
    #'
    #' @param newParams List. New parameters including updated datasources.
    #' @param output Shiny output object.
    #' @param parentSession Shiny session object.
    updateContent = function(newParams, output, parentSession) {
      # 1. Clear old outputs
      self$clearOutputs(output)

      # 2. Reset output ID tracking (old IDs are invalid after content removal)
      private$.outputIds <- list(
        tables = character(),
        text = character(),
        conversion = character()
      )

      # 3. Update parameters (datasources changed)
      self$updateParameters(newParams)

      # 4. Remove old content
      shiny::removeUI(
        selector = paste0("#", self$id, "_content"),
        session = parentSession
      )

      # 5. Generate and insert new content
      newContent <- shiny::tags$div(
        id = paste0(self$id, "_content"),
        shiny::tags$br(),
        private$generateParametersBox(),
        shiny::tabsetPanel(
          private$generateResultsPanel(),
          private$generatePlotsPanel()
        )
      )

      shiny::insertUI(
        selector = paste0("#", self$id, "_content_wrapper"),
        where = "afterBegin",
        ui = newContent,
        session = parentSession
      )

      invisible(self)
    },

    #' Set up server logic
    #'
    #' Registers observers for plot generation, table rendering, etc.
    #'
    #' @param input Shiny input object
    #' @param output Shiny output object
    #' @param parentSession Shiny session object
    server = function(input, output, parentSession) {
      # Use moduleServer for namespacing
      shiny::moduleServer(self$id, function(input, output, session) {
        # Store session references
        private$.moduleSession <- session

        # For Part 2, delegate to existing observer registration
        # This will be refactored in Part 3 (OutputSessions)
        # NOTE: Observers are currently registered externally in func-observers.R
        # We'll migrate them here incrementally

        # Register close tab observer
        private$.observers$close <- shiny::observeEvent(
          input[[paste0("close_", self$runId)]],
          {
            # Trigger cleanup - will be called by EnrichmentController
            # For now, just log
            message(sprintf("Close requested for session: %s", self$id))
          },
          ignoreInit = TRUE
        )
      })
    },

    #' Render results tables for ORA paradigm
    #'
    #' Renders the "All" table and per-datasource tables for ORA enrichment.
    #' ORA-specific: uses P-value, Enrichment Score, Positive Hits columns.
    #'
    #' @param output Shiny output object to render tables into
    renderResultsTables = function(output) {
      results <- private$.results
      runKey <- self$id

      if (is.null(results) || nrow(results) == 0) {
        return(invisible(self))
      }

      # Helper function to render a single results table
      renderSingleTable <- function(shinyOutputId, data, datasource) {
        if (nrow(data) == 0) return()

        # Format Positive Hits with spaces after commas
        data$`Positive Hits` <- gsub(",", ", ", data$`Positive Hits`)

        # Show the source tab (use session's own method)
        self$showSourceTab(datasource)

        # ORA-specific table parameters
        caption <- "Enrichment Results"
        fileName <- paste(runKey, datasource, sep = "_")
        mode <- "Positive Hits"
        hiddenColumns <- c(10, 11)  # Positive Hits detail, Term_ID_noLinks
        expandableColumn <- 10

        # Convert Source to factor for dropdown filtering
        data$Source <- as.factor(data$Source)

        # Render using internal method (encapsulated from func-render.R)
        private$renderResultsTableInternal(
          output = output,
          shinyOutputId = shinyOutputId,
          data = data,
          caption = caption,
          fileName = fileName,
          mode = mode,
          hiddenColumns = hiddenColumns,
          expandableColumn = expandableColumn,
          filter = 'top'
        )
      }

      # Render "All" table
      shinyOutputId <- paste(runKey, "table_all", sep = "_")
      renderSingleTable(shinyOutputId, results, "all")

      # Render per-datasource tables
      params <- private$.parameters
      datasources <- params$datasources
      lapply(datasources, function(datasource) {
        partialId <- as.character(TAB_NAMES[datasource])
        shinyOutputId <- paste(runKey, "table", partialId, sep = "_")
        pattern <- paste0("^", datasource, "$")
        matches <- grepl(pattern, results$Source)
        filteredResults <- results[matches, ]

        if (nrow(filteredResults) > 0) {
          renderSingleTable(shinyOutputId, filteredResults, datasource)
        }
      })

      invisible(self)
    },

    #' Create output sessions for plots
    #'
    #' Creates OutputSessions for each plot type.
    #' Uses proper Shiny module pattern - each OutputSession owns its UI and server.
    #' Call this after the tab is inserted (the container div exists).
    #' Only called for NEW sessions - for datasources-differ, use refreshOutputSessions().
    createOutputSessions = function() {
      # Helper to insert UI into container
      insertIntoContainer <- function(containerId, sessionUI) {
        shiny::insertUI(
          selector = paste0("#", containerId),
          where = "afterBegin",
          ui = sessionUI,
          immediate = TRUE
        )
      }

      # Create BarchartOutputSession
      private$.outputSessions$barchart <- BarchartOutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "barchart_container", sep = "_"),
                          private$.outputSessions$barchart$ui())
      private$.outputSessions$barchart$server()

      # Create ScatterOutputSession
      private$.outputSessions$scatter <- ScatterOutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "scatterPlot_container", sep = "_"),
                          private$.outputSessions$scatter$ui())
      private$.outputSessions$scatter$server()

      # Create DotPlotOutputSession
      private$.outputSessions$dotplot <- DotPlotOutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "dotPlot_container", sep = "_"),
                          private$.outputSessions$dotplot$ui())
      private$.outputSessions$dotplot$server()

      # Create Heatmap1OutputSession (Function vs Gene)
      private$.outputSessions$heatmap1 <- Heatmap1OutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "heatmap1_container", sep = "_"),
                          private$.outputSessions$heatmap1$ui())
      private$.outputSessions$heatmap1$server()

      # Create Heatmap2OutputSession (Function vs Function)
      private$.outputSessions$heatmap2 <- Heatmap2OutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "heatmap2_container", sep = "_"),
                          private$.outputSessions$heatmap2$ui())
      private$.outputSessions$heatmap2$server()

      # Create Heatmap3OutputSession (Gene vs Gene)
      private$.outputSessions$heatmap3 <- Heatmap3OutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "heatmap3_container", sep = "_"),
                          private$.outputSessions$heatmap3$ui())
      private$.outputSessions$heatmap3$server()

      # Create Network1OutputSession (Function vs Gene)
      private$.outputSessions$network1 <- Network1OutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "network1_container", sep = "_"),
                          private$.outputSessions$network1$ui())
      private$.outputSessions$network1$server()

      # Create Network2OutputSession (Function vs Function)
      private$.outputSessions$network2 <- Network2OutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "network2_container", sep = "_"),
                          private$.outputSessions$network2$ui())
      private$.outputSessions$network2$server()

      # Create Network3OutputSession (Gene vs Gene)
      private$.outputSessions$network3 <- Network3OutputSession$new(
        runKey = self$id,
        enrichSession = self
      )
      insertIntoContainer(paste(self$id, "network3_container", sep = "_"),
                          private$.outputSessions$network3$ui())
      private$.outputSessions$network3$server()
    },

    #' Get an output session by type
    #'
    #' @param type Character. Output type ("barchart", "scatter", etc.)
    #' @return OutputSession object or NULL
    getOutputSession = function(type) {
      private$.outputSessions[[type]]
    },

    #' Destroy all output sessions
    #'
    #' Cleans up output sessions without destroying the enrichment session itself.
    #' Used when datasources change - we destroy output sessions, re-execute,
    #' then recreate output sessions with fresh state.
    destroyOutputSessions = function() {
      for (session in private$.outputSessions) {
        if (!is.null(session)) {
          tryCatch(session$cleanup(), error = function(e) NULL)
        }
      }
      private$.outputSessions <- list()
    },

    #' Refresh all output sessions (keep alive, clear state)
    #'
    #' Used when datasources change. Unlike destroyOutputSessions(), this keeps
    #' the sessions alive with their moduleServer bindings intact. It just clears
    #' their state and rendered outputs, then updates their controls for new data.
    refreshOutputSessions = function() {
      for (session in private$.outputSessions) {
        if (!is.null(session)) {
          tryCatch({
            session$clearForRefresh()
            session$updateControls()
          }, error = function(e) {
            cat("[refreshOutputSessions] Error:", conditionMessage(e), "\n")
          })
        }
      }
    },

    #' Clean up all resources
    #'
    #' Destroys output sessions first, clears our outputs, then observers and state.
    #'
    #' @param output Shiny output object (optional, for clearing outputs)
    cleanup = function(output = NULL) {
      # 1. Clean up OutputSessions (plots) FIRST
      for (session in private$.outputSessions) {
        if (!is.null(session)) {
          tryCatch(session$cleanup(), error = function(e) NULL)
        }
      }
      private$.outputSessions <- list()

      # 2. Clear our own outputs (tables, text) if output provided
      if (!is.null(output)) {
        self$clearOutputs(output)
      }

      # 3. Call parent cleanup (observers, results, arenaEdgelists)
      super$cleanup()
    },

    #' Check if this session has a background list
    #' @return Logical
    hasBackground = function() {
      !is.null(self$background)
    },

    # =========================================================================
    # DISPLAY METHODS (Session owns its display)
    # =========================================================================

    #' Print run parameters to the UI
    #'
    #' Renders the parameters block showing run configuration.
    #' Session owns its parameters and knows how to display them.
    #'
    #' @param listName Character. Name of the input gene list.
    printParameters = function(listName) {
      bgSize <- self$getBackgroundSize()
      bgSizeDisplay <- if (is.null(bgSize)) "Genome-wide (tool default)" else bgSize
      params <- private$.parameters

      # Get background mode from params (stored during capture)
      bgMode <- if (!is.null(params$backgroundMode)) params$backgroundMode else "genome"

      # Get datasources from session results
      results <- private$.results
      datasourcesDisplay <- if (!is.null(results) && nrow(results) > 0) {
        paste(unique(results$Source), collapse = ", ")
      } else {
        paste(params$datasources, collapse = ", ")
      }

      parametersOutput <- paste0(
        "Run: ", self$toolName, " (", self$displayNumber, ")",
        "\nFile: ", listName,
        "\nOrganism: ", ORGANISMS[ORGANISMS$taxid == self$organism, ]$print_name,
        "\nBackground: ", bgMode,
        "\nBackground size (no. of genes): ", bgSizeDisplay,
        "\nDatasources: ", datasourcesDisplay,
        "\nNamespace: ", params$namespace,
        "\nSignificance metric: ", params$metric,
        "\nSignificance threshold: ", params$threshold
      )
      renderShinyText(paste(self$id, "enrichment_parameters", sep = "_"),
                      parametersOutput)
    },

    #' Print no-hit genes to the UI
    #'
    #' Finds and displays genes from checkList that were not found
    #' in any enriched term's Positive Hits.
    #'
    #' @param checkList Character vector. Gene IDs to check against results.
    printNoHitGenes = function(checkList) {
      results <- private$.results
      if (is.null(results) || nrow(results) == 0) {
        private$renderNoHitGenesInternal(checkList)
        return(invisible(self))
      }

      # Collect all genes from Positive Hits column
      allHitGenes <- paste(results$`Positive Hits`, collapse = ",")
      allHitGenes <- strsplit(allHitGenes, ",")[[1]]
      allHitGenes <- unique(allHitGenes)

      # Find genes not in any term
      noHitGenes <- checkList[!checkList %in% allHitGenes]
      private$renderNoHitGenesInternal(noHitGenes)
      invisible(self)
    },

    #' Print unconverted genes to the UI
    #'
    #' Displays genes that failed namespace conversion for both
    #' input list and optional background list.
    printUnconvertedGenes = function() {
      convTable <- private$.conversionTable
      bgConvTable <- private$.backgroundConversionTable
      origInputs <- private$.input$getIds()
      origBackground <- if (!is.null(self$background)) self$background$getIds() else NULL

      # Report unconverted input genes
      inputOutputId <- paste(self$id, "notConverted_input", sep = "_")
      unconvertedInputs <- origInputs[!origInputs %in% convTable$input]
      private$renderGeneReportInternal(
        outputId = inputOutputId,
        genes = unconvertedInputs,
        messageTemplate = "%d input item(s) could not be converted to the target namespace:\n%s"
      )

      # Report unconverted background genes (if provided)
      refOutputId <- paste(self$id, "notConverted_reference", sep = "_")
      refDivId <- paste(self$id, "notConverted_reference_div", sep = "_")

      if (!is.null(bgConvTable) && !is.null(origBackground)) {
        unconvertedBackground <- origBackground[!origBackground %in% bgConvTable$input]
        private$renderGeneReportInternal(
          outputId = refOutputId,
          genes = unconvertedBackground,
          messageTemplate = "%d reference background item(s) could not be converted to the target namespace:\n%s"
        )
        shinyjs::show(refDivId)
      } else {
        shinyjs::hide(refDivId)
      }
      invisible(self)
    },

    #' Print conversion tables to the UI
    #'
    #' Renders the gene conversion tables for input list and
    #' optional background list.
    printConversionTables = function() {
      inputConversionTable <- private$.conversionTable
      backgroundConversionTable <- private$.backgroundConversionTable

      # Render input list conversion table
      shinyOutputId <- paste(self$id, "conversionTable_input", sep = "_")
      fileName <- paste(self$id, "conversion_table", sep = "_")
      inputTableCopy <- inputConversionTable
      colnames(inputTableCopy) <- c("Input", "Target", "Name")
      renderShinyDataTable(shinyOutputId, inputTableCopy, fileName = fileName)

      # Handle reference background conversion table
      genomeDivId <- paste(self$id, "conversionTable_genome_div", sep = "_")
      refDivId <- paste(self$id, "conversionTable_reference_div", sep = "_")

      if (is.null(backgroundConversionTable)) {
        shinyjs::show(genomeDivId)
        shinyjs::hide(refDivId)
      } else {
        shinyjs::hide(genomeDivId)
        shinyOutputId <- paste(self$id, "conversionTable_reference", sep = "_")
        fileName <- paste(self$id, "conversion_table_reference", sep = "_")
        bgTableCopy <- backgroundConversionTable
        colnames(bgTableCopy) <- c("Input", "Target", "Name")
        renderShinyDataTable(shinyOutputId, bgTableCopy, fileName = fileName)
        shinyjs::show(refDivId)
      }
      invisible(self)
    },

    #' Show a specific datasource tab for this run
    #'
    #' @param datasource Character. Datasource code or "all"
    #' @param parentSession Shiny session for tab operations
    showSourceTab = function(datasource, parentSession = NULL) {
      sourcePanelId <- paste(self$id, "sources_panel", sep = "_")
      sess <- if (!is.null(parentSession)) {
        parentSession
      } else {
        shiny::getDefaultReactiveDomain()
      }
      tabTitle <- if (datasource == "all") {
        "ALL"
      } else if (datasource == "pubmed") {
        "PUBMED"
      } else {
        datasource
      }
      showTab(inputId = sourcePanelId, target = tabTitle, session = sess)
      invisible(self)
    },

    #' Print summary
    print = function() {
      cat(sprintf("<ORAEnrichmentSession> %s\n", self$id))
      cat(sprintf("  Tool: %s\n", self$toolName))
      cat(sprintf("  Organism: %s\n", self$organism))
      cat(sprintf("  Input: %d genes\n", private$.input$size()))
      if (self$hasBackground()) {
        cat(sprintf("  Background: %d genes\n", self$background$size()))
      }
      cat(sprintf("  Has results: %s\n", self$hasResults()))
      if (self$hasResults()) {
        cat(sprintf("  Result rows: %d\n", nrow(private$.results)))
      }
      invisible(self)
    }
  ),

  private = list(
    # Module session reference
    .moduleSession = NULL,

    # Parent Shiny session (for UI operations from output sessions)
    .parentSession = NULL,

    # Output sessions (BarchartOutputSession, etc.)
    .outputSessions = list(),

    # Background conversion table (separate from input conversion table)
    .backgroundConversionTable = NULL,

    # Output IDs for cleanup tracking
    .outputIds = list(
      tables = character(),
      text = character(),
      conversion = character()
    ),

    # =========================================================================
    # UI GENERATION HELPERS
    # =========================================================================

    #' Generate the Parameters box
    generateParametersBox = function() {
      outputId <- paste(self$id, "enrichment_parameters", sep = "_")
      private$.outputIds$text <- c(private$.outputIds$text, outputId)

      shinydashboard::box(
        title = "Parameters",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        shiny::verbatimTextOutput(outputId = outputId)
      )
    },

    #' Generate the Results panel with datasource tabs
    generateResultsPanel = function() {
      runKey <- self$id

      # Get selected datasources from parameters
      selectedDatasources <- self$getParameters()$datasources

      # Build list of tab codes to create: "all" + selected datasources
      # TAB_NAMES maps display names (e.g., "GO:MF") to codes (e.g., "gomf")
      tabCodesToCreate <- c("all")
      for (ds in selectedDatasources) {
        if (ds %in% names(TAB_NAMES)) {
          tabCodesToCreate <- c(tabCodesToCreate, TAB_NAMES[[ds]])
        }
      }

      # Generate only the tabs we need
      sourcesPanel <- do.call(
        shiny::tabsetPanel, c(
          id = paste(runKey, "sources_panel", sep = "_"),
          lapply(tabCodesToCreate, function(tabName) {
            private$generateDatasourceTab(tabName)
          })
        )
      )

      # Track text output IDs
      genesNotFoundId <- paste(runKey, "genesNotFound", sep = "_")
      notConvertedInputId <- paste(runKey, "notConverted_input", sep = "_")
      notConvertedRefId <- paste(runKey, "notConverted_reference", sep = "_")
      private$.outputIds$text <- c(private$.outputIds$text,
                                    genesNotFoundId, notConvertedInputId, notConvertedRefId)

      # Track conversion table IDs
      convInputId <- paste(runKey, "conversionTable_input", sep = "_")
      convRefId <- paste(runKey, "conversionTable_reference", sep = "_")
      private$.outputIds$conversion <- c(private$.outputIds$conversion,
                                          convInputId, convRefId)

      shiny::tabPanel(
        title = "Results",
        icon = shiny::icon("table"),
        shiny::tags$div(
          id = paste(runKey, "resultsDiv", sep = "_"),
          class = "enrichmentResultsDiv",
          shiny::tags$br(),
          sourcesPanel
        ),
        shiny::tags$br(),
        shiny::tags$div(
          id = paste(runKey, "conversionBoxes", sep = "_"),
          style = "display: none;",
          shinydashboard::box(
            title = "Conversion Table",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            shiny::tabsetPanel(
              shiny::tabPanel("Input List",
                              DT::dataTableOutput(convInputId)),
              shiny::tabPanel("Reference Background",
                              shiny::div(id = paste(runKey, "conversionTable_genome_div", sep = "_"),
                                         shiny::h3("No custom background was submitted by the user, the entire selected genome was used instead.")),
                              shiny::div(id = paste(runKey, "conversionTable_reference_div", sep = "_"),
                                         style = "display:none",
                                         DT::dataTableOutput(convRefId)))
            )
          ),
          shinydashboard::box(
            title = "Unconverted Inputs",
            class = "conversionBox",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            shiny::verbatimTextOutput(notConvertedInputId),
            shiny::tags$hr(),
            shiny::div(id = paste(runKey, "notConverted_reference_div", sep = "_"),
                       style = "display:none",
                       shiny::verbatimTextOutput(notConvertedRefId))
          )
        ),
        shinydashboard::box(
          title = "No-hit Inputs",
          class = "conversionBox",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          shiny::verbatimTextOutput(genesNotFoundId)
        )
      )
    },

    #' Generate a single datasource tab
    generateDatasourceTab = function(tabName) {
      tableId <- paste(self$id, "table", tabName, sep = "_")
      private$.outputIds$tables <- c(private$.outputIds$tables, tableId)

      shiny::tabPanel(
        title = names(TAB_NAMES[TAB_NAMES == tabName]),
        shiny::tags$br(),
        DT::dataTableOutput(tableId)
      )
    },

    #' Generate the Plots panel with OutputSession containers
    #'
    #' Creates tabs for: Barchart, Dot Plot, Scatter Plot, Heatmap (3 sub-tabs),
    #' Network (3 sub-tabs). No external config - method is source of truth.
    generatePlotsPanel = function() {
      runKey <- self$id
      uiTermKeyword <- stringr::str_to_title(
        UI_TERM_KEYWORD[[self$enrichmentType]]
      )

      shiny::tabPanel(
        title = "Plots",
        icon = shiny::icon("chart-bar"),
        shiny::tabsetPanel(
          # Barchart tab
          shiny::tabPanel(
            title = "Barchart",
            shiny::tags$br(),
            shiny::tags$div(
              id = paste(runKey, "barchart_container", sep = "_"),
              class = "output-session-container"
            )
          ),
          # Dot Plot tab
          shiny::tabPanel(
            title = "Dot Plot",
            shiny::tags$br(),
            shiny::tags$div(
              id = paste(runKey, "dotPlot_container", sep = "_"),
              class = "output-session-container"
            )
          ),
          # Scatter Plot tab
          shiny::tabPanel(
            title = "Scatter Plot",
            shiny::tags$br(),
            shiny::tags$div(
              id = paste(runKey, "scatterPlot_container", sep = "_"),
              class = "output-session-container"
            )
          ),
          # Heatmap tab with 3 sub-tabs
          shiny::tabPanel(
            title = "Heatmap",
            shiny::tags$br(),
            shiny::tabsetPanel(
              shiny::tabPanel(
                title = paste0(uiTermKeyword, " Vs Genes"),
                shiny::tags$div(
                  id = paste(runKey, "heatmap1_container", sep = "_"),
                  class = "output-session-container"
                )
              ),
              shiny::tabPanel(
                title = paste0(uiTermKeyword, " Vs ", uiTermKeyword),
                shiny::tags$div(
                  id = paste(runKey, "heatmap2_container", sep = "_"),
                  class = "output-session-container"
                )
              ),
              shiny::tabPanel(
                title = "Genes Vs Genes",
                shiny::tags$div(
                  id = paste(runKey, "heatmap3_container", sep = "_"),
                  class = "output-session-container"
                )
              )
            )
          ),
          # Network tab with 3 sub-tabs
          shiny::tabPanel(
            title = "Network",
            shiny::tags$br(),
            shiny::tabsetPanel(
              shiny::tabPanel(
                title = paste0(uiTermKeyword, " Vs Genes"),
                shiny::tags$div(
                  id = paste(runKey, "network1_container", sep = "_"),
                  class = "output-session-container"
                )
              ),
              shiny::tabPanel(
                title = paste0(uiTermKeyword, " Vs ", uiTermKeyword),
                shiny::tags$div(
                  id = paste(runKey, "network2_container", sep = "_"),
                  class = "output-session-container"
                )
              ),
              shiny::tabPanel(
                title = "Genes Vs Genes",
                shiny::tags$div(
                  id = paste(runKey, "network3_container", sep = "_"),
                  class = "output-session-container"
                )
              )
            )
          )
        )
      )
    },

    #' Convert gene IDs to tool-specific format
    #'
    #' @param geneList Character vector of gene symbols
    #' @param namespace Target namespace (or NULL for default)
    #' @return Data frame with (input, target, name) columns, or NULL on failure
    convertGeneIds = function(geneList, namespace) {
      # Determine default namespace if not specified
      if (is.null(namespace) || namespace == "Default namespace") {
        namespace <- getDefaultTargetNamespace(self$toolName, self$organism)
      }

      conversionTable <- tryCatch({
        if (namespace == "USERINPUT") {
          # No conversion needed - pass through
          data.frame(
            input = geneList,
            target = geneList,
            name = geneList,
            stringsAsFactors = FALSE
          )
        } else if (self$toolName == "STRING") {
          # STRING: Use STRING's get_string_ids API
          private$stringConvert(geneList)
        } else if (self$toolName == "PANTHER") {
          # PANTHER: Use PANTHER's geneinfo API
          private$pantherConvert(geneList)
        } else if (self$toolName == "GeneCodis") {
          # GeneCodis: No conversion needed
          data.frame(
            input = geneList,
            target = geneList,
            name = geneList,
            stringsAsFactors = FALSE
          )
        } else {
          # gProfiler, WebGestalt, enrichR: Use g:Profiler conversion
          private$gprofilerConvert(geneList, namespace)
        }
      }, error = function(e) {
        warning(sprintf("Gene conversion error for %s: %s", self$toolName, e$message))
        NULL
      })

      # Attach namespace as attribute
      if (!is.null(conversionTable)) {
        attr(conversionTable, "namespace") <- namespace
      }

      conversionTable
    },

    # getDefaultTargetNamespace REMOVED - use global function from enrich-main.R
    # Called as: getDefaultTargetNamespace(self$toolName, self$organism)

    #' STRING-specific gene conversion via STRING API
    stringConvert = function(geneList) {
      url <- "https://string-db.org/api/json/get_string_ids"
      params <- list(
        "identifiers" = paste0(geneList, collapse = "%0d"),
        "species" = self$organism
      )
      request <- httr::POST(url, body = params)
      if (httr::status_code(request) == 200) {
        result <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))
        if (is.data.frame(result) && nrow(result) > 0) {
          result <- result[, c("queryItem", "stringId", "preferredName")]
          colnames(result) <- c("input", "target", "name")
          return(result)
        }
      }
      NULL
    },

    #' PANTHER-specific gene conversion
    pantherConvert = function(geneList) {
      url <- "https://pantherdb.org/services/oai/pantherdb/geneinfo"
      params <- list(
        "geneInputList" = paste0(geneList, collapse = ","),
        "organism" = self$organism
      )
      request <- httr::POST(url, body = params, encode = "form")
      if (httr::status_code(request) == 200) {
        pantherResponse <- jsonlite::fromJSON(rawToChar(httr::content(request, "raw")))
        if ("search" %in% names(pantherResponse) && "mapped_genes" %in% names(pantherResponse$search)) {
          mappedGenes <- pantherResponse$search$mapped_genes$gene
          if (length(mappedGenes) > 0) {
            return(data.frame(
              input = mappedGenes$mapped_id_list,
              target = mappedGenes$accession,
              name = mappedGenes$sf_name,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      NULL
    },

    #' gProfiler-based gene conversion
    gprofilerConvert = function(geneList, namespace) {
      organismShortName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$short_name
      result <- gprofiler2::gconvert(
        geneList,
        organism = organismShortName,
        target = namespace,
        mthreshold = 1,
        filter_na = TRUE
      )
      if (!is.null(result) && nrow(result) > 0) {
        result <- result[, c("input", "target", "name")]
        return(result)
      }
      NULL
    },

    #' Attach database links to Term_ID column
    #'
    #' Modifies the Term_ID column to include hyperlinks to external databases.
    #' @param df Data frame with enrichment results
    #' @return Modified data frame with hyperlinked Term_IDs
    attachDBLinks = function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)

      # Gene Ontology - stopChar=":" prevents matching "GOSLIM:*"
      df <- private$attachLinksToDF(df, "GO", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")
      df <- private$attachLinksToDF(df, "GOSLIM", "https://www.ebi.ac.uk/QuickGO/term/", stopChar = ":")

      # Protein domains and classifications
      df <- private$attachLinksToDF(df, "INTERPRO", "https://www.ebi.ac.uk/interpro/entry/InterPro/")
      df <- private$attachLinksToDF(df, "PFAM", "https://www.ebi.ac.uk/interpro/entry/pfam/")
      df <- private$attachLinksToDF(df, "UNIPROT", "https://www.uniprot.org/keywords/")
      df <- private$attachLinksToDF(df, "PANTHERPC", "https://pantherdb.org/panther/category.do?categoryAcc=")

      # Pathways
      df <- private$attachLinksToDF(df, "PANTHER Pathways", "http://www.pantherdb.org/pathway/pathDetail.do?clsAccession=")
      df <- private$attachLinksToDF(df, "REAC", "https://reactome.org/content/detail/")
      df <- private$attachLinksToDF(df, "WP", "https://www.wikipathways.org/index.php/Pathway:")
      df <- private$attachLinksToDF(df, "BioPlanet", "https://tripod.nih.gov/bioplanet/detail.jsp?pid=", urlSuffix = "&target=pathway")

      # Disease and phenotype ontologies
      df <- private$attachLinksToDF(df, "DO", "http://www.informatics.jax.org/disease/")
      df <- private$attachLinksToDF(df, "HP", "https://monarchinitiative.org/")
      df <- private$attachLinksToDF(df, "OMIM", "https://www.omim.org/entry/")
      df <- private$attachLinksToDF(df, "ORPHA", "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=", gSub = "ORPHA:")
      df <- private$attachLinksToDF(df, "WBP", "https://wormbase.org/species/all/phenotype/")
      df <- private$attachLinksToDF(df, "WBBT", "https://wormbase.org/species/all/anatomy_term/")
      df <- private$attachLinksToDF(df, "MGI", "https://www.informatics.jax.org/vocab/mp_ontology/")

      # Tissue ontologies
      df <- private$attachLinksToDF(df, "BTO", "https://www.ebi.ac.uk/ols/ontologies/bto/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FBTO_", gSub = "BTO:")

      # Regulatory elements
      df <- private$attachLinksToDF(df, "TF", "http://gene-regulation.com/cgi-bin/pub/databases/transfac/search.cgi?species=Homo_sapiens&factor=")
      df <- private$attachLinksToDF(df, "CollecTRI", "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")
      df <- private$attachLinksToDF(df, "MIRNA", "https://www.mirbase.org/textsearch.shtml?q=", gSub = "MIRNA:")

      # Pharmacogenomics
      df <- private$attachLinksToDF(df, "PharmGKB", "https://www.clinpgx.org/chemical/")

      # KEGG (special handling)
      df <- private$attachKEGGLinksToDF(df)

      # DISGENET special handling
      df <- private$attachDISGENETLinks(df)

      df
    },

    #' Attach links to a specific datasource in the data frame
    attachLinksToDF = function(df, sourceId, url, stopChar = "$", gSub = NULL, urlSuffix = "") {
      pattern <- paste0("^", sourceId, stopChar)
      matches <- grepl(pattern, df$Source)

      if (any(matches)) {
        linksVector <- df$Term_ID[matches]
        gSubLinksVector <- if (!is.null(gSub)) gsub(gSub, "", linksVector) else linksVector

        df$Term_ID[matches] <- paste0(
          "<a href='", url, gSubLinksVector, urlSuffix, "' target='_blank'>",
          linksVector, "</a>"
        )
      }
      df
    },

    #' Attach KEGG-specific links with optional gene highlighting
    attachKEGGLinksToDF = function(df) {
      keggMatches <- grepl("^KEGG$", df$Source)
      if (!any(keggMatches)) return(df)

      keggName <- ORGANISMS[ORGANISMS$taxid == self$organism, ]$kegg_name
      if (is.na(keggName)) return(df)  # No KEGG support for this organism

      linksVector <- df$Term_ID[keggMatches]

      if (self$toolName == "STRING") {
        # STRING provides organism-specific KEGG IDs already
        df$Term_ID[keggMatches] <- paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          linksVector, "' target='_blank'>", linksVector, "</a>"
        )
      } else {
        # Traditional tools - need to add organism prefix
        df$Term_ID[keggMatches] <- paste0(
          "<a href='https://www.kegg.jp/kegg-bin/show_pathway?",
          gsub("KEGG:|map", keggName, linksVector),
          "' target='_blank'>", linksVector, "</a>"
        )
      }
      df
    },

    #' Attach DISGENET-specific links
    attachDISGENETLinks = function(df) {
      disgenetMatches <- df$Source == "DISGENET"
      if (any(disgenetMatches)) {
        df$Term_ID[disgenetMatches] <- paste0(
          "<a href='https://www.disgenet.org/search/0/",
          df$Term_ID_noLinks[disgenetMatches],
          "/' target='_blank'>",
          df$Term_ID_noLinks[disgenetMatches],
          "</a>"
        )
      }
      df
    },

    # -------------------------------------------------------------------------
    # Display Helpers (encapsulated from enrich-main.R)
    # -------------------------------------------------------------------------

    #' Render gene report text
    #'
    #' Helper for displaying gene lists with counts.
    #'
    #' @param outputId Shiny output ID
    #' @param genes Character vector of genes
    #' @param messageTemplate sprintf template with %d and %s placeholders
    renderGeneReportInternal = function(outputId, genes, messageTemplate) {
      count <- length(genes)
      if (count > 0) {
        geneList <- paste(genes, collapse = ", ")
        message <- sprintf(messageTemplate, count, geneList)
        renderShinyText(outputId, message)
      } else {
        renderShinyText(outputId, "-")
      }
    },

    #' Render no-hit genes display
    #'
    #' Displays genes not found in any enriched term.
    #'
    #' @param noHitGenes Character vector of genes
    renderNoHitGenesInternal = function(noHitGenes) {
      shinyOutputId <- paste(self$id, "genesNotFound", sep = "_")
      private$renderGeneReportInternal(
        outputId = shinyOutputId,
        genes = noHitGenes,
        messageTemplate = "%d input item(s) not found in any result term:\n%s"
      )
    },

    # -------------------------------------------------------------------------
    # Results Table Rendering (encapsulated from func-render.R)
    # -------------------------------------------------------------------------

    #' Render enrichment results table with expandable rows
    #'
    #' Renders a DT table with expandable rows for ORA results.
    #' Encapsulated from func-render.R:renderEnrichmentTable().
    #'
    #' @param output Shiny output object
    #' @param shinyOutputId Output ID to render to
    #' @param data Data frame to render
    #' @param caption Table caption
    #' @param fileName Base filename for exports
    #' @param mode Label for expandable content (e.g., "Positive Hits")
    #' @param hiddenColumns Column indices to hide (0-indexed after expand column)
    #' @param expandableColumn Column index containing expandable content
    #' @param filter DT filter type ("none", "top", "bottom")
    #' @param exportExcludeColumns Columns to exclude from export
    renderResultsTableInternal = function(output, shinyOutputId, data, caption, fileName,
                                           mode, hiddenColumns, expandableColumn,
                                           filter = 'none', exportExcludeColumns = c(0, 11)) {
      output[[shinyOutputId]] <- DT::renderDataTable({
        tableData <- cbind(' ' = '&oplus;', data)

        dt <- DT::datatable(
          tableData,
          escape = FALSE,
          rownames = FALSE,
          selection = 'none',
          filter = filter,
          extensions = c('Buttons'),
          caption = caption,
          options = list(
            scrollX = TRUE,
            "dom" = 'T<"clear">lBfrtip',
            buttons = createExportButtons(fileName, exportExcludeColumns),
            columnDefs = list(
              list(visible = FALSE, targets = hiddenColumns),
              list(orderable = FALSE, searchable = FALSE,
                   className = 'details-control', targets = 0)
            ),
            initComplete = htmlwidgets::JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('thead tr:eq(1) td:eq(0)').find('input,select').hide();",
              "}"
            )
          ),
          callback = htmlwidgets::JS(paste0(
            "table.column(0).nodes().to$().css({cursor: 'pointer'});
            let format = function(d) {
              return '<div style=\"background-color:#eee; padding: .5em;\"> <b>", mode, ":</b> ' +
                      d[", expandableColumn, "] + '</div>';
            };
            table.on('click', 'td.details-control', function() {
              let td = $(this), row = table.row(td.closest('tr'));
              if (row.child.isShown()) {
                row.child.hide();
                td.html('&oplus;');
              } else {
                row.child(format(row.data())).show();
                td.html('&CircleMinus;');
              }
            });"
          ))
        )

        # Format P-value column to show 3 significant figures
        dt <- DT::formatSignif(dt, columns = 'P-value', digits = 3)
        dt
      }, server = FALSE)
    }
  )
)
