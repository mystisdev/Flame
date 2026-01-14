# =============================================================================
# FLAME Combination Session
# =============================================================================
#
# Manages the Combination tab which compares results across multiple enrichment
# runs. This R6 class owns the combination state and encapsulates all logic for
# computing, filtering, and rendering combined results.
#
# Responsibilities:
# - Compute combined results from all active enrichment sessions
# - Calculate Fisher's combined p-values and hit statistics
# - Filter results by datasources, tools, and rank
# - Render combination table, UpSet plot, and network visualization
# - Manage observers for combination UI controls
#
# Pattern:
# - Instantiated once per user session in server.R
# - refresh() called when enrichment runs complete or close
# - server() sets up observers via moduleServer pattern
#
# Dependencies:
# - enrich-session-registry.R (for EnrichmentSessionRegistry)
# - config-b-global_variables.R (for DATASOURCE_COLORS, etc.)
#
# =============================================================================

# =============================================================================
# R6 SESSION CLASS
# =============================================================================

CombinationSession <- R6::R6Class(
  "CombinationSession",

  public = list(
    #' Initialize a CombinationSession
    #'
    #' @param enrichmentRegistry EnrichmentSessionRegistry instance
    initialize = function(enrichmentRegistry) {
      if (!inherits(enrichmentRegistry, "EnrichmentSessionRegistry")) {
        stop("enrichmentRegistry must be an EnrichmentSessionRegistry")
      }
      private$.enrichmentRegistry <- enrichmentRegistry
      private$.results <- NULL
      private$.observers <- list()
      # Fixed output IDs for cleanup
      private$.outputIds <- c(
        "combo_table",
        "combo_upsetClick_table",
        "combo_network_table",
        "combo_visNetwork",
        "upsetjsCombo"
      )
    },

    #' Refresh combination results from all active sessions
    #'
    #' Called when: new run completes, run closes, datasources change.
    #' Recomputes the combined results table from all sessions with results.
    #'
    #' @return Invisible self for chaining
    refresh = function() {
      sessions <- private$.enrichmentRegistry$getAll()
      sessionsWithResults <- Filter(function(s) s$hasResults(), sessions)

      if (length(sessionsWithResults) < 2) {
        private$.results <- NULL
        return(invisible(self))
      }

      # Compute combined results
      private$.results <- private$computeCombinedResults(sessionsWithResults)
      invisible(self)
    },

    #' Check if combination is available (2+ sessions with results)
    #'
    #' @return Logical TRUE if combination can be shown
    isAvailable = function() {
      !is.null(private$.results) && nrow(private$.results) > 0
    },

    #' Get filtered results
    #'
    #' Pure function - returns filtered DataFrame without side effects.
    #'
    #' @param datasources Character vector of datasources to include (NULL = all)
    #' @param tools Character vector of tool display names to include (NULL = all)
    #' @param rankThreshold Minimum rank to include (NULL = no filter)
    #' @return Filtered DataFrame or NULL if no results
    getFilteredResults = function(datasources = NULL, tools = NULL,
                                  rankThreshold = NULL) {
      if (is.null(private$.results)) return(NULL)

      filtered <- private$.results

      if (!is.null(datasources) && length(datasources) > 0) {
        filtered <- filtered[filtered$Source %in% datasources, ]
      }

      if (!is.null(tools) && length(tools) > 0) {
        # Filter rows where any of the selected tools appear in Tools column
        toolPattern <- paste(tools, collapse = "|")
        filtered <- filtered[grepl(toolPattern, filtered$Tools), ]
      }

      if (!is.null(rankThreshold)) {
        filtered <- filtered[filtered$Rank >= rankThreshold, ]
      }

      return(filtered)
    },

    #' Get all results (unfiltered)
    #'
    #' @return DataFrame of all combined results or NULL
    getResults = function() {
      private$.results
    },

    #' Get available datasources for UI picker
    #'
    #' @return Character vector of datasource names
    getAvailableDatasources = function() {
      if (is.null(private$.results)) return(character(0))
      unique(private$.results$Source)
    },

    #' Get available tools for UI picker
    #'
    #' @return Character vector of tool display names
    getAvailableTools = function() {
      if (is.null(private$.results)) return(character(0))
      # Extract individual tools from comma-separated Tools column
      allTools <- unlist(strsplit(unique(private$.results$Tools), split = ","))
      unique(trimws(allTools))
    },

    #' Get maximum rank for slider
    #'
    #' @return Integer maximum rank value
    getMaxRank = function() {
      if (is.null(private$.results)) return(2)
      max(private$.results$Rank, na.rm = TRUE)
    },

    #' Set up server logic
    #'
    #' Registers observers for combination UI controls.
    #' Uses direct observer registration (not moduleServer) since

    #' combination UI is not namespaced.
    #'
    #' @param input Shiny input object
    #' @param output Shiny output object
    #' @param session Shiny session object
    server = function(input, output, session) {
      private$.input <- input
      private$.output <- output
      private$.session <- session

      # Output IDs are tracked in private$.outputIds (initialized in constructor)

      # Datasource filter change
      private$.observers$datasource <- shiny::observeEvent(
        input$combo_datasources,
        private$handleDatasourceFilter(),
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

      # UpSet click
      private$.observers$upsetClick <- shiny::observeEvent(
        input$upsetjsCombo_click,
        private$handleUpsetClick(),
        ignoreInit = TRUE
      )

      # Network visualization button
      private$.observers$network <- shiny::observeEvent(
        input$combo_visNetwork_run,
        private$handleNetworkViz(),
        ignoreInit = TRUE
      )
    },

    #' Update UI when results change
    #'
    #' Shows/hides combination tab and updates picker choices.
    #'
    #' @param parentSession Parent Shiny session for tab operations
    updateUI = function(parentSession) {
      if (self$isAvailable()) {
        shiny::showTab(inputId = "toolTabsPanel", target = "Combination",
                       session = parentSession)

        # Update datasource picker
        dsChoices <- ENRICHMENT_DATASOURCES[
          which(ENRICHMENT_DATASOURCES %in% self$getAvailableDatasources())
        ]
        # Clear then set to trigger observer
        shinyWidgets::updatePickerInput(parentSession, "combo_datasources",
                                        choices = dsChoices, selected = NULL)
        shinyWidgets::updatePickerInput(parentSession, "combo_datasources",
                                        choices = dsChoices, selected = dsChoices)

        # Update tool picker
        toolChoices <- self$getAvailableTools()
        shinyWidgets::updatePickerInput(parentSession, "combo_tool_picker",
                                        choices = toolChoices, selected = NULL)
        shinyWidgets::updatePickerInput(parentSession, "combo_tool_picker",
                                        choices = toolChoices, selected = toolChoices)

        # Update rank slider
        maxRank <- self$getMaxRank()
        sliderValue <- if (maxRank <= 2) maxRank else maxRank - 1
        shiny::updateSliderInput(parentSession, "combo_rank_slider",
                                 value = sliderValue, max = maxRank)

        # Show clear all button
        shinyjs::show(paste0(ModuleIds$ENRICH_FORM, "-enrichment_all_clear"))
      } else {
        shiny::hideTab(inputId = "toolTabsPanel", target = "Combination",
                       session = parentSession)
      }
    },

    #' Reset combination state
    #'
    #' Clears results and hides tab.
    #'
    #' @param parentSession Parent Shiny session for tab operations
    reset = function(parentSession) {
      private$.results <- NULL
      shiny::hideTab(inputId = "toolTabsPanel", target = "Combination",
                     session = parentSession)
      # Clear our own outputs
      self$clearOutputs()
    },

    #' Clear all Shiny outputs owned by this session
    clearOutputs = function() {
      output <- private$.output
      if (is.null(output)) return(invisible(self))

      for (outputId in private$.outputIds) {
        tryCatch({
          if (outputId == "combo_visNetwork") {
            output[[outputId]] <- visNetwork::renderVisNetwork({})
            shinyjs::hide(outputId)
          } else if (outputId == "upsetjsCombo") {
            output[[outputId]] <- upsetjs::renderUpsetjs({})
          } else {
            # datatables
            output[[outputId]] <- DT::renderDataTable(NULL)
          }
        }, error = function(e) NULL)
      }
      invisible(self)
    },

    #' Clean up observers and outputs
    cleanup = function() {
      # Clear outputs
      self$clearOutputs()

      # Destroy observers
      for (obs in private$.observers) {
        if (!is.null(obs)) {
          tryCatch(obs$destroy(), error = function(e) NULL)
        }
      }
      private$.observers <- list()
      private$.results <- NULL
    }
  ),

  private = list(
    # Dependencies
    .enrichmentRegistry = NULL,

    # Output IDs for cleanup tracking
    .outputIds = character(),

    # State
    .results = NULL,  # Combined DataFrame (was global combinationResult)

    # Shiny references
    .input = NULL,
    .output = NULL,
    .session = NULL,

    # Observers for cleanup
    .observers = list(),

    # =========================================================================
    # COMPUTATION METHODS
    # =========================================================================

    #' Compute combined results from sessions
    #'
    #' @param sessionsWithResults List of ORAEnrichmentSession objects
    #' @return DataFrame with combined results
    computeCombinedResults = function(sessionsWithResults) {
      # Build named list of results keyed by session ID
      functionalEnrichmentResults <- lapply(
        sessionsWithResults,
        function(s) s$getResults()
      )
      names(functionalEnrichmentResults) <- names(sessionsWithResults)

      # Combine all results
      combined <- dplyr::bind_rows(functionalEnrichmentResults, .id = "Tool")

      # Select relevant columns
      combined <- combined[, c("Source", "Term_ID", "Function",
                               "Term_ID_noLinks", "Tool", "P-value",
                               "Positive Hits")]

      # Convert session ID to display name
      combined$Tool <- sapply(combined$Tool, private$sessionIdToDisplayName)

      # Calculate hit statistics per term
      termHitStats <- private$calculateTermHitStats(combined)

      # Deduplicate (Term_ID, Tool) pairs before grouping
      termToolMatching <- combined %>%
        dplyr::distinct(Term_ID_noLinks, Tool, .keep_all = TRUE) %>%
        dplyr::group_by(Term_ID_noLinks) %>%
        dplyr::summarise(Tools = toString(Tool), .groups = "drop")

      # Join and calculate Fisher stats
      combined <- plyr::join(termToolMatching, combined, type = "left",
                             by = "Term_ID_noLinks")
      combined <- private$calculateFisherStats(combined)
      combined <- dplyr::distinct(combined, Term_ID_noLinks, Tools,
                                  .keep_all = TRUE)

      # Join hit stats
      combined <- plyr::join(combined, termHitStats, type = "left",
                             by = "Term_ID_noLinks")

      # Select and rename final columns
      combined <- combined[, c("Source", "Term_ID", "Function",
                               "Term_ID_noLinks", "Tools",
                               "Chisq", "P_value_combined",
                               "Intersection_Hits", "Union_Hits",
                               "Hit_Summary")]
      names(combined) <- c("Source", "Term ID", "Function",
                           "Term_ID_noLinks", "Tools",
                           "X<sup>2</sup>", "Comb. P-value",
                           "Intersection_Hits", "Union_Hits", "Hit_Summary")

      # Calculate rank (number of tools)
      combined$Rank <- lengths(
        regmatches(combined$Tools, gregexpr(",", combined$Tools))
      ) + 1

      return(combined)
    },

    #' Convert session ID to display name
    #'
    #' Single source of truth for the format: "toolName (displayNumber)"
    #'
    #' @param sessionId Full session ID (e.g., "functional_gProfiler_5")
    #' @return Display name (e.g., "gProfiler (1)")
    sessionIdToDisplayName = function(sessionId) {
      enrichSession <- private$.enrichmentRegistry$get(sessionId)
      if (!is.null(enrichSession)) {
        paste0(enrichSession$toolName, " (", enrichSession$displayNumber, ")")
      } else {
        # Fallback: parse from key
        parts <- strsplit(sessionId, "_")[[1]]
        paste(parts[-1], collapse = "_")
      }
    },

    #' Calculate term hit statistics
    #'
    #' @param comboData Combined DataFrame
    #' @return DataFrame with hit intersection/union per term
    calculateTermHitStats = function(comboData) {
      # Get unique hits per term per tool
      hitsPerTermTool <- comboData %>%
        dplyr::select(Term_ID_noLinks, Tool, `Positive Hits`) %>%
        dplyr::distinct(Term_ID_noLinks, Tool, .keep_all = TRUE)

      # Calculate intersection and union per term
      hitsPerTermTool %>%
        dplyr::group_by(Term_ID_noLinks) %>%
        dplyr::summarise(
          Intersection_Hits = {
            hitLists <- strsplit(trimws(`Positive Hits`), ",\\s*")
            if (length(hitLists) == 1) {
              paste(hitLists[[1]], collapse = ", ")
            } else {
              paste(Reduce(intersect, hitLists), collapse = ", ")
            }
          },
          Union_Hits = {
            hitLists <- strsplit(trimws(`Positive Hits`), ",\\s*")
            paste(Reduce(union, hitLists), collapse = ", ")
          },
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Intersection_Count = sapply(
            strsplit(Intersection_Hits, ",\\s*"),
            function(x) sum(trimws(x) != "")
          ),
          Union_Count = sapply(
            strsplit(Union_Hits, ",\\s*"),
            function(x) sum(trimws(x) != "")
          ),
          Hit_Summary = paste0(Intersection_Count, " / ", Union_Count, " genes")
        ) %>%
        dplyr::select(Term_ID_noLinks, Intersection_Hits, Union_Hits,
                      Hit_Summary)
    },

    #' Calculate Fisher's combined p-values
    #'
    #' @param combinedData Combined DataFrame with P-value column
    #' @return DataFrame with Chisq and P_value_combined columns added
    calculateFisherStats = function(combinedData) {
      combinedData$`P-value` <- as.numeric(combinedData$`P-value`)

      combinedResults <- do.call(
        rbind,
        tapply(
          combinedData$`P-value`,
          combinedData$Term_ID_noLinks,
          FUN = private$combinePvalues
        )
      )

      combinedResults <- as.data.frame(combinedResults)
      colnames(combinedResults) <- c("P_value_combined", "Chisq")
      combinedResults$Term_ID_noLinks <- row.names(combinedResults)

      # Merge back
      combinedData <- merge(combinedData, combinedResults,
                            by = "Term_ID_noLinks")
      combinedData$P_value_combined <- as.numeric(
        combinedData$P_value_combined
      )
      combinedData$Chisq <- as.numeric(combinedData$Chisq)

      return(combinedData)
    },

    #' Combine p-values using Fisher's method
    #'
    #' @param pvalues Numeric vector of p-values
    #' @return Named vector with p and statistic
    combinePvalues = function(pvalues) {
      result <- poolr::fisher(pvalues)
      return(c(result$p, result$statistic))
    },

    # =========================================================================
    # EVENT HANDLERS
    # =========================================================================

    #' Handle datasource filter change
    handleDatasourceFilter = function() {
      tryCatch({
        # Clear click-generated outputs (they show stale data)
        private$.output$combo_upsetClick_table <- DT::renderDataTable(NULL)
        private$.output$combo_network_table <- DT::renderDataTable(NULL)
        private$.output$combo_visNetwork <- renderVisNetwork({})

        # Get filtered results
        filtered <- self$getFilteredResults(
          datasources = private$.input$combo_datasources
        )

        if (is.null(filtered) || nrow(filtered) == 0) {
          renderWarning("Select at least one datasource.")
          return()
        }

        # Sort by rank descending
        filtered <- filtered[order(-filtered$Rank), ]

        # Convert to factors for dropdown filtering
        filtered$Source <- as.factor(filtered$Source)
        filtered$Tools <- as.factor(filtered$Tools)
        filtered$Rank <- as.factor(filtered$Rank)

        # Render table
        private$renderCombinationTable(filtered)

        # Render UpSet plot
        private$renderUpsetPlot(filtered)

      }, error = function(e) {
        print(paste0("Error in handleDatasourceFilter: ", e))
        renderError("Problem with combination results.")
      })
    },

    #' Handle UpSet click
    handleUpsetClick = function() {
      tryCatch({
        clickData <- private$.input$upsetjsCombo_click

        if (identical(as.character(clickData$elems), character(0))) {
          return()
        }

        elements <- as.data.frame(as.character(clickData$elems))
        colnames(elements) <- "Term_ID_noLinks"

        # Join with full results
        elements <- plyr::join(elements, private$.results, type = "left",
                               by = "Term_ID_noLinks")
        elements <- elements[, names(private$.results)]

        # Convert to factors for dropdown filtering
        elements$Source <- as.factor(elements$Source)
        elements$Tools <- as.factor(elements$Tools)
        elements$Rank <- as.factor(elements$Rank)

        # Render clicked terms table
        private$renderClickedTermsTable(elements)

      }, error = function(e) {
        print(paste0("Error in handleUpsetClick: ", e))
        renderError("Problem with UpSet Plot click.")
      })
    },

    #' Handle network visualization
    handleNetworkViz = function() {
      tryCatch({
        # Validate inputs
        if (is.null(private$.input$combo_datasources)) {
          renderWarning("Select at least one datasource.")
          return()
        }

        toolPicker <- private$.input$combo_tool_picker
        if (is.null(toolPicker) || length(toolPicker) < 2) {
          renderWarning("Select at least two tools.")
          return()
        }

        # Get network data
        networkData <- private$computeNetworkData()

        if (nrow(networkData) == 0) {
          renderWarning("Filters resulted in empty table.")
          return()
        }

        # Build and render network
        private$renderNetwork(networkData)

        # Render network table
        renderShinyDataTable(
          shinyOutputId = "combo_network_table",
          networkData,
          caption = "Combination Network",
          fileName = "combo_network"
        )

      }, error = function(e) {
        print(paste0("Error in handleNetworkViz: ", e))
        renderError("Problem with combinatorial network.")
      })
    },

    # =========================================================================
    # NETWORK COMPUTATION
    # =========================================================================

    #' Compute network data from sessions
    #'
    #' @return DataFrame with network edges
    computeNetworkData = function() {
      sessions <- private$.enrichmentRegistry$getAll()
      sessionsWithResults <- Filter(function(s) s$hasResults(), sessions)

      functionalEnrichmentResults <- lapply(
        sessionsWithResults,
        function(s) s$getResults()
      )
      names(functionalEnrichmentResults) <- names(sessionsWithResults)

      networkData <- dplyr::bind_rows(functionalEnrichmentResults, .id = "Tool")
      networkData <- networkData[, c("Source", "Function", "Positive Hits",
                                     "Tool", "P-value")]

      # Filter by datasources
      networkData <- subset(networkData,
                            Source %in% private$.input$combo_datasources)

      # Convert session ID to display name
      networkData$Tool <- sapply(networkData$Tool,
                                 private$sessionIdToDisplayName)

      # Filter by selected tools
      networkData <- subset(networkData,
                            Tool %in% private$.input$combo_tool_picker)

      # Expand positive hits
      networkData <- networkData %>%
        tidyr::separate_rows(`Positive Hits`, sep = ",")

      # Group by function and hit
      networkData <- networkData %>%
        dplyr::group_by(Source, Function, `Positive Hits`) %>%
        dplyr::summarise(Tool = paste(unique(Tool), collapse = ", "),
                         .groups = "drop")

      # Calculate rank and filter
      networkData <- networkData %>%
        dplyr::mutate(Rank = sapply(strsplit(Tool, ","), length))

      networkData <- networkData %>%
        dplyr::filter(Rank >= private$.input$combo_rank_slider)

      networkData <- networkData %>%
        dplyr::arrange(desc(Rank))

      return(networkData)
    },

    # =========================================================================
    # RENDER METHODS
    # =========================================================================

    #' Render combination table
    #'
    #' @param data Filtered DataFrame to render
    renderCombinationTable = function(data) {
      private$.output$combo_table <- DT::renderDataTable({
        tableData <- cbind(' ' = '&oplus;', data)

        dt <- DT::datatable(
          tableData,
          escape = FALSE,
          rownames = FALSE,
          selection = 'none',
          filter = "top",
          extensions = c('Buttons'),
          caption = "Term-tool combinations",
          options = list(
            scrollX = TRUE,
            "dom" = 'T<"clear">lBfrtip',
            buttons = createExportButtons("combination", c(0, 4)),
            columnDefs = list(
              list(visible = FALSE, targets = c(4, 8, 9, 10)),
              list(orderable = FALSE, searchable = FALSE,
                   className = 'details-control', targets = 0)
            ),
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('thead tr:eq(1) td:eq(0)').find('input,select').hide();",
              "}"
            )
          ),
          callback = JS(
            "table.column(0).nodes().to$().css({cursor: 'pointer'});
            let format = function(d) {
              return '<div style=\"background-color:#eee; padding: .5em;\">' +
                     '<b>Hit Summary:</b> ' + d[10] + '<br/><br/>' +
                     '<b>Intersection Hits (found by all tools):</b><br/>' + d[8] + '<br/><br/>' +
                     '<b>Union Hits (found by any tool):</b><br/>' + d[9] + '</div>';
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
          )
        )

        DT::formatSignif(dt, columns = c('X<sup>2</sup>', 'Comb. P-value'),
                         digits = 3)
      }, server = FALSE)
    },

    #' Render UpSet plot
    #'
    #' @param data Filtered DataFrame
    renderUpsetPlot = function(data) {
      # Extract tool-term lists
      expanded <- tidyr::separate_rows(data, `Tools`, sep = ", ")
      expanded <- expanded[, c("Tools", "Term_ID_noLinks")]
      expanded <- expanded %>%
        dplyr::group_by(Tools) %>%
        dplyr::mutate(`Term_ID_noLinks` = paste(`Term_ID_noLinks`,
                                                collapse = ",")) %>%
        dplyr::distinct()

      toolTermLists <- as.list(strsplit(expanded$Term_ID_noLinks, ","))
      names(toolTermLists) <- expanded$Tools

      # Render
      private$.output$upsetjsCombo <- upsetjs::renderUpsetjs({
        upsetjs::upsetjs() %>%
          upsetjs::fromList(toolTermLists) %>%
          upsetjs::interactiveChart() %>%
          upsetjs::generateDistinctIntersections() %>%
          upsetjs::chartLabels(combination.name = "Distinct Combinations Size") %>%
          upsetjs::chartFontSizes(
            font.family = "Segoe UI",
            chart.label = "18px",
            set.label = "10px"
          ) %>%
          upsetjs::chartTheme(color = "#383f4f") %>%
          upsetjs::chartLayout(width.ratios = c(0.2, 0.1, 0.7),
                               bar.padding = 0.3)
      })
    },

    #' Render clicked terms table
    #'
    #' @param data DataFrame of clicked terms
    renderClickedTermsTable = function(data) {
      private$.output$combo_upsetClick_table <- DT::renderDataTable({
        tableData <- cbind(' ' = '&oplus;', data)

        dt <- DT::datatable(
          tableData,
          escape = FALSE,
          rownames = FALSE,
          selection = 'none',
          filter = "top",
          extensions = c('Buttons'),
          caption = "UpSet Clicked Terms",
          options = list(
            scrollX = TRUE,
            "dom" = 'T<"clear">lBfrtip',
            buttons = createExportButtons("combo_upsetClick", c(0, 4)),
            columnDefs = list(
              list(visible = FALSE, targets = c(4, 8, 9, 10)),
              list(orderable = FALSE, searchable = FALSE,
                   className = 'details-control', targets = 0)
            ),
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('thead tr:eq(1) td:eq(0)').find('input,select').hide();",
              "}"
            )
          ),
          callback = JS(
            "table.column(0).nodes().to$().css({cursor: 'pointer'});
            let format = function(d) {
              return '<div style=\"background-color:#eee; padding: .5em;\">' +
                     '<b>Hit Summary:</b> ' + d[10] + '<br/><br/>' +
                     '<b>Intersection Hits (found by all tools):</b><br/>' + d[8] + '<br/><br/>' +
                     '<b>Union Hits (found by any tool):</b><br/>' + d[9] + '</div>';
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
          )
        )

        DT::formatSignif(dt, columns = c('X<sup>2</sup>', 'Comb. P-value'),
                         digits = 3)
      }, server = FALSE)
    },

    #' Render network visualization
    #'
    #' @param networkData DataFrame with network data
    renderNetwork = function(networkData) {
      # Create graph
      graph <- igraph::graph_from_edgelist(
        as.matrix(networkData[, c("Function", "Positive Hits")]),
        directed = FALSE
      )

      weights <- c(0, 1, 3, 6)
      igraph::E(graph)$weight <- 1
      igraph::E(graph)$title <- networkData$Tool
      igraph::E(graph)$width <- weights[networkData$Rank]

      # Convert to visNetwork data
      data <- visNetwork::toVisNetworkData(graph)

      nodes <- data$nodes
      row.names(nodes) <- NULL
      nodes$title <- nodes$label
      nodes$font.size <- 24
      nodes$group <- networkData$Source[match(nodes$label,
                                              networkData$Function)]
      nodes$group[is.na(nodes$group)] <- "Gene"

      edges <- data$edges

      # Get layout
      layout <- names(LAYOUT_CHOICES)[match(
        private$.input$combo_network_layout, LAYOUT_CHOICES
      )]

      # Render
      shinyjs::show("combo_visNetwork")
      private$.output$combo_visNetwork <- renderVisNetwork({
        set.seed(123)
        visNetwork::visNetwork(nodes = nodes, edges = edges,
                               background = "white") %>%
          visNetwork::visGroups(groupname = "GO:MF",
                                color = DATASOURCE_COLORS["GO:MF"][[1]],
                                shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "GO:BP",
                                color = DATASOURCE_COLORS["GO:BP"][[1]],
                                shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "GO:CC",
                                color = DATASOURCE_COLORS["GO:CC"][[1]],
                                shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "UNIPROT",
                                color = DATASOURCE_COLORS["UNIPROT"][[1]],
                                shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "KEGG",
                                color = DATASOURCE_COLORS["KEGG"][[1]],
                                shape = "diamond") %>%
          visNetwork::visGroups(groupname = "REAC",
                                color = DATASOURCE_COLORS["REAC"][[1]],
                                shape = "diamond") %>%
          visNetwork::visGroups(groupname = "WP",
                                color = DATASOURCE_COLORS["WP"][[1]],
                                shape = "diamond") %>%
          visNetwork::visGroups(groupname = "PANTHER Pathways",
                                color = DATASOURCE_COLORS["PANTHER Pathways"][[1]],
                                shape = "diamond") %>%
          visNetwork::visGroups(groupname = "DO",
                                color = DATASOURCE_COLORS["DO"][[1]],
                                shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "DISGENET",
                                color = DATASOURCE_COLORS["DISGENET"][[1]],
                                shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "OMIM",
                                color = DATASOURCE_COLORS["OMIM"][[1]],
                                shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "GLAD4U_DISEASE",
                                color = DATASOURCE_COLORS["GLAD4U_DISEASE"][[1]],
                                shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "ORPHA",
                                color = DATASOURCE_COLORS["ORPHA"][[1]],
                                shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "DRUGBANK",
                                color = DATASOURCE_COLORS["DRUGBANK"][[1]],
                                shape = "star") %>%
          visNetwork::visGroups(groupname = "GLAD4U_DRUG",
                                color = DATASOURCE_COLORS["GLAD4U_DRUG"][[1]],
                                shape = "star") %>%
          visNetwork::visGroups(groupname = "INTERPRO",
                                color = DATASOURCE_COLORS["INTERPRO"][[1]],
                                shape = "star") %>%
          visNetwork::visGroups(groupname = "PFAM",
                                color = DATASOURCE_COLORS["PFAM"][[1]],
                                shape = "star") %>%
          visNetwork::visGroups(groupname = "BTO",
                                color = DATASOURCE_COLORS["BTO"][[1]],
                                shape = "triangle") %>%
          visNetwork::visGroups(groupname = "WBBT",
                                color = DATASOURCE_COLORS["WBBT"][[1]],
                                shape = "triangle") %>%
          visNetwork::visGroups(groupname = "TF",
                                color = DATASOURCE_COLORS["TF"][[1]],
                                shape = "triangle") %>%
          visNetwork::visGroups(groupname = "CollecTRI",
                                color = DATASOURCE_COLORS["CollecTRI"][[1]],
                                shape = "triangle") %>%
          visNetwork::visGroups(groupname = "MIRNA",
                                color = DATASOURCE_COLORS["MIRNA"][[1]],
                                shape = "triangle") %>%
          visNetwork::visGroups(groupname = "CORUM",
                                color = DATASOURCE_COLORS["CORUM"][[1]],
                                shape = "triangle") %>%
          visNetwork::visGroups(groupname = "HPA",
                                color = DATASOURCE_COLORS["HPA"][[1]],
                                shape = "square") %>%
          visNetwork::visGroups(groupname = "HP",
                                color = DATASOURCE_COLORS["HP"][[1]],
                                shape = "square") %>%
          visNetwork::visGroups(groupname = "WBP",
                                color = DATASOURCE_COLORS["WBP"][[1]],
                                shape = "square") %>%
          visNetwork::visGroups(groupname = "MGI",
                                color = DATASOURCE_COLORS["MGI"][[1]],
                                shape = "square") %>%
          visNetwork::visGroups(groupname = "Gene",
                                color = GENE_NODE_COLOR, shape = "square") %>%
          visNetwork::visGroups(groupname = "PUBMED",
                                color = DATASOURCE_COLORS["PUBMED"][[1]],
                                shape = "square") %>%
          visNetwork::visEdges(color = "black") %>%
          visNetwork::visIgraphLayout(layout = layout) %>%
          visNetwork::visInteraction(navigationButtons = TRUE, hover = TRUE,
                                     multiselect = TRUE) %>%
          visNetwork::visEvents(
            click = sprintf("function(params) {
              setTimeout(function() {
                Shiny.setInputValue('%s_click', params, {priority: 'event'});
              }, 0);
            }", "combo_visNetwork"),
            select = sprintf("function(params) {
              setTimeout(function() {
                Shiny.setInputValue('%s_selected', params.nodes, {priority: 'event'});
              }, 0);
            }", "combo_visNetwork"),
            deselectNode = sprintf("function(params) {
              setTimeout(function() {
                Shiny.setInputValue('%s_deselect', params, {priority: 'event'});
              }, 0);
            }", "combo_visNetwork")
          )
      })
    }
  )
)
