# =============================================================================
# NETWORK OUTPUT SESSIONS
# =============================================================================
# All network-related output sessions in one file for cohesion.
#
# Class Hierarchy:
# OutputSession (base)
#     └── NetworkOutputSession (intermediate - adds network-specific state)
#             ├── Network1OutputSession (Function vs Gene)
#             ├── Network2OutputSession (Function vs Function)
#             └── Network3OutputSession (Gene vs Gene)
#
# Key differences from other OutputSessions:
# - Uses visNetwork instead of plotly
# - Events via Shiny input bindings (not plotly source pattern)
# - THREE outputs: visNetwork + enrichment table + edgelist table
# - Owns arenaEdgelist for Arena3D export
# =============================================================================

# =============================================================================
# NETWORK OUTPUT SESSION (Intermediate Base Class)
# =============================================================================
# Adds network-specific state on top of OutputSession.
# NOT used directly - subclasses implement concrete network types.
# =============================================================================

NetworkOutputSession <- R6::R6Class(
  "NetworkOutputSession",
  inherit = OutputSession,

  public = list(
    #' Initialize a new NetworkOutputSession
    #'
    #' @param runKey Parent run key (e.g., "functional_gProfiler_1")
    #' @param enrichSession Parent ORAEnrichmentSession object
    #' @param outputType Type of network (e.g., "network1")
    initialize = function(runKey, enrichSession, outputType) {
      super$initialize(runKey, enrichSession, outputType)

      # Network-specific state (4 components like heatmaps have pairs/cells)
      private$.enrichmentData <- shiny::reactiveVal(NULL)
      private$.edgelistData <- shiny::reactiveVal(NULL)
      private$.nodes <- shiny::reactiveVal(NULL)
      private$.edges <- shiny::reactiveVal(NULL)

      # Current view for deselect restore
      private$.currentViewEnrichment <- shiny::reactiveVal(NULL)
      private$.currentViewEdgelist <- shiny::reactiveVal(NULL)

      # Arena edgelist for Arena3D export
      private$.arenaEdgelist <- shiny::reactiveVal(NULL)

      # Expected row counts for cascade detection
      private$.expectedEnrichmentRows <- NULL
      private$.expectedEdgelistRows <- NULL
    },

    #' Get arena edgelist for export
    #'
    #' @return Data frame or NULL
    getArenaEdgelist = function() {
      private$.arenaEdgelist()
    },

    #' Handle reset view button (override from base)
    #'
    #' Restores network and tables to original generated state.
    handleReset = function() {
      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) {
        shiny::showNotification("No data available. Generate the network first.",
                                type = "warning", duration = 3)
        return()
      }

      # Restore current views
      private$.currentViewEnrichment(enrichmentData)
      private$.currentViewEdgelist(edgelistData)

      # Render network and tables
      private$renderNetworkFromState()
      private$renderNetworkTablesFromState()
    },

    #' Clear for refresh (override from base)
    #'
    #' Clears network-specific state in addition to base state.
    clearForRefresh = function() {
      # Clear network-specific state first
      private$.enrichmentData(NULL)
      private$.edgelistData(NULL)
      private$.nodes(NULL)
      private$.edges(NULL)
      private$.currentViewEnrichment(NULL)
      private$.currentViewEdgelist(NULL)
      private$.arenaEdgelist(NULL)

      # Clear visible outputs
      if (!is.null(private$.output)) {
        tryCatch({
          private$.output$network <- visNetwork::renderVisNetwork({
            visNetwork::visNetwork(data.frame(), data.frame())
          })
          private$.output$table <- DT::renderDataTable(NULL)
          private$.output$edgelist <- DT::renderDataTable(NULL)
        }, error = function(e) NULL)
      }

      # Don't call super$clearForRefresh() - it tries to clear plotly outputs
      # Instead, clear base state manually
      private$.originalData(NULL)
      private$.currentView(NULL)
      private$.renderedData(NULL)
      private$.selectedTerms(character(0))
      private$.updateTracker$counter <- 0
      private$.updateTracker$source <- "refresh"
      private$.updateTracker$time <- Sys.time()
    },

    #' Clean up resources (override from base)
    #'
    #' Destroys observers and clears network-specific state.
    cleanup = function() {
      # Clear network-specific state first
      private$.enrichmentData(NULL)
      private$.edgelistData(NULL)
      private$.nodes(NULL)
      private$.edges(NULL)
      private$.currentViewEnrichment(NULL)
      private$.currentViewEdgelist(NULL)
      private$.arenaEdgelist(NULL)

      # Call parent cleanup (destroys observers, clears base state)
      super$cleanup()
    },

    #' Handle enrichment table filter
    handleEnrichmentTableFilter = function(filteredRows) {
      if (private$isExpectedRowCount("enrichment", filteredRows)) return()

      enrichmentData <- private$.enrichmentData()
      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()
      if (is.null(filteredRows) || length(filteredRows) == 0) return()
      if (max(filteredRows) > nrow(enrichmentData)) return()

      # All rows visible = restore full state
      if (length(filteredRows) == nrow(enrichmentData)) {
        edgelistData <- private$.edgelistData()
        private$.currentViewEnrichment(enrichmentData)
        private$.currentViewEdgelist(edgelistData)
        private$renderNetworkFromState()
        return()
      }

      filteredEnrichment <- enrichmentData[filteredRows, , drop = FALSE]
      filteredEdgelist <- private$getFilteredEdgelistFromEnrichment(filteredEnrichment)

      private$.currentViewEnrichment(filteredEnrichment)
      private$.currentViewEdgelist(filteredEdgelist)

      private$renderNetworkFromFilteredEnrichment(filteredEnrichment)
      private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
      private$renderEdgelistTableWithData(filteredEdgelist)
    },

    #' Handle edgelist table filter
    handleEdgelistTableFilter = function(filteredRows) {
      if (private$isExpectedRowCount("edgelist", filteredRows)) return()

      edgelistData <- private$.edgelistData()
      if (is.null(edgelistData) || nrow(edgelistData) == 0) return()
      if (is.null(filteredRows) || length(filteredRows) == 0) return()
      if (max(filteredRows) > nrow(edgelistData)) return()

      # All rows visible = restore full state
      if (length(filteredRows) == nrow(edgelistData)) {
        enrichmentData <- private$.enrichmentData()
        private$.currentViewEnrichment(enrichmentData)
        private$.currentViewEdgelist(edgelistData)
        private$renderNetworkFromState()
        return()
      }

      filteredEdgelist <- edgelistData[filteredRows, , drop = FALSE]
      filteredEnrichment <- private$getFilteredEnrichmentFromEdgelist(filteredEdgelist)

      private$.currentViewEnrichment(filteredEnrichment)
      private$.currentViewEdgelist(filteredEdgelist)

      private$renderNetworkFromFilteredEdgelist(filteredEdgelist)
      private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
      private$renderEnrichmentTableWithData(filteredEnrichment)
    },

    #' Handle network node click - subclasses must implement
    handleNodeClick = function(clickEvent) {
      stop("Subclass must implement handleNodeClick()")
    },

    #' Handle network edge click - subclasses must implement
    handleEdgeClick = function(clickEvent) {
      stop("Subclass must implement handleEdgeClick()")
    },

    #' Handle network deselection (empty space click)
    handleDeselection = function() {
      enrichmentData <- private$.currentViewEnrichment()
      edgelistData <- private$.currentViewEdgelist()

      # Fall back to full state if no current view
      if (is.null(enrichmentData)) {
        enrichmentData <- private$.enrichmentData()
        edgelistData <- private$.edgelistData()
      }

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      private$setExpectedRowCount("enrichment", nrow(enrichmentData))
      private$setExpectedRowCount("edgelist", nrow(edgelistData))
      private$renderEnrichmentTableWithData(enrichmentData)
      if (!is.null(edgelistData) && nrow(edgelistData) > 0) {
        private$renderEdgelistTableWithData(edgelistData)
      }
    },

    #' Handle multi-node selection - subclasses must implement
    handleSelection = function(selectedNodes) {
      stop("Subclass must implement handleSelection()")
    },

    # =========================================================================
    # Network-Specific UI Fragment Methods
    # =========================================================================
    # Network plots have additional controls beyond the base OutputSession.
    # Subclasses compose their ui() by calling these + base class fragments.

    #' Generate layout picker UI fragment
    #'
    #' Dropdown for selecting network layout algorithm.
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @return Shiny selectInput element
    layoutPickerUI = function(ns) {
      shiny::selectInput(
        inputId = ns("layoutSelect"),
        label = "Choose layout algorithm:",
        choices = as.vector(unlist(LAYOUT_CHOICES))
      )
    },

    #' Generate Arena3D export button UI fragment
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @return Shiny actionButton element
    arenaButtonUI = function(ns) {
      shiny::actionButton(
        inputId = ns("arenaBtn"),
        label = "Visualize 3D",
        class = "arena_button"
      )
    },

    #' Generate draw format radio buttons UI fragment
    #'
    #' Radio buttons for choosing between Term ID or Function name display.
    #' Used by Network1 and Network2 (not Network3 which only has genes).
    #'
    #' @param ns Namespace function from NS(self$id)
    #' @param uiTermKeyword Label term (e.g., "functions", "terms")
    #' @return Shiny radioButtons element
    drawFormatUI = function(ns, uiTermKeyword = "functions") {
      shiny::radioButtons(
        inputId = ns("drawFormat"),
        label = paste0("Choose format of ", uiTermKeyword, " to draw:"),
        choices = list("ID" = "Term_ID", "Name" = "Function"),
        inline = TRUE
      )
    }
  ),

  private = list(
    # Network-specific state
    .enrichmentData = NULL,
    .edgelistData = NULL,
    .nodes = NULL,
    .edges = NULL,
    .currentViewEnrichment = NULL,
    .currentViewEdgelist = NULL,
    .arenaEdgelist = NULL,

    # Expected row counts for cascade detection
    .expectedEnrichmentRows = NULL,
    .expectedEdgelistRows = NULL,

    # Network visualization config
    .networkHeight = "850px",
    .legendItems = list(
      list("GO:MF", "GO:BP", "GO:CC", "UNIPROT"),
      list("KEGG", "REAC", "WP", "PANTHER Pathways"),
      list("DO", "DISGENET", "OMIM", "GLAD4U_DISEASE", "ORPHA"),
      list("DRUGBANK", "GLAD4U_DRUG", "INTERPRO", "PFAM", "PUBMED"),
      list("BTO", "WBBT", "TF", "CollecTRI", "MIRNA", "CORUM"),
      list("HPA", "HP", "WBP", "MGI", "GENE")
    ),

    # Edge width scaling range (for network visualizations)
    .edgeWidthMin = 0.1,
    .edgeWidthMax = 3,

    # -------------------------------------------------------------------------
    # State Management
    # -------------------------------------------------------------------------

    #' Store all network state after generation
    setNetworkState = function(enrichmentData, edgelistData, nodes, edges) {
      private$.enrichmentData(enrichmentData)
      private$.edgelistData(edgelistData)
      private$.nodes(nodes)
      private$.edges(edges)
      private$.currentViewEnrichment(enrichmentData)
      private$.currentViewEdgelist(edgelistData)
      # Also store in base class for compatibility
      private$.originalData(enrichmentData)
      private$.currentView(enrichmentData)
    },

    #' Set expected row count for cascade detection
    setExpectedRowCount = function(tableType, count) {
      if (tableType == "enrichment") {
        private$.expectedEnrichmentRows <- count
      } else {
        private$.expectedEdgelistRows <- count
      }
    },

    #' Check if this is a cascade re-render (expected row count matches)
    isExpectedRowCount = function(tableType, filteredRows) {
      expected <- if (tableType == "enrichment") {
        private$.expectedEnrichmentRows
      } else {
        private$.expectedEdgelistRows
      }

      if (!is.null(expected) && length(filteredRows) == expected) {
        # Clear the expectation
        if (tableType == "enrichment") {
          private$.expectedEnrichmentRows <- NULL
        } else {
          private$.expectedEdgelistRows <- NULL
        }
        return(TRUE)
      }
      return(FALSE)
    },

    # -------------------------------------------------------------------------
    # Common Network Rendering
    # -------------------------------------------------------------------------

    #' Render visNetwork from state
    renderNetworkFromState = function() {
      nodes <- private$.nodes()
      edges <- private$.edges()
      if (is.null(nodes) || nrow(nodes) == 0) return()

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(nodes, edges, layout)
    },

    #' Render visNetwork with given nodes and edges
    #' Matches renderShinyVisNetwork exactly for colors, shapes, and interactivity
    renderVisNetwork = function(nodes, edges, layout) {
      # Get the namespace function for proper input ID namespacing
      ns <- private$.moduleSession$ns

      private$.output$network <- visNetwork::renderVisNetwork({
        set.seed(123)
        visNetwork::visNetwork(nodes = nodes, edges = edges, background = "white") %>%
          # Group colors and shapes - EXACT match to renderShinyVisNetwork
          visNetwork::visGroups(groupname = "GO:MF", color = DATASOURCE_COLORS["GO:MF"][[1]], shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "GO:BP", color = DATASOURCE_COLORS["GO:BP"][[1]], shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "GO:CC", color = DATASOURCE_COLORS["GO:CC"][[1]], shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "UNIPROT", color = DATASOURCE_COLORS["UNIPROT"][[1]], shape = "hexagon") %>%
          visNetwork::visGroups(groupname = "KEGG", color = DATASOURCE_COLORS["KEGG"][[1]], shape = "diamond") %>%
          visNetwork::visGroups(groupname = "REAC", color = DATASOURCE_COLORS["REAC"][[1]], shape = "diamond") %>%
          visNetwork::visGroups(groupname = "WP", color = DATASOURCE_COLORS["WP"][[1]], shape = "diamond") %>%
          visNetwork::visGroups(groupname = "PANTHER Pathways", color = DATASOURCE_COLORS["PANTHER Pathways"][[1]], shape = "diamond") %>%
          visNetwork::visGroups(groupname = "DO", color = DATASOURCE_COLORS["DO"][[1]], shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "DISGENET", color = DATASOURCE_COLORS["DISGENET"][[1]], shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "OMIM", color = DATASOURCE_COLORS["OMIM"][[1]], shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "GLAD4U_DISEASE", color = DATASOURCE_COLORS["GLAD4U_DISEASE"][[1]], shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "ORPHA", color = DATASOURCE_COLORS["ORPHA"][[1]], shape = "triangleDown") %>%
          visNetwork::visGroups(groupname = "DRUGBANK", color = DATASOURCE_COLORS["DRUGBANK"][[1]], shape = "star") %>%
          visNetwork::visGroups(groupname = "GLAD4U_DRUG", color = DATASOURCE_COLORS["GLAD4U_DRUG"][[1]], shape = "star") %>%
          visNetwork::visGroups(groupname = "INTERPRO", color = DATASOURCE_COLORS["INTERPRO"][[1]], shape = "star") %>%
          visNetwork::visGroups(groupname = "PFAM", color = DATASOURCE_COLORS["PFAM"][[1]], shape = "star") %>%
          visNetwork::visGroups(groupname = "BTO", color = DATASOURCE_COLORS["BTO"][[1]], shape = "triangle") %>%
          visNetwork::visGroups(groupname = "WBBT", color = DATASOURCE_COLORS["WBBT"][[1]], shape = "triangle") %>%
          visNetwork::visGroups(groupname = "TF", color = DATASOURCE_COLORS["TF"][[1]], shape = "triangle") %>%
          visNetwork::visGroups(groupname = "CollecTRI", color = DATASOURCE_COLORS["CollecTRI"][[1]], shape = "triangle") %>%
          visNetwork::visGroups(groupname = "MIRNA", color = DATASOURCE_COLORS["MIRNA"][[1]], shape = "triangle") %>%
          visNetwork::visGroups(groupname = "CORUM", color = DATASOURCE_COLORS["CORUM"][[1]], shape = "triangle") %>%
          visNetwork::visGroups(groupname = "HPA", color = DATASOURCE_COLORS["HPA"][[1]], shape = "square") %>%
          visNetwork::visGroups(groupname = "HP", color = DATASOURCE_COLORS["HP"][[1]], shape = "square") %>%
          visNetwork::visGroups(groupname = "WBP", color = DATASOURCE_COLORS["WBP"][[1]], shape = "square") %>%
          visNetwork::visGroups(groupname = "MGI", color = DATASOURCE_COLORS["MGI"][[1]], shape = "square") %>%
          visNetwork::visGroups(groupname = "Gene", color = GENE_NODE_COLOR, shape = "square") %>%
          visNetwork::visGroups(groupname = "PUBMED", color = DATASOURCE_COLORS["PUBMED"][[1]], shape = "square") %>%
          visNetwork::visEdges(color = "black") %>%
          visNetwork::visIgraphLayout(layout = layout) %>%
          visNetwork::visInteraction(navigationButtons = TRUE, hover = TRUE, multiselect = TRUE) %>%
          # Event callbacks with setTimeout wrapper - matches original exactly
          visNetwork::visEvents(
            click = sprintf("function(params) {
              setTimeout(function() {
                Shiny.setInputValue('%s', params, {priority: 'event'});
              }, 0);
            }", ns("network_click")),
            select = sprintf("function(params) {
              setTimeout(function() {
                Shiny.setInputValue('%s', params.nodes, {priority: 'event'});
              }, 0);
            }", ns("network_selected")),
            deselectNode = sprintf("function(params) {
              setTimeout(function() {
                Shiny.setInputValue('%s', params, {priority: 'event'});
              }, 0);
            }", ns("network_deselect"))
          )
      })
    },

    #' Get selected layout from input
    getSelectedLayout = function() {
      layoutInput <- shiny::isolate(private$.moduleSession$input$layoutSelect)
      if (is.null(layoutInput)) return("layout_nicely")
      names(LAYOUT_CHOICES)[match(layoutInput, LAYOUT_CHOICES)]
    },

    #' Render network tables from state
    renderNetworkTablesFromState = function() {
      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (!is.null(enrichmentData) && nrow(enrichmentData) > 0) {
        private$setExpectedRowCount("enrichment", nrow(enrichmentData))
        private$renderEnrichmentTableWithData(enrichmentData)
      }
      if (!is.null(edgelistData) && nrow(edgelistData) > 0) {
        private$setExpectedRowCount("edgelist", nrow(edgelistData))
        private$renderEdgelistTableWithData(edgelistData)
      }
    },

    #' Render enrichment table with data
    renderEnrichmentTableWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      data$Source <- as.factor(data$Source)
      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)

      # Use the encapsulated internal method from OutputSession base
      private$renderEnrichmentTableInternal(
        data = data,
        caption = "Enrichment Results",
        fileName = paste(self$id, paste(sourceSelect, collapse = "_"), sep = "_"),
        mode = "Positive Hits",
        hiddenColumns = c(10, 11),
        expandableColumn = 10,
        filter = "top"
      )
    },

    #' Render edgelist table with data
    renderEdgelistTableWithData = function(data) {
      if (is.null(data) || nrow(data) == 0) return()

      sourceSelect <- shiny::isolate(private$.moduleSession$input$sourceSelect)
      fileName <- paste(self$id, paste(sourceSelect, collapse = "_"), "edgelist", sep = "_")

      private$.output$edgelist <- DT::renderDataTable(
        data,
        server = FALSE,
        selection = "none",
        extensions = "Buttons",
        caption = "Edgelist",
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          dom = "Blfiprt",
          buttons = createExportButtons(fileName, c())
        ),
        filter = "top",
        rownames = FALSE,
        escape = FALSE
      )
    },

    # -------------------------------------------------------------------------
    # Network Construction Helpers
    # -------------------------------------------------------------------------

    #' Create graph from edgelist
    createGraph = function(edgelist, sourceCol, targetCol, weightCol = NULL) {
      graph <- igraph::graph_from_edgelist(
        as.matrix(edgelist[, c(sourceCol, targetCol)]),
        directed = FALSE
      )
      if (!is.null(weightCol)) {
        igraph::E(graph)$weight <- edgelist[[weightCol]]
      } else {
        igraph::E(graph)$weight <- 1
      }
      return(graph)
    },

    #' Convert graph to visNetwork data
    graphToVisNetworkData = function(graph) {
      data <- visNetwork::toVisNetworkData(graph)
      nodes <- data$nodes
      row.names(nodes) <- NULL
      nodes$font.size <- 24
      edges <- data$edges
      edges$id <- seq_len(nrow(edges))  # Required for edge click events
      edges <- private$appendEdgeWidth(edges)
      edges$color <- "black"
      return(list(nodes = nodes, edges = edges))
    },

    #' Map numeric values to a new range (for edge width scaling)
    mapRange = function(numericList, newMin, newMax) {
      oldRange <- max(numericList) - min(numericList)
      if (oldRange != 0)
        numericList <- (((numericList - min(numericList)) * (newMax - newMin)) / oldRange) + newMin
      else
        numericList <- rep(
          (private$.edgeWidthMin - private$.edgeWidthMin) / 2,
          length(numericList)
        )
      return(numericList)
    },

    #' Append width to edges based on weight
    appendEdgeWidth = function(edges) {
      if (nrow(edges) > 0 && "weight" %in% names(edges)) {
        edges$width <- private$mapRange(edges$weight,
                                        newMin = private$.edgeWidthMin, newMax = private$.edgeWidthMax)
        edges$title <- as.character(edges$weight)
      } else {
        edges$width <- 1
      }
      return(edges)
    },

    # -------------------------------------------------------------------------
    # Abstract Methods (subclasses must implement)
    # -------------------------------------------------------------------------

    #' Filter enrichment data (shared by all network types)
    #'
    #' Now supports multi-select datasources.
    filterEnrichmentData = function(results, sourceSelect, sortMode, termCount) {
      filteredData <- subset(results, Source %in% sourceSelect)
      if (nrow(filteredData) == 0) return(NULL)

      if (sortMode == "Enrichment Score") {
        filteredData <- filteredData[order(-filteredData$`Enrichment Score %`), ]
      } else {
        filteredData <- filteredData[order(-filteredData$`-log10Pvalue`), ]
      }

      filteredData <- head(filteredData, termCount)
      filteredData$`Positive Hits` <- gsub(",", ", ", filteredData$`Positive Hits`)
      return(filteredData)
    },

    #' Get filtered edgelist from filtered enrichment (subclass-specific)
    getFilteredEdgelistFromEnrichment = function(filteredEnrichment) {
      stop("Subclass must implement getFilteredEdgelistFromEnrichment()")
    },

    #' Get filtered enrichment from filtered edgelist (subclass-specific)
    getFilteredEnrichmentFromEdgelist = function(filteredEdgelist) {
      stop("Subclass must implement getFilteredEnrichmentFromEdgelist()")
    },

    #' Render network from filtered enrichment (subclass-specific)
    renderNetworkFromFilteredEnrichment = function(filteredEnrichment) {
      stop("Subclass must implement renderNetworkFromFilteredEnrichment()")
    },

    #' Render network from filtered edgelist (subclass-specific)
    renderNetworkFromFilteredEdgelist = function(filteredEdgelist) {
      stop("Subclass must implement renderNetworkFromFilteredEdgelist()")
    },

    #' Initialize picker and slider controls
    #'
    #' Populates datasource picker and sets term count slider range.
    #' Uses multi-select with all sources selected by default.
    initializeControls = function(session) {
      results <- self$enrichSession$getResults()
      if (is.null(results) || nrow(results) == 0) return()

      sources <- unique(as.character(results$Source))

      shinyWidgets::updatePickerInput(
        session,
        "sourceSelect",
        choices = sources,
        selected = sources  # Multi-select: select all by default
      )

      maxRows <- nrow(results)
      shiny::updateSliderInput(
        session,
        "termCountSlider",
        max = min(maxRows, private$.maxSliderValue),
        value = min(10, maxRows)
      )
    },

    # Not used for networks (base class abstract method)
    renderBothFromCurrentView = function() {
      private$renderNetworkFromState()
      private$renderNetworkTablesFromState()
    },

    #' Generate color coding legend for network visualization
    #'
    #' Creates a collapsible legend box showing datasource colors and shapes.
    #' Encapsulated from func-tabGeneration.R.
    #'
    #' @return Shiny UI elements (fluidRow with legend box)
    generateColorLegend = function() {
      shiny::fluidRow(
        do.call(
          shinydashboard::box, c(
            class = "legend", title = "Legend", status = "primary",
            solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            lapply(private$.legendItems, function(legendList) {
              do.call(
                shiny::column, c(
                  width = 2,
                  lapply(legendList, function(source) {
                    fontColor <- "white"
                    if (source %in% c("GENE", "DO", "GO:BP", "UNIPROT",
                                      "DISGENET", "OMIM", "BTO", "WBP"))
                      fontColor <- "black"
                    shiny::tags$div(
                      shiny::tags$p(source),
                      style = paste0(
                        "background-color: ", DATASOURCE_COLORS[source][[1]],
                        "; color: ", fontColor, ";"
                      )
                    )
                  })
                )
              )}
            )
          )
        )
      )
    }
  )
)


# =============================================================================
# NETWORK1 OUTPUT SESSION (Function vs Gene)
# =============================================================================

Network1OutputSession <- R6::R6Class(
  "Network1OutputSession",
  inherit = NetworkOutputSession,

  public = list(
    #' Initialize
    initialize = function(runKey, enrichSession) {
      super$initialize(runKey, enrichSession, outputType = "network1")
    },

    #' Generate namespaced UI
    #'
    #' Composes UI from base class fragments.
    ui = function() {
      ns <- shiny::NS(self$id)
      enrichmentType <- "functional"
      uiTermKeyword <- UI_TERM_KEYWORD[[enrichmentType]]

      shiny::tagList(
        shiny::tags$br(),
        # Row 1: Datasource, Slider, Mode
        shiny::fluidRow(
          shiny::column(4, self$datasourcePickerUI(ns)),
          shiny::column(4, self$termCountSliderUI(ns, uiTermKeyword)),
          shiny::column(4, self$sortModeUI(ns, uiTermKeyword))
        ),
        # Row 2: Arena, Layout (network1 has no threshold slider)
        shiny::fluidRow(
          shiny::column(4, self$arenaButtonUI(ns)),
          shiny::column(4, self$layoutPickerUI(ns)),
          shiny::column(4)  # Empty column
        ),
        # Row 3: Generate, Reset, DrawFormat
        shiny::fluidRow(
          shiny::column(
            4,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          ),
          shiny::column(4, self$drawFormatUI(ns, uiTermKeyword))
        ),
        shiny::tags$hr(),
        # Network output
        shiny::tags$div(
          class = "networkOutput",
          visNetwork::visNetworkOutput(ns("network"), height = private$.networkHeight)
        ),
        shiny::tags$br(),
        # Color legend and edgelist
        private$generateColorLegend(),
        DT::dataTableOutput(ns("edgelist")),
        shiny::tags$br(),
        # Enrichment table
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate network
    generate = function(sourceSelect, sortMode, termCount, drawFormat) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Network.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for network.")
          return()
        }

        # Filter enrichment data
        enrichmentData <- private$filterEnrichmentData(results, sourceSelect, sortMode, termCount)
        if (is.null(enrichmentData) || nrow(enrichmentData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Create edgelist (Function vs Gene)
        edgelist <- private$separateRows(enrichmentData)
        edgelist <- private$keepEdgelistColumns(edgelist)

        # Store for Arena3D
        private$.arenaEdgelist(edgelist)

        # Render edgelist table
        private$renderEdgelistTableWithData(edgelist)

        # Render enrichment table
        private$renderEnrichmentTableWithData(enrichmentData)

        # Build network
        networkData <- private$constructVisNetwork(edgelist, drawFormat)

        # Store state
        private$setNetworkState(enrichmentData, edgelist, networkData$nodes, networkData$edges)

      }, error = function(e) {
        cat("[NetworkOutputSession generate] Error:", conditionMessage(e), "\n")
        print(e)
        renderWarning(paste("Network error:", conditionMessage(e)))
      }, finally = {
        removeModal()
      })
    },

    #' Handle node click
    handleNodeClick = function(clickEvent) {
      if (is.null(clickEvent) || is.null(clickEvent$nodes) ||
          length(clickEvent$nodes) == 0) {
        # Empty space click
        if (is.null(clickEvent$edges) || length(clickEvent$edges) == 0) {
          self$handleDeselection()
        }
        return()
      }

      nodeId <- clickEvent$nodes[[1]]
      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      # Determine if clicked node is term or gene
      isTerm <- nodeId %in% enrichmentData$Term_ID_noLinks ||
                nodeId %in% enrichmentData$Function

      if (isTerm) {
        filteredEnrichment <- enrichmentData[
          enrichmentData$Term_ID_noLinks == nodeId |
          enrichmentData$Function == nodeId, , drop = FALSE
        ]
        filteredEdgelist <- edgelistData[
          edgelistData$`Source Id` == nodeId |
          edgelistData$`Source Name` == nodeId, , drop = FALSE
        ]
      } else {
        # Gene node
        filteredEnrichment <- enrichmentData[
          grepl(nodeId, enrichmentData$`Positive Hits`, fixed = TRUE), , drop = FALSE
        ]
        filteredEdgelist <- edgelistData[
          edgelistData$`Target Gene` == nodeId, , drop = FALSE
        ]
      }

      if (nrow(filteredEnrichment) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Handle edge click
    handleEdgeClick = function(clickEvent) {
      if (is.null(clickEvent) || is.null(clickEvent$edges) ||
          length(clickEvent$edges) == 0) return()
      if (!is.null(clickEvent$nodes) && length(clickEvent$nodes) > 0) return()

      edgeId <- clickEvent$edges[[1]]
      edges <- private$.edges()
      if (is.null(edges) || nrow(edges) == 0) return()

      edgeRow <- edges[edges$id == edgeId, , drop = FALSE]
      if (nrow(edgeRow) == 0) return()

      fromNode <- edgeRow$from[1]
      toNode <- edgeRow$to[1]

      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      # Term-Gene edge
      filteredEdgelist <- edgelistData[
        (edgelistData$`Source Id` == fromNode & edgelistData$`Target Gene` == toNode) |
        (edgelistData$`Source Id` == toNode & edgelistData$`Target Gene` == fromNode) |
        (edgelistData$`Source Name` == fromNode & edgelistData$`Target Gene` == toNode) |
        (edgelistData$`Source Name` == toNode & edgelistData$`Target Gene` == fromNode),
        , drop = FALSE
      ]

      termNodes <- c(fromNode, toNode)
      filteredEnrichment <- enrichmentData[
        enrichmentData$Term_ID_noLinks %in% termNodes |
        enrichmentData$Function %in% termNodes, , drop = FALSE
      ]

      if (nrow(filteredEdgelist) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Handle multi-node selection
    handleSelection = function(selectedNodes) {
      if (is.null(selectedNodes) || length(selectedNodes) <= 1) return()

      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      # Separate terms and genes
      termNodes <- selectedNodes[selectedNodes %in% enrichmentData$Term_ID_noLinks |
                                 selectedNodes %in% enrichmentData$Function]
      geneNodes <- setdiff(selectedNodes, termNodes)

      termMatch <- enrichmentData$Term_ID_noLinks %in% termNodes |
                   enrichmentData$Function %in% termNodes
      geneMatch <- sapply(enrichmentData$`Positive Hits`, function(hits) {
        genes <- unlist(strsplit(as.character(hits), ",\\s*"))
        any(genes %in% geneNodes)
      })
      filteredEnrichment <- enrichmentData[termMatch | geneMatch, , drop = FALSE]

      filteredEdgelist <- edgelistData[
        edgelistData$`Source Id` %in% selectedNodes |
        edgelistData$`Source Name` %in% selectedNodes |
        edgelistData$`Target Gene` %in% selectedNodes, , drop = FALSE
      ]

      if (nrow(filteredEnrichment) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Set up server logic
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Initialize controls (populates picker, sets slider range)
        private$initializeControls(session)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            if (!is.null(input$sourceSelect)) {
              self$generate(input$sourceSelect, input$sortMode, input$termCountSlider, input$drawFormat)
            }
          },
          ignoreInit = TRUE
        )

        # Reset button
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          { self$handleReset() },
          ignoreInit = TRUE
        )

        # Arena button
        private$.observers$arena <- shiny::observeEvent(
          input$arenaBtn,
          { private$exportToArena() },
          ignoreInit = TRUE
        )

        # Network click (uses namespaced input from JavaScript)
        private$.observers$click <- shiny::observeEvent(
          input$network_click,
          {
            clickEvent <- input$network_click
            self$handleNodeClick(clickEvent)
            self$handleEdgeClick(clickEvent)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Multi-select (uses namespaced input from JavaScript)
        private$.observers$selected <- shiny::observeEvent(
          input$network_selected,
          {
            selectedNodes <- input$network_selected
            self$handleSelection(selectedNodes)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Deselect (uses namespaced input from JavaScript)
        private$.observers$deselect <- shiny::observeEvent(
          input$network_deselect,
          { self$handleDeselection() },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Enrichment table filter
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          { self$handleEnrichmentTableFilter(input$table_rows_all) },
          ignoreInit = TRUE
        )

        # Edgelist table filter
        private$.observers$edgelistFilter <- shiny::observeEvent(
          input$edgelist_rows_all,
          { self$handleEdgelistTableFilter(input$edgelist_rows_all) },
          ignoreInit = TRUE
        )
      })
    }
  ),

  private = list(
    #' Separate rows - explode Positive Hits
    separateRows = function(enrichmentData) {
      enrichmentData <- enrichmentData[, c(
        "Source", "Term_ID", "Term_ID_noLinks", "Function", "Positive Hits",
        "Enrichment Score %", "-log10Pvalue", "Intersection Size")]
      tidyr::separate_rows(enrichmentData, `Positive Hits`, sep = ", ")
    },

    #' Keep only edgelist columns
    keepEdgelistColumns = function(data) {
      data <- data[, c("Source", "Term_ID_noLinks", "Function", "Positive Hits")]
      colnames(data) <- c("Source Database", "Source Id", "Source Name", "Target Gene")
      return(data)
    },

    #' Construct visNetwork
    constructVisNetwork = function(edgelist, drawFormat) {
      graph <- private$createGraph(edgelist, "Source Id", "Target Gene", NULL)
      data <- private$graphToVisNetworkData(graph)
      nodes <- data$nodes
      edges <- data$edges

      # Assign groups and titles
      nodes$group <- edgelist$`Source Database`[match(nodes$label, edgelist$`Source Id`)]
      nodes$title <- edgelist$`Source Name`[match(nodes$label, edgelist$`Source Id`)]
      nodes$group[is.na(nodes$group)] <- "Gene"

      # Swap labels if drawFormat is Function
      if (drawFormat == "Function") {
        nonGeneIdx <- which(nodes$group != "Gene")
        if (length(nonGeneIdx) > 0) {
          temp <- nodes$label[nonGeneIdx]
          nodes$label[nonGeneIdx] <- nodes$title[nonGeneIdx]
          nodes$title[nonGeneIdx] <- temp
        }
      }

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(nodes, edges, layout)

      return(list(nodes = nodes, edges = edges))
    },

    #' Get filtered edgelist from filtered enrichment
    getFilteredEdgelistFromEnrichment = function(filteredEnrichment) {
      edgelistData <- private$.edgelistData()
      if (is.null(edgelistData) || nrow(edgelistData) == 0) return(NULL)

      visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
      visibleTermNames <- unique(filteredEnrichment$Function)

      edgelistData[
        edgelistData$`Source Id` %in% visibleTermIds |
        edgelistData$`Source Name` %in% visibleTermNames, , drop = FALSE
      ]
    },

    #' Get filtered enrichment from filtered edgelist
    getFilteredEnrichmentFromEdgelist = function(filteredEdgelist) {
      enrichmentData <- private$.enrichmentData()
      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return(NULL)

      referencedTermIds <- unique(c(
        filteredEdgelist$`Source Id`,
        filteredEdgelist$`Source Name`
      ))

      enrichmentData[
        enrichmentData$Term_ID_noLinks %in% referencedTermIds |
        enrichmentData$Function %in% referencedTermIds, , drop = FALSE
      ]
    },

    #' Render network from filtered enrichment
    renderNetworkFromFilteredEnrichment = function(filteredEnrichment) {
      nodes <- private$.nodes()
      edges <- private$.edges()
      edgelistData <- private$.edgelistData()

      if (is.null(nodes) || nrow(nodes) == 0) return()

      visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
      visibleTermNames <- unique(filteredEnrichment$Function)

      filteredEdgelist <- edgelistData[
        edgelistData$`Source Id` %in% visibleTermIds |
        edgelistData$`Source Name` %in% visibleTermNames, , drop = FALSE
      ]
      visibleGenes <- unique(filteredEdgelist$`Target Gene`)

      filteredNodes <- nodes[
        nodes$id %in% visibleTermIds |
        nodes$id %in% visibleTermNames |
        nodes$label %in% visibleTermIds |
        nodes$label %in% visibleTermNames |
        nodes$id %in% visibleGenes |
        nodes$label %in% visibleGenes, , drop = FALSE
      ]

      filteredEdges <- edges[
        edges$from %in% filteredNodes$id & edges$to %in% filteredNodes$id, , drop = FALSE
      ]

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(filteredNodes, filteredEdges, layout)
    },

    #' Render network from filtered edgelist
    renderNetworkFromFilteredEdgelist = function(filteredEdgelist) {
      nodes <- private$.nodes()
      edges <- private$.edges()

      if (is.null(nodes) || nrow(nodes) == 0) return()

      referencedNodes <- unique(c(
        filteredEdgelist$`Source Id`,
        filteredEdgelist$`Source Name`,
        filteredEdgelist$`Target Gene`
      ))

      validEdgePairs <- unique(paste(
        ifelse(!is.na(filteredEdgelist$`Source Id`),
               filteredEdgelist$`Source Id`, filteredEdgelist$`Source Name`),
        filteredEdgelist$`Target Gene`, sep = "|||"
      ))

      filteredNodes <- nodes[
        nodes$id %in% referencedNodes |
        nodes$label %in% referencedNodes, , drop = FALSE
      ]

      edgePairsForward <- paste(edges$from, edges$to, sep = "|||")
      edgePairsReverse <- paste(edges$to, edges$from, sep = "|||")
      filteredEdges <- edges[
        edgePairsForward %in% validEdgePairs | edgePairsReverse %in% validEdgePairs,
        , drop = FALSE
      ]

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(filteredNodes, filteredEdges, layout)
    },

    #' Export to Arena3D
    exportToArena = function() {
      edgelist <- private$.arenaEdgelist()
      if (is.null(edgelist) || nrow(edgelist) == 0) {
        renderWarning("Make sure a visible network exists.")
        return()
      }

      tryCatch({
        renderModal("<h2>Please wait.</h2><p>Building network for Arena3Dweb</p>")
        result <- arena_export(edgelist)

        if (result$success) {
          private$.moduleSession$sendCustomMessage("handler_browseUrl", result$url)
        } else {
          renderWarning(result$error)
        }
      }, error = function(e) {
        renderWarning("Cannot open Arena3Dweb network at this time.")
      }, finally = {
        removeModal()
      })
    }
  )
)


# =============================================================================
# NETWORK2 OUTPUT SESSION (Function vs Function)
# =============================================================================

Network2OutputSession <- R6::R6Class(
  "Network2OutputSession",
  inherit = NetworkOutputSession,

  public = list(
    #' Initialize
    initialize = function(runKey, enrichSession) {
      super$initialize(runKey, enrichSession, outputType = "network2")
    },

    #' Generate namespaced UI
    #'
    #' Composes UI from base class fragments.
    #' Network2 has similarity cutoff slider (unique to this type).
    ui = function() {
      ns <- shiny::NS(self$id)
      enrichmentType <- "functional"
      uiTermKeyword <- UI_TERM_KEYWORD[[enrichmentType]]

      shiny::tagList(
        shiny::tags$br(),
        # Row 1: Datasource, Slider, Mode
        shiny::fluidRow(
          shiny::column(4, self$datasourcePickerUI(ns)),
          shiny::column(4, self$termCountSliderUI(ns, uiTermKeyword)),
          shiny::column(4, self$sortModeUI(ns, uiTermKeyword))
        ),
        # Row 2: Arena, Layout, Similarity cutoff
        shiny::fluidRow(
          shiny::column(4, self$arenaButtonUI(ns)),
          shiny::column(4, self$layoutPickerUI(ns)),
          shiny::column(
            4,
            # Similarity cutoff - unique to Network2
            shiny::tags$div(
              class = "drawUp",
              shiny::sliderInput(
                inputId = ns("similarityCutoff"),
                label = "Similarity score cut-off (%):",
                min = 1, max = 100, value = 10, step = 1
              )
            )
          )
        ),
        # Row 3: Generate, Reset, DrawFormat
        shiny::fluidRow(
          shiny::column(
            4,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          ),
          shiny::column(4, self$drawFormatUI(ns, uiTermKeyword))
        ),
        shiny::tags$hr(),
        # Network output
        shiny::tags$div(
          class = "networkOutput",
          visNetwork::visNetworkOutput(ns("network"), height = private$.networkHeight)
        ),
        shiny::tags$br(),
        # Color legend and edgelist
        private$generateColorLegend(),
        DT::dataTableOutput(ns("edgelist")),
        shiny::tags$br(),
        # Enrichment table
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate network
    generate = function(sourceSelect, sortMode, termCount, similarityCutoff, drawFormat) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Network.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for network.")
          return()
        }

        # Filter enrichment data
        enrichmentData <- private$filterEnrichmentData(results, sourceSelect, sortMode, termCount)
        if (is.null(enrichmentData) || nrow(enrichmentData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Create Function vs Function edgelist with similarity
        edgelist <- private$extractFunctionVsFunctionEdgelist(
          enrichmentData, similarityCutoff, simplifyForNetwork = TRUE
        )

        if (is.null(edgelist) || nrow(edgelist) == 0) {
          renderWarning("The current filters cannot produce enough edges. Please adjust the threshold.")
          return()
        }

        # Store for Arena3D
        private$.arenaEdgelist(edgelist)

        # Render tables
        private$renderEdgelistTableWithData(edgelist)
        private$renderEnrichmentTableWithData(enrichmentData)

        # Build network
        networkData <- private$constructVisNetwork(edgelist, drawFormat)

        # Store state
        private$setNetworkState(enrichmentData, edgelist, networkData$nodes, networkData$edges)

      }, error = function(e) {
        cat("[NetworkOutputSession generate] Error:", conditionMessage(e), "\n")
        print(e)
        renderWarning(paste("Network error:", conditionMessage(e)))
      }, finally = {
        removeModal()
      })
    },

    #' Handle node click
    handleNodeClick = function(clickEvent) {
      if (is.null(clickEvent) || is.null(clickEvent$nodes) ||
          length(clickEvent$nodes) == 0) {
        if (is.null(clickEvent$edges) || length(clickEvent$edges) == 0) {
          self$handleDeselection()
        }
        return()
      }

      nodeId <- clickEvent$nodes[[1]]
      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      # Get all edges connected to this term
      filteredEdgelist <- edgelistData[
        edgelistData$`Source Id` == nodeId |
        edgelistData$`Source Name` == nodeId |
        edgelistData$`Target Id` == nodeId |
        edgelistData$`Target Name` == nodeId, , drop = FALSE
      ]

      # Collect all connected term IDs
      connectedTermIds <- unique(c(
        nodeId,
        filteredEdgelist$`Source Id`,
        filteredEdgelist$`Target Id`
      ))

      filteredEnrichment <- enrichmentData[
        enrichmentData$Term_ID_noLinks %in% connectedTermIds |
        enrichmentData$Function %in% connectedTermIds, , drop = FALSE
      ]

      if (nrow(filteredEdgelist) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Handle edge click
    handleEdgeClick = function(clickEvent) {
      if (is.null(clickEvent) || is.null(clickEvent$edges) ||
          length(clickEvent$edges) == 0) return()
      if (!is.null(clickEvent$nodes) && length(clickEvent$nodes) > 0) return()

      edgeId <- clickEvent$edges[[1]]
      edges <- private$.edges()
      if (is.null(edges) || nrow(edges) == 0) return()

      edgeRow <- edges[edges$id == edgeId, , drop = FALSE]
      if (nrow(edgeRow) == 0) return()

      fromNode <- edgeRow$from[1]
      toNode <- edgeRow$to[1]

      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      # Term-Term edge
      filteredEdgelist <- edgelistData[
        (edgelistData$`Source Id` == fromNode & edgelistData$`Target Id` == toNode) |
        (edgelistData$`Source Id` == toNode & edgelistData$`Target Id` == fromNode) |
        (edgelistData$`Source Name` == fromNode & edgelistData$`Target Name` == toNode) |
        (edgelistData$`Source Name` == toNode & edgelistData$`Target Name` == fromNode),
        , drop = FALSE
      ]

      termNodes <- c(fromNode, toNode)
      filteredEnrichment <- enrichmentData[
        enrichmentData$Term_ID_noLinks %in% termNodes |
        enrichmentData$Function %in% termNodes, , drop = FALSE
      ]

      if (nrow(filteredEdgelist) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Handle multi-node selection
    handleSelection = function(selectedNodes) {
      if (is.null(selectedNodes) || length(selectedNodes) <= 1) return()

      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      filteredEnrichment <- enrichmentData[
        enrichmentData$Term_ID_noLinks %in% selectedNodes |
        enrichmentData$Function %in% selectedNodes, , drop = FALSE
      ]

      filteredEdgelist <- edgelistData[
        edgelistData$`Source Id` %in% selectedNodes |
        edgelistData$`Source Name` %in% selectedNodes |
        edgelistData$`Target Id` %in% selectedNodes |
        edgelistData$`Target Name` %in% selectedNodes, , drop = FALSE
      ]

      if (nrow(filteredEnrichment) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Set up server logic
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Initialize controls (populates picker, sets slider range)
        private$initializeControls(session)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            if (!is.null(input$sourceSelect)) {
              self$generate(input$sourceSelect, input$sortMode, input$termCountSlider,
                            input$similarityCutoff, input$drawFormat)
            }
          },
          ignoreInit = TRUE
        )

        # Reset button
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          { self$handleReset() },
          ignoreInit = TRUE
        )

        # Arena button
        private$.observers$arena <- shiny::observeEvent(
          input$arenaBtn,
          { private$exportToArena() },
          ignoreInit = TRUE
        )

        # Network click (uses namespaced input from JavaScript)
        private$.observers$click <- shiny::observeEvent(
          input$network_click,
          {
            clickEvent <- input$network_click
            self$handleNodeClick(clickEvent)
            self$handleEdgeClick(clickEvent)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Multi-select (uses namespaced input from JavaScript)
        private$.observers$selected <- shiny::observeEvent(
          input$network_selected,
          {
            selectedNodes <- input$network_selected
            self$handleSelection(selectedNodes)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Deselect (uses namespaced input from JavaScript)
        private$.observers$deselect <- shiny::observeEvent(
          input$network_deselect,
          { self$handleDeselection() },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Enrichment table filter
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          { self$handleEnrichmentTableFilter(input$table_rows_all) },
          ignoreInit = TRUE
        )

        # Edgelist table filter
        private$.observers$edgelistFilter <- shiny::observeEvent(
          input$edgelist_rows_all,
          { self$handleEdgelistTableFilter(input$edgelist_rows_all) },
          ignoreInit = TRUE
        )
      })
    }
  ),

  private = list(
    #' Initialize picker and slider controls (override to include similarityCutoff)
    initializeControls = function(session) {
      # Call parent to initialize sourceSelect and termCountSlider
      super$initializeControls(session)

      # Also reset similarityCutoff to default
      shiny::updateSliderInput(
        session,
        "similarityCutoff",
        value = 10  # Default value
      )
    },

    #' Extract Function vs Function edgelist with similarity calculation
    extractFunctionVsFunctionEdgelist = function(enrichmentData, similarityCutoff, simplifyForNetwork = FALSE) {
      functionsEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
      totalGenesEdgelist <- private$calculateEdgeTotalGenes(functionsEdgelist)
      commonGenesEdgelist <- private$calculateEdgeCommonGenes(functionsEdgelist)
      functionsEdgelist <- merge(
        commonGenesEdgelist, totalGenesEdgelist,
        by = c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
      )
      functionsEdgelist <- private$calculateSimilarityScore(functionsEdgelist)

      if (simplifyForNetwork) {
        functionsEdgelist <- private$removeDuplicateSelfAndOppositeEdges(
          functionsEdgelist, "Similarity Score %")
      } else {
        functionsEdgelist <- private$tuneForHeatmap(functionsEdgelist)
      }

      if (!is.null(similarityCutoff)) {
        functionsEdgelist <- functionsEdgelist[
          functionsEdgelist$`Similarity Score %` >= similarityCutoff, , drop = FALSE
        ]
      }

      functionsEdgelist <- private$appendSourceDatabasesAndIds(enrichmentData, functionsEdgelist)
      functionsEdgelist <- functionsEdgelist[order(-functionsEdgelist$`Similarity Score %`), ]
      return(functionsEdgelist)
    },

    calculateEdgeTotalGenes = function(edgelist) {
      edgelistCopy <- edgelist
      colnames(edgelistCopy) <- c("TermsCopy", "HitsCopy")
      edgelist <- merge(edgelist, edgelistCopy)
      edgelist$`Positive Hits` <- paste(edgelist$`Positive Hits`, edgelist$HitsCopy, sep = ", ")
      edgelist$HitsCopy <- NULL
      edgelist <- tidyr::separate_rows(edgelist, `Positive Hits`, sep = ", ")
      edgelist <- dplyr::distinct(edgelist)
      edgelist$`Positive Hits` <- NULL
      edgelist <- data.table::setDT(edgelist)[, list(`Total Genes` = .N), names(edgelist)]
      colnames(edgelist)[1:2] <- c("Term_ID_noLinks.x", "Term_ID_noLinks.y")
      return(edgelist)
    },

    calculateEdgeCommonGenes = function(edgelist) {
      edgelist <- tidyr::separate_rows(edgelist, `Positive Hits`, sep = ", ")
      edgelist <- merge(edgelist, edgelist, by.x = "Positive Hits", by.y = "Positive Hits")
      edgelist$`Positive Hits` <- NULL
      edgelist <- data.table::setDT(edgelist)[, list(`Common Genes` = .N), names(edgelist)]
      return(edgelist)
    },

    calculateSimilarityScore = function(edgelist) {
      edgelist$`Similarity Score %` <- edgelist$`Common Genes` / edgelist$`Total Genes` * 100
      edgelist$`Similarity Score %` <- as.numeric(format(round(edgelist$`Similarity Score %`, 2)))
      return(edgelist)
    },

    removeDuplicateSelfAndOppositeEdges = function(edgelist, weightColumn) {
      graph <- igraph::graph_from_data_frame(edgelist, directed = FALSE)
      igraph::E(graph)$weight <- edgelist[[weightColumn]]
      graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE,
                                edge.attr.comb = "first")
      edgelist <- private$appendEdgelistColumns(graph, weightColumn)
      edgelist[[weightColumn]] <- as.numeric(edgelist[[weightColumn]])
      return(edgelist)
    },

    appendEdgelistColumns = function(graph, weightColumn) {
      graphEdgelist <- as.data.frame(cbind(
        igraph::get.edgelist(graph),
        igraph::E(graph)$`Common Genes`,
        igraph::E(graph)$`Total Genes`,
        igraph::E(graph)$weight
      ))
      colnames(graphEdgelist) <- c("Source Node", "Target Node",
                                   "Common Genes", "Total Genes", "Similarity Score %")
      return(graphEdgelist)
    },

    tuneForHeatmap = function(edgelist) {
      edgelist$`Similarity Score %` <- as.numeric(edgelist$`Similarity Score %`)
      colnames(edgelist) <- c("Source Node", "Target Node",
                              "Common Genes", "Total Genes", "Similarity Score %")
      return(edgelist)
    },

    appendSourceDatabasesAndIds = function(enrichmentData, edgelist) {
      enrichedNetworkData <- enrichmentData[, c("Source", "Term_ID_noLinks", "Function")]
      edgelist <- merge(edgelist, enrichedNetworkData,
                        by.x = "Source Node", by.y = "Term_ID_noLinks")
      edgelist <- merge(edgelist, enrichedNetworkData,
                        by.x = "Target Node", by.y = "Term_ID_noLinks")
      colnames(edgelist) <- c("Target Id", "Source Id", "Common Genes", "Total Genes",
                              "Similarity Score %", "Source Database", "Source Name",
                              "Target Database", "Target Name")
      edgelist <- edgelist[, c(
        "Source Database", "Source Id", "Source Name",
        "Target Database", "Target Id", "Target Name",
        "Common Genes", "Total Genes", "Similarity Score %"
      )]

      edgelist$`Source Database` <- as.factor(edgelist$`Source Database`)
      edgelist$`Target Database` <- as.factor(edgelist$`Target Database`)
      edgelist$`Common Genes` <- as.numeric(edgelist$`Common Genes`)
      edgelist$`Total Genes` <- as.numeric(edgelist$`Total Genes`)
      edgelist$`Similarity Score %` <- as.numeric(edgelist$`Similarity Score %`)

      return(edgelist)
    },

    #' Construct visNetwork
    constructVisNetwork = function(edgelist, drawFormat) {
      graph <- private$createGraph(edgelist, "Source Id", "Target Id", "Similarity Score %")
      data <- private$graphToVisNetworkData(graph)
      nodes <- data$nodes
      edges <- data$edges

      # Assign groups and titles
      nodes$group <- edgelist$`Source Database`[match(nodes$label, edgelist$`Source Id`)]
      nodes$title <- edgelist$`Source Name`[match(nodes$label, edgelist$`Source Id`)]
      nodes$group[is.na(nodes$group)] <- edgelist$`Target Database`[match(
        nodes$label[is.na(nodes$group)], edgelist$`Target Id`)]
      nodes$title[is.na(nodes$title)] <- edgelist$`Target Name`[match(
        nodes$label[is.na(nodes$title)], edgelist$`Target Id`)]

      # Swap labels if drawFormat is Function
      if (drawFormat == "Function") {
        temp <- nodes$label
        nodes$label <- nodes$title
        nodes$title <- temp
      }

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(nodes, edges, layout)

      return(list(nodes = nodes, edges = edges))
    },

    #' Get filtered edgelist from filtered enrichment
    getFilteredEdgelistFromEnrichment = function(filteredEnrichment) {
      edgelistData <- private$.edgelistData()
      if (is.null(edgelistData) || nrow(edgelistData) == 0) return(NULL)

      visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
      visibleTermNames <- unique(filteredEnrichment$Function)

      edgelistData[
        (edgelistData$`Source Id` %in% visibleTermIds | edgelistData$`Source Name` %in% visibleTermNames) &
        (edgelistData$`Target Id` %in% visibleTermIds | edgelistData$`Target Name` %in% visibleTermNames),
        , drop = FALSE
      ]
    },

    #' Get filtered enrichment from filtered edgelist
    getFilteredEnrichmentFromEdgelist = function(filteredEdgelist) {
      enrichmentData <- private$.enrichmentData()
      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return(NULL)

      referencedTermIds <- unique(c(
        filteredEdgelist$`Source Id`,
        filteredEdgelist$`Source Name`,
        filteredEdgelist$`Target Id`,
        filteredEdgelist$`Target Name`
      ))

      enrichmentData[
        enrichmentData$Term_ID_noLinks %in% referencedTermIds |
        enrichmentData$Function %in% referencedTermIds, , drop = FALSE
      ]
    },

    #' Render network from filtered enrichment
    renderNetworkFromFilteredEnrichment = function(filteredEnrichment) {
      nodes <- private$.nodes()
      edges <- private$.edges()

      if (is.null(nodes) || nrow(nodes) == 0) return()

      visibleTermIds <- unique(filteredEnrichment$Term_ID_noLinks)
      visibleTermNames <- unique(filteredEnrichment$Function)

      filteredNodes <- nodes[
        nodes$id %in% visibleTermIds |
        nodes$id %in% visibleTermNames |
        nodes$label %in% visibleTermIds |
        nodes$label %in% visibleTermNames, , drop = FALSE
      ]

      filteredEdges <- edges[
        edges$from %in% filteredNodes$id & edges$to %in% filteredNodes$id, , drop = FALSE
      ]

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(filteredNodes, filteredEdges, layout)
    },

    #' Render network from filtered edgelist
    renderNetworkFromFilteredEdgelist = function(filteredEdgelist) {
      nodes <- private$.nodes()
      edges <- private$.edges()

      if (is.null(nodes) || nrow(nodes) == 0) return()

      referencedNodes <- unique(c(
        filteredEdgelist$`Source Id`,
        filteredEdgelist$`Source Name`,
        filteredEdgelist$`Target Id`,
        filteredEdgelist$`Target Name`
      ))

      validEdgePairs <- unique(paste(
        ifelse(!is.na(filteredEdgelist$`Source Id`),
               filteredEdgelist$`Source Id`, filteredEdgelist$`Source Name`),
        ifelse(!is.na(filteredEdgelist$`Target Id`),
               filteredEdgelist$`Target Id`, filteredEdgelist$`Target Name`),
        sep = "|||"
      ))

      filteredNodes <- nodes[
        nodes$id %in% referencedNodes |
        nodes$label %in% referencedNodes, , drop = FALSE
      ]

      edgePairsForward <- paste(edges$from, edges$to, sep = "|||")
      edgePairsReverse <- paste(edges$to, edges$from, sep = "|||")
      filteredEdges <- edges[
        edgePairsForward %in% validEdgePairs | edgePairsReverse %in% validEdgePairs,
        , drop = FALSE
      ]

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(filteredNodes, filteredEdges, layout)
    },

    #' Export to Arena3D
    exportToArena = function() {
      edgelist <- private$.arenaEdgelist()
      if (is.null(edgelist) || nrow(edgelist) == 0) {
        renderWarning("Make sure a visible network exists.")
        return()
      }

      tryCatch({
        renderModal("<h2>Please wait.</h2><p>Building network for Arena3Dweb</p>")
        result <- arena_export(edgelist)

        if (result$success) {
          private$.moduleSession$sendCustomMessage("handler_browseUrl", result$url)
        } else {
          renderWarning(result$error)
        }
      }, error = function(e) {
        renderWarning("Cannot open Arena3Dweb network at this time.")
      }, finally = {
        removeModal()
      })
    }
  )
)


# =============================================================================
# NETWORK3 OUTPUT SESSION (Gene vs Gene)
# =============================================================================

Network3OutputSession <- R6::R6Class(
  "Network3OutputSession",
  inherit = NetworkOutputSession,

  public = list(
    #' Initialize
    initialize = function(runKey, enrichSession) {
      super$initialize(runKey, enrichSession, outputType = "network3")
    },

    #' Generate namespaced UI
    #'
    #' Composes UI from base class fragments.
    #' Network3 has min common functions slider (unique to this type).
    #' NO drawFormat control (Gene vs Gene - genes only).
    ui = function() {
      ns <- shiny::NS(self$id)
      enrichmentType <- "functional"
      uiTermKeyword <- UI_TERM_KEYWORD[[enrichmentType]]

      shiny::tagList(
        shiny::tags$br(),
        # Row 1: Datasource, Slider, Mode
        shiny::fluidRow(
          shiny::column(4, self$datasourcePickerUI(ns)),
          shiny::column(4, self$termCountSliderUI(ns, uiTermKeyword)),
          shiny::column(4, self$sortModeUI(ns, uiTermKeyword))
        ),
        # Row 2: Arena, Layout, Min common functions
        shiny::fluidRow(
          shiny::column(4, self$arenaButtonUI(ns)),
          shiny::column(4, self$layoutPickerUI(ns)),
          shiny::column(
            4,
            # Min common functions - unique to Network3
            shiny::sliderInput(
              inputId = ns("minCommonFunctions"),
              label = paste0("Number of common ", uiTermKeyword, ":"),
              min = 1, max = 100, value = 5, step = 1
            )
          )
        ),
        # Row 3: Generate, Reset (NO drawFormat for network3)
        shiny::fluidRow(
          shiny::column(
            12,
            self$generateButtonUI(ns),
            self$resetButtonUI(ns)
          )
        ),
        shiny::tags$hr(),
        # Network output
        shiny::tags$div(
          class = "networkOutput",
          visNetwork::visNetworkOutput(ns("network"), height = private$.networkHeight)
        ),
        shiny::tags$br(),
        # Color legend and edgelist
        private$generateColorLegend(),
        DT::dataTableOutput(ns("edgelist")),
        shiny::tags$br(),
        # Enrichment table
        DT::dataTableOutput(ns("table"))
      )
    },

    #' Generate network
    generate = function(sourceSelect, sortMode, termCount, minCommonFunctions) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Rendering Network.</p>")

        results <- self$enrichSession$getResults()
        if (is.null(results) || nrow(results) == 0) {
          renderWarning("No results available for network.")
          return()
        }

        # Filter enrichment data
        enrichmentData <- private$filterEnrichmentData(results, sourceSelect, sortMode, termCount)
        if (is.null(enrichmentData) || nrow(enrichmentData) == 0) {
          renderWarning("No data matches the selected filters.")
          return()
        }

        # Create Gene vs Gene edgelist
        edgelist <- private$extractGeneVsGeneEdgelist(
          enrichmentData, minCommonFunctions, simplifyForNetwork = TRUE
        )

        if (is.null(edgelist) || nrow(edgelist) == 0) {
          renderWarning("The current filters cannot produce enough edges. Please adjust the threshold.")
          return()
        }

        # Store for Arena3D
        private$.arenaEdgelist(edgelist)

        # Render tables
        private$renderEdgelistTableWithData(edgelist)
        private$renderEnrichmentTableWithData(enrichmentData)

        # Build network
        networkData <- private$constructVisNetwork(edgelist)

        # Store state
        private$setNetworkState(enrichmentData, edgelist, networkData$nodes, networkData$edges)

      }, error = function(e) {
        cat("[NetworkOutputSession generate] Error:", conditionMessage(e), "\n")
        print(e)
        renderWarning(paste("Network error:", conditionMessage(e)))
      }, finally = {
        removeModal()
      })
    },

    #' Handle node click
    handleNodeClick = function(clickEvent) {
      if (is.null(clickEvent) || is.null(clickEvent$nodes) ||
          length(clickEvent$nodes) == 0) {
        if (is.null(clickEvent$edges) || length(clickEvent$edges) == 0) {
          self$handleDeselection()
        }
        return()
      }

      nodeId <- clickEvent$nodes[[1]]
      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      # Gene node - filter to terms containing this gene
      filteredEnrichment <- enrichmentData[
        grepl(nodeId, enrichmentData$`Positive Hits`, fixed = TRUE), , drop = FALSE
      ]

      filteredEdgelist <- edgelistData[
        edgelistData$`Source Name` == nodeId |
        edgelistData$`Target Name` == nodeId, , drop = FALSE
      ]

      if (nrow(filteredEnrichment) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Handle edge click
    handleEdgeClick = function(clickEvent) {
      if (is.null(clickEvent) || is.null(clickEvent$edges) ||
          length(clickEvent$edges) == 0) return()
      if (!is.null(clickEvent$nodes) && length(clickEvent$nodes) > 0) return()

      edgeId <- clickEvent$edges[[1]]
      edges <- private$.edges()
      if (is.null(edges) || nrow(edges) == 0) return()

      edgeRow <- edges[edges$id == edgeId, , drop = FALSE]
      if (nrow(edgeRow) == 0) return()

      fromNode <- edgeRow$from[1]
      toNode <- edgeRow$to[1]

      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      # Gene-Gene edge - filter to terms containing both genes
      filteredEdgelist <- edgelistData[
        (edgelistData$`Source Name` == fromNode & edgelistData$`Target Name` == toNode) |
        (edgelistData$`Source Name` == toNode & edgelistData$`Target Name` == fromNode),
        , drop = FALSE
      ]

      filteredEnrichment <- enrichmentData[
        grepl(fromNode, enrichmentData$`Positive Hits`, fixed = TRUE) &
        grepl(toNode, enrichmentData$`Positive Hits`, fixed = TRUE), , drop = FALSE
      ]

      if (nrow(filteredEdgelist) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Handle multi-node selection
    handleSelection = function(selectedNodes) {
      if (is.null(selectedNodes) || length(selectedNodes) <= 1) return()

      enrichmentData <- private$.enrichmentData()
      edgelistData <- private$.edgelistData()

      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return()

      filteredEnrichment <- enrichmentData[
        sapply(enrichmentData$`Positive Hits`, function(hits) {
          genes <- unlist(strsplit(as.character(hits), ",\\s*"))
          any(genes %in% selectedNodes)
        }), , drop = FALSE
      ]

      filteredEdgelist <- edgelistData[
        edgelistData$`Source Name` %in% selectedNodes |
        edgelistData$`Target Name` %in% selectedNodes, , drop = FALSE
      ]

      if (nrow(filteredEnrichment) > 0) {
        private$setExpectedRowCount("enrichment", nrow(filteredEnrichment))
        private$setExpectedRowCount("edgelist", nrow(filteredEdgelist))
        private$renderEnrichmentTableWithData(filteredEnrichment)
        private$renderEdgelistTableWithData(filteredEdgelist)
      }
    },

    #' Set up server logic
    server = function() {
      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.output <- output

        # Register common observers from base class (datasource picker updates slider)
        self$registerCommonObservers(input, session)

        # Initialize controls (populates picker, sets slider range)
        private$initializeControls(session)

        # Generate button
        private$.observers$generate <- shiny::observeEvent(
          input$generateBtn,
          {
            if (!is.null(input$sourceSelect)) {
              self$generate(input$sourceSelect, input$sortMode, input$termCountSlider, input$minCommonFunctions)
            }
          },
          ignoreInit = TRUE
        )

        # Reset button
        private$.observers$reset <- shiny::observeEvent(
          input$resetBtn,
          { self$handleReset() },
          ignoreInit = TRUE
        )

        # Arena button
        private$.observers$arena <- shiny::observeEvent(
          input$arenaBtn,
          { private$exportToArena() },
          ignoreInit = TRUE
        )

        # Network click (uses namespaced input from JavaScript)
        private$.observers$click <- shiny::observeEvent(
          input$network_click,
          {
            clickEvent <- input$network_click
            self$handleNodeClick(clickEvent)
            self$handleEdgeClick(clickEvent)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Multi-select (uses namespaced input from JavaScript)
        private$.observers$selected <- shiny::observeEvent(
          input$network_selected,
          {
            selectedNodes <- input$network_selected
            self$handleSelection(selectedNodes)
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Deselect (uses namespaced input from JavaScript)
        private$.observers$deselect <- shiny::observeEvent(
          input$network_deselect,
          { self$handleDeselection() },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        # Enrichment table filter
        private$.observers$tableFilter <- shiny::observeEvent(
          input$table_rows_all,
          { self$handleEnrichmentTableFilter(input$table_rows_all) },
          ignoreInit = TRUE
        )

        # Edgelist table filter
        private$.observers$edgelistFilter <- shiny::observeEvent(
          input$edgelist_rows_all,
          { self$handleEdgelistTableFilter(input$edgelist_rows_all) },
          ignoreInit = TRUE
        )
      })
    },

    #' Handle datasource selection change (override from base)
    #'
    #' Updates both termCountSlider (via super) and minCommonFunctions slider
    #' based on the actual data range for selected datasources.
    #'
    #' @param selectedSources Character vector of selected datasources
    #' @param session Shiny session object
    onDatasourceChange = function(selectedSources, session) {
      # Call parent to update termCountSlider
      super$onDatasourceChange(selectedSources, session)

      # Also update minCommonFunctions range based on actual data
      private$updateMinCommonFunctionsRange(selectedSources, session)
    }
  ),

  private = list(
    #' Initialize picker and slider controls (override to include minCommonFunctions)
    initializeControls = function(session) {
      # Call parent to initialize sourceSelect and termCountSlider
      super$initializeControls(session)

      # Initialize minCommonFunctions with dynamic range
      # NOTE: Get sources from results directly, not from input (avoids async race condition)
      results <- self$enrichSession$getResults()
      if (!is.null(results) && nrow(results) > 0) {
        sources <- unique(as.character(results$Source))
        private$updateMinCommonFunctionsRange(sources, session)
      } else {
        # Fallback: set safe defaults (sqrt(10) ≈ 3)
        shiny::updateSliderInput(session, "minCommonFunctions", max = 10, value = 3)
      }
    },

    #' Update minCommonFunctions slider range based on selected datasources
    #'
    #' Computes the maximum number of common functions possible for gene pairs
    #' in the filtered enrichment data.
    #'
    #' @param selectedSources Character vector of selected datasources
    #' @param session Shiny session object
    updateMinCommonFunctionsRange = function(selectedSources, session) {
      results <- self$enrichSession$getResults()
      if (is.null(results) || nrow(results) == 0) return()
      if (is.null(selectedSources) || length(selectedSources) == 0) return()

      # Filter by selected sources
      filteredData <- subset(results, Source %in% selectedSources)
      if (nrow(filteredData) == 0) return()

      # Compute max common functions from filtered data
      maxCommon <- private$computeMaxCommonFunctions(filteredData)
      effectiveMax <- max(1, maxCommon)

      # Calculate sensible default: sqrt of max (scales well across ranges)
      sensibleDefault <- max(1, round(sqrt(effectiveMax)))

      # Get current value (use sensible default if NULL/first init)
      currentValue <- shiny::isolate(private$.moduleSession$input$minCommonFunctions)
      newValue <- if (is.null(currentValue)) sensibleDefault else min(currentValue, effectiveMax)

      shiny::updateSliderInput(
        session,
        "minCommonFunctions",
        max = effectiveMax,
        value = newValue
      )
    },

    #' Compute maximum common functions from enrichment data
    #'
    #' Calculates the max number of shared functions between any gene pair.
    #' This determines the upper bound of the minCommonFunctions slider.
    #'
    #' @param enrichmentData Filtered enrichment results
    #' @return Integer - maximum common functions count
    computeMaxCommonFunctions = function(enrichmentData) {
      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return(1)

      tryCatch({
        # Extract gene lists from each term's Positive Hits
        genesPerTerm <- strsplit(as.character(enrichmentData$`Positive Hits`), ",\\s*")

        # Count how many terms each gene appears in
        allGenes <- unlist(genesPerTerm)
        geneCounts <- table(allGenes)

        # The max common functions is the max count for any gene
        # (because if gene X is in N terms, it shares those N terms with itself,
        # but the practical max is the count of the most frequently appearing gene)
        maxCount <- max(geneCounts, na.rm = TRUE)
        return(as.integer(maxCount))
      }, error = function(e) {
        return(1)
      })
    },

    #' Extract Gene vs Gene edgelist
    extractGeneVsGeneEdgelist = function(enrichmentData, minCommonFunctions, simplifyForNetwork = FALSE) {
      genesEdgelist <- enrichmentData[, c("Term_ID_noLinks", "Positive Hits")]
      genesEdgelist <- tidyr::separate_rows(genesEdgelist, `Positive Hits`, sep = ", ")
      genesEdgelist <- merge(genesEdgelist, genesEdgelist,
                             by.x = "Term_ID_noLinks", by.y = "Term_ID_noLinks")
      genesEdgelist$`Term_ID_noLinks` <- NULL
      genesEdgelist <- data.table::setDT(
        genesEdgelist)[, list(`Common Functions` = .N), names(genesEdgelist)]

      if (simplifyForNetwork) {
        graph <- igraph::graph_from_data_frame(genesEdgelist, directed = FALSE)
        igraph::E(graph)$weight <- genesEdgelist$`Common Functions`
        graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE,
                                  edge.attr.comb = "first")
        genesEdgelist <- as.data.frame(cbind(
          igraph::get.edgelist(graph),
          igraph::E(graph)$weight
        ))
        colnames(genesEdgelist) <- c("Source Name", "Target Name", "Common Functions")
        genesEdgelist$`Common Functions` <- as.numeric(genesEdgelist$`Common Functions`)
      } else {
        colnames(genesEdgelist) <- c("Source Name", "Target Name", "Common Functions")
      }

      if (!is.null(minCommonFunctions)) {
        genesEdgelist <- genesEdgelist[
          genesEdgelist$`Common Functions` >= minCommonFunctions, , drop = FALSE
        ]
      }

      return(genesEdgelist)
    },

    #' Construct visNetwork
    constructVisNetwork = function(edgelist) {
      graph <- private$createGraph(edgelist, "Source Name", "Target Name", "Common Functions")

      data <- private$graphToVisNetworkData(graph)
      nodes <- data$nodes
      edges <- data$edges

      # All nodes are genes
      nodes$group <- "Gene"
      nodes$title <- nodes$label

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(nodes, edges, layout)

      return(list(nodes = nodes, edges = edges))
    },

    #' Get filtered edgelist from filtered enrichment
    getFilteredEdgelistFromEnrichment = function(filteredEnrichment) {
      edgelistData <- private$.edgelistData()
      if (is.null(edgelistData) || nrow(edgelistData) == 0) return(NULL)

      allGenes <- unique(unlist(strsplit(as.character(filteredEnrichment$`Positive Hits`), ",\\s*")))
      edgelistData[
        edgelistData$`Source Name` %in% allGenes &
        edgelistData$`Target Name` %in% allGenes, , drop = FALSE
      ]
    },

    #' Get filtered enrichment from filtered edgelist
    getFilteredEnrichmentFromEdgelist = function(filteredEdgelist) {
      enrichmentData <- private$.enrichmentData()
      if (is.null(enrichmentData) || nrow(enrichmentData) == 0) return(NULL)

      referencedGenes <- unique(c(
        filteredEdgelist$`Source Name`,
        filteredEdgelist$`Target Name`
      ))

      enrichmentData[
        sapply(enrichmentData$`Positive Hits`, function(hits) {
          genes <- unlist(strsplit(as.character(hits), ",\\s*"))
          any(genes %in% referencedGenes)
        }), , drop = FALSE
      ]
    },

    #' Render network from filtered enrichment
    renderNetworkFromFilteredEnrichment = function(filteredEnrichment) {
      nodes <- private$.nodes()
      edges <- private$.edges()

      if (is.null(nodes) || nrow(nodes) == 0) return()

      visibleGenes <- unique(unlist(strsplit(
        as.character(filteredEnrichment$`Positive Hits`), ",\\s*"
      )))

      filteredNodes <- nodes[
        nodes$id %in% visibleGenes |
        nodes$label %in% visibleGenes, , drop = FALSE
      ]

      filteredEdges <- edges[
        edges$from %in% filteredNodes$id & edges$to %in% filteredNodes$id, , drop = FALSE
      ]

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(filteredNodes, filteredEdges, layout)
    },

    #' Render network from filtered edgelist
    renderNetworkFromFilteredEdgelist = function(filteredEdgelist) {
      nodes <- private$.nodes()
      edges <- private$.edges()

      if (is.null(nodes) || nrow(nodes) == 0) return()

      referencedNodes <- unique(c(
        filteredEdgelist$`Source Name`,
        filteredEdgelist$`Target Name`
      ))

      validEdgePairs <- unique(paste(
        filteredEdgelist$`Source Name`,
        filteredEdgelist$`Target Name`, sep = "|||"
      ))

      filteredNodes <- nodes[
        nodes$id %in% referencedNodes |
        nodes$label %in% referencedNodes, , drop = FALSE
      ]

      edgePairsForward <- paste(edges$from, edges$to, sep = "|||")
      edgePairsReverse <- paste(edges$to, edges$from, sep = "|||")
      filteredEdges <- edges[
        edgePairsForward %in% validEdgePairs | edgePairsReverse %in% validEdgePairs,
        , drop = FALSE
      ]

      layout <- private$getSelectedLayout()
      private$renderVisNetwork(filteredNodes, filteredEdges, layout)
    },

    #' Export to Arena3D
    exportToArena = function() {
      edgelist <- private$.arenaEdgelist()
      if (is.null(edgelist) || nrow(edgelist) == 0) {
        renderWarning("Make sure a visible network exists.")
        return()
      }

      tryCatch({
        renderModal("<h2>Please wait.</h2><p>Building network for Arena3Dweb</p>")
        result <- arena_export(edgelist)

        if (result$success) {
          private$.moduleSession$sendCustomMessage("handler_browseUrl", result$url)
        } else {
          renderWarning(result$error)
        }
      }, error = function(e) {
        renderWarning("Cannot open Arena3Dweb network at this time.")
      }, finally = {
        removeModal()
      })
    }
  )
)
