# =============================================================================
# FLAME Network Analysis Session
# =============================================================================
#
# Manages the STRING Network Analysis tab.
#
# Dependencies:
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - input-analytelist-unranked.R (for UnrankedAnalyteList)
# - infrastructure-config.R (for AnalyteType)
# - func-string-network.R (for API wrappers)
# - enrich-main.R (for stringPOSTConvertENSP)
#
# =============================================================================

# =============================================================================
# NETWORK ANALYSIS UI FUNCTION
# =============================================================================

#' Generate the Network Analysis panel UI
#' @param id Module namespace ID (must match the ID used in NetworkAnalysisSession$new)
#' @return A div containing the network analysis UI
networkAnalysisUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    shiny::tags$h3("Protein-Protein Interaction Network"),
    shiny::tags$br(),
    shiny::fluidRow(
      width = 12,
      shiny::column(
        4,
        shinyWidgets::pickerInput(
          ns("select"),
          label = "1. Select list:",
          choices = NULL,
          choicesOpt = NULL,
          width = "80%"
        ),
        shiny::selectizeInput(
          inputId = ns("organism"),
          label = "2. Select organism:",
          choices = NULL,
          multiple = FALSE,
          selected = NULL,
          width = "80%",
          options = list(placeholder = "Select an option or start typing...")
        )
      ),
      shiny::column(
        4,
        shiny::radioButtons(
          inputId = ns("networkType"),
          label = "3. Select interaction type:",
          choiceNames = list("Full network", "Physical subnetwork"),
          choiceValues = list("functional", "physical")
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Full network: edges indicate both functional and physical associations.\nPhysical subnetwork: edges indicate existence of a physical complex."
              )
          ),
        shiny::radioButtons(
          inputId = ns("networkEdges"),
          label = "4. Select meaning of network edges:",
          choiceNames = list("Evidence", "Confidence"),
          choiceValues = list("evidence", "confidence")
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Evidence: edge colors indicate the type of interaction evidence.\nConfidence: edge thickness indicates the strength of data support based on the interaction score"
              )
          )
      ),
      shiny::column(
        4,
        shinyWidgets::pickerInput(
          inputId = ns("networkScore"),
          label = "5. Select interaction score cut-off:",
          choices = list(
            `highest confidence (0.900)` = 900,
            `high confidence (0.700)` = 700,
            `medium confidence (0.400)` = 400,
            `low confidence (0.150)` = 150
          ),
          selected = 400
        )
      )
    ),
    shiny::actionButton(
      ns("runAnalysis"),
      label = "Run analysis",
      icon = shiny::icon("paper-plane"),
      class = "submit_button"
    ),
    shiny::tags$br(),
    shiny::tags$hr(),
    shiny::verbatimTextOutput(ns("networkParameters")),
    shiny::htmlOutput(ns("legend")),
    shiny::tags$br(),
    shiny::htmlOutput(ns("exportButtons")),
    shiny::tags$br(),
    shiny::htmlOutput(ns("viewer"))
  )
}

# =============================================================================
# NETWORK ANALYSIS SESSION
# =============================================================================

#' Network Analysis Session Class
#'
#' Manages the STRING Network Analysis tab.
#'
#' @section UI Components:
#' - List selector dropdown
#' - Organism dropdown
#' - Network type radio buttons
#' - Edge meaning radio buttons
#' - Score cutoff picker
#' - Run analysis button
#' - Results area with legend, export buttons, and interactive viewer
#'
#' @section Dependencies:
#' - AnalyteListRegistry for accessing analyte lists
#' - STRING API via func-string-network.R
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' networkSession <- NetworkAnalysisSession$new("string_network", registry)
#' networkSession$server(input, session)
#' }
#'
NetworkAnalysisSession <- R6::R6Class(
  "NetworkAnalysisSession",

  public = list(
    #' @field id Module namespace ID
    id = NULL,

    #' Initialize a NetworkAnalysisSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for accessing analyte lists.
    initialize = function(id, registry) {
      self$id <- id
      private$.registry <- registry
      private$.networkData <- NULL
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      # Destroy all observers
      for (obs in private$.observers) {
        if (!is.null(obs)) {
          obs$destroy()
        }
      }
      private$.observers <- list()
      private$.networkData <- NULL
    },

    #' Check if there is a network result
    hasResult = function() {
      !is.null(private$.networkData) && !is.null(private$.networkData$string_tsv)
    },

    #' Get the current network data
    getResult = function() {
      private$.networkData
    },

    # =========================================================================
    # SERVER METHOD
    # =========================================================================

    #' Initialize server logic
    #' @param parentInput Parent session's input object
    #' @param parentSession Parent session object
    server = function(parentInput, parentSession) {
      private$.parentSession <- parentSession

      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        ns <- session$ns

        # -------------------------------------------------------------------
        # INITIALIZATION
        # -------------------------------------------------------------------

        # Initialize organism dropdown with STRING-supported organisms
        shiny::observe({
          sortedOrganisms <- ORGANISMS[order(ORGANISMS$print_name), ]
          STRING_printNames <- sortedOrganisms[sortedOrganisms$taxid %in% TOOL_ORGANISMS$STRING, ]$print_name
          selected <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
          shiny::updateSelectizeInput(
            session, "organism",
            choices = STRING_printNames,
            selected = selected,
            server = TRUE
          )
        }, priority = 100)

        # Update list selector when registry changes
        # Disables lists that exceed the STRING API limit
        shiny::observe({
          listNames <- private$.registry$getNamesReactive()
          if (length(listNames) == 0) {
            shinyWidgets::updatePickerInput(session, "select", choices = character(0))
            return()
          }

          sizes <- sapply(listNames, function(name) {
            analyteList <- private$.registry$get(name)
            if (!is.null(analyteList)) length(analyteList$getIds()) else 0
          })

          disabled <- sizes > private$.stringLimit
          subtext <- ifelse(
            disabled,
            sprintf("%d items - exceeds %d limit", sizes, private$.stringLimit),
            ""
          )

          currentSelection <- input$select
          selected <- if (!is.null(currentSelection) &&
                          currentSelection %in% listNames &&
                          !disabled[currentSelection]) {
            currentSelection
          } else {
            NULL
          }

          shinyWidgets::updatePickerInput(
            session, "select",
            choices = listNames,
            selected = selected,
            choicesOpt = list(disabled = unname(disabled), subtext = unname(subtext))
          )
        })

        # -------------------------------------------------------------------
        # OBSERVERS
        # -------------------------------------------------------------------

        # Run analysis button
        private$.observers$runAnalysis <- shiny::observeEvent(input$runAnalysis, {
          private$handleRunAnalysis(input, output, session)
        }, ignoreInit = TRUE)

        # Submit for functional enrichment (dynamic button)
        private$.observers$submitFunctional <- shiny::observeEvent(input$submit_functional, {
          private$handleSubmitForEnrichment(input, parentSession, "functional")
        }, ignoreInit = TRUE)

      }) # end moduleServer
    }
  ),

  private = list(
    .stringLimit = 500,
    .registry = NULL,
    .parentSession = NULL,
    .moduleSession = NULL,
    .observers = list(),

    # Current network data (replaces global STRINGNetworkData)
    .networkData = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    handleRunAnalysis = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Generating STRING network.</p>")

        # Get inputs
        selectedListName <- input$select
        organism <- input$organism
        networkType <- input$networkType
        networkEdges <- input$networkEdges
        networkScore <- input$networkScore

        # Validate inputs
        if (is.null(selectedListName) || selectedListName == "") {
          renderWarning("Please select a list for network analysis.")
          return()
        }

        if (is.null(organism) || organism == "") {
          renderWarning("Please select an organism.")
          return()
        }

        # Get the selected list
        analyteList <- private$.registry$get(selectedListName)
        if (is.null(analyteList)) {
          renderWarning("Selected list not found.")
          return()
        }

        # Display parameters
        private$displayParameters(output, organism, networkType, networkEdges, networkScore)

        # Get taxid and convert IDs
        string_taxid <- ORGANISMS[ORGANISMS$print_name == organism, ]$taxid
        selectedListItems <- analyteList$getIds()

        # Validate list size
        if (length(selectedListItems) > private$.stringLimit) {
          renderWarning(sprintf(
            "List contains %d items, which exceeds the STRING API limit of %d.",
            length(selectedListItems),
            private$.stringLimit
          ))
          return()
        }

        # Convert to STRING format
        result_ids <- private$parseInputsForRequest(selectedListItems, string_taxid)
        if (is.null(result_ids)) {
          renderWarning("Failed to convert gene IDs. Please check your input list.")
          return()
        }

        ids_interactive <- result_ids$ids_interactive
        ids_post <- result_ids$ids_post

        # Create legend
        private$createLegend(output, networkEdges)

        # Get network data from STRING API
        string_link <- string_network_get_link(ids_post, string_taxid, networkType, networkEdges, networkScore)
        string_tsv <- string_network_get_data(ids_post, string_taxid, networkType, networkEdges, networkScore)

        # Check for error response
        if (strsplit(string_tsv, "\t")[[1]][1] == "Error") {
          renderWarning("STRING did not return any valid results. Make sure the input is in ENSP namespace.")
          return()
        }

        # Store network data
        private$.networkData <- list(
          string_link = string_link,
          string_tsv = string_tsv,
          svg_to_png_js_code = string_network_png_export_js()
        )

        # Create action buttons
        private$createActionButtons(output, session, private$.networkData)

        # Create interactive viewer
        private$createInteractiveViewer(output, ids_interactive, string_taxid, networkType, networkEdges, networkScore)

      }, error = function(e) {
        cat(paste("[NetworkAnalysisSession] Error:", conditionMessage(e), "\n"))
        renderError("Problem with STRING network.")
      }, finally = {
        removeModal()
      })
    },

    handleSubmitForEnrichment = function(input, parentSession, category) {
      tryCatch({
        if (!self$hasResult()) {
          renderWarning("No network data available. Please run analysis first.")
          return()
        }

        # Parse genes from network TSV
        stringGenes <- string_network_parse_genes(private$.networkData$string_tsv)

        if (length(stringGenes) == 0) {
          renderWarning("No genes found in STRING network.")
          return()
        }

        # Generate unique name
        listName <- sprintf("string_%s", category)
        existingNames <- private$.registry$getNames()
        if (listName %in% existingNames) {
          suffix <- 1
          while (paste0(listName, "_", suffix) %in% existingNames) {
            suffix <- suffix + 1
          }
          listName <- paste0(listName, "_", suffix)
        }

        # Create and add analyte list
        analyteList <- UnrankedAnalyteList$new(
          name = listName,
          analyteType = AnalyteType$GENE,
          ids = stringGenes
        )
        private$.registry$add(analyteList)

        # Select this list in the enrichment dropdown (using parent session)
        shiny::updateSelectInput(
          parentSession,
          sprintf("%s_enrichment_file", category),
          selected = listName
        )

        # Navigate to enrichment tab
        shinydashboard::updateTabItems(
          parentSession,
          "sideBarId",
          selected = sprintf("%s_enrichment", category)
        )

      }, error = function(e) {
        cat(paste("[NetworkAnalysisSession] Submit error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      })
    },

    # =========================================================================
    # HELPER METHODS
    # =========================================================================

    displayParameters = function(output, organism, networkType, networkEdges, networkScore) {
      stringParameters <- paste0(
        "Organism: ", organism,
        "\nNetwork Type: ", networkType,
        "\nMeaning of network edges: ", networkEdges,
        "\nInteraction score cut-off: ", as.numeric(networkScore) / 1000
      )
      output$networkParameters <- shiny::renderText(stringParameters)
    },

    parseInputsForRequest = function(selectedListItems, string_taxid) {
      # Convert to STRING ENSP format
      conversionResult <- stringPOSTConvertENSP(selectedListItems, string_taxid)
      if (is.null(conversionResult)) {
        return(NULL)
      }
      selectedListItems <- conversionResult$target

      ids_interactive <- paste(selectedListItems, collapse = "','")
      ids_post <- paste(selectedListItems, collapse = "%0d")
      return(list(ids_interactive = ids_interactive, ids_post = ids_post))
    },

    createLegend = function(output, edge_meaning) {
      table_nodes <- "<table>
        <tr><td><img src='string_icons/node_known_structure.png' /></td><td>Proteins with known 3D structure (experimental or predicted)</td></tr>
        <tr><td><img src='string_icons/node_unknown_structure_string.png' /></td><td>Proteins with unknown 3D structure</td></tr>
        </table>
        "

      if (tolower(edge_meaning) == "evidence") {
        table_edges <- "<table style='width:70%'>
        <th colspan=6>Known Interactions</th>
        <tr>
        <td style='width:5%'><img src='string_icons/edge_experiment.png' ></td><td style='width:20%'>Experimentally determined</td>
        <td style='width:5%'><img src='string_icons/edge_curated_database.png' ></td><td style='width:20%'>From curated databases</td>
        <td></td>
        </tr>
        <th colspan=6>Computationally inferred from gene analysis</th>
        <tr>
        <td style='width:5%'><img src='string_icons/edge_gene_neighborhood.png' ></td><td style='width:20%'>Gene neighborhood</td>
        <td style='width:5%'><img src='string_icons/edge_gene_fusions.png' ></td><td style='width:20%'>Gene fusions</td>
        <td style='width:5%'><img src='string_icons/edge_gene_coocurrence.png' ></td><td style='width:20%'>Gene co-occurrence</td>
        </tr>
        <th colspan=6>Computationally inferred from other sources</th>
        <tr>
        <td style='width:5%'><img src='string_icons/edge_textmining.png' ></td><td style='width:20%'>Text mining</td>
        <td style='width:5%'><img src='string_icons/edge_coexpression.png' ></td><td style='width:20%'>Co-expression</td>
        <td style='width:5%'><img src='string_icons/edge_homology.png' ></td><td style='width:20%'>Protein homology</td>
        </tr>
        </table>
        "
      } else {
        table_edges <- "<table style='width:60%'>
        <th colspan=4>Confidence levels</th>
        <tr>
        <td style='width:8%'><img src='string_icons/edge_confidence_low.png' ></td><td>Low confidence (>=0.150) </td>
        <td style='width:8%'><img src='string_icons/edge_confidence_medium.png' ></td><td>Medium confidence (>=0.400) </td>
        </tr>
        <tr>
        <td style='width:8%'><img src='string_icons/edge_confidence_high.png' ></td><td>High confidence (>=0.700) </td>
        <td style='width:8%'><img src='string_icons/edge_confidence_highest.png' ></td><td>Highest confidence (>=0.900) </td>
        </tr>
        </table>
        "
      }

      output$legend <- shiny::renderUI({
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::div(
              shiny::h5(shiny::strong("Nodes:")),
              shiny::HTML(table_nodes)
            )
          ),
          shiny::column(
            8,
            shiny::div(
              shiny::h5(shiny::strong("Edges:")),
              shiny::HTML(table_edges)
            )
          )
        )
      })
    },

    createActionButtons = function(output, session, networkData) {
      ns <- session$ns
      string_link <- networkData$string_link
      string_tsv <- networkData$string_tsv
      svg_to_png_js_code <- networkData$svg_to_png_js_code

      output$exportButtons <- shiny::renderUI({
        shiny::div(
          style = "margin:10px !important",
          shiny::fluidRow(
            shiny::actionButton(
              inputId = ns("open_in_string"),
              label = "Open in STRING",
              icon = shiny::icon("link"),
              style = "md-flat",
              onclick = sprintf("window.open('%s', '_blank')", string_link)
            ),
            shiny::downloadButton(
              outputId = ns("dnl_tsv"),
              label = "Download Network",
              icon = shiny::icon("download"),
              style = "md-flat"
            ),
            shiny::actionButton(
              inputId = ns("dnl_png"),
              label = "Export Image",
              icon = shiny::icon("image"),
              style = "md-flat",
              onclick = svg_to_png_js_code
            )
          ),
          shiny::h5("Perform Enrichment on Network:"),
          shiny::fluidRow(
            shiny::actionButton(
              inputId = ns("submit_functional"),
              label = "Run Enrichment",
              icon = shiny::icon("paper-plane"),
              style = "md-flat"
            )
          )
        )
      })

      # Download handler for TSV
      output$dnl_tsv <- shiny::downloadHandler(
        filename = "network.tsv",
        content = function(file) {
          write(string_tsv, file)
        }
      )
    },

    createInteractiveViewer = function(output, ids_interactive, string_taxid, networkType, networkEdges, networkScore) {
      output$viewer <- shiny::renderUI({
        shiny::tags$div(
          shiny::tags$script(sprintf(
            "var proteins = ['%s'];
            var species = ['%s'];
            var type = ['%s'];
            var edges = ['%s'];
            var score = ['%s'];
            getSTRING('https://string-db.org', {
            'species':species,
            'identifiers':proteins,
            'network_type':type,
            'network_flavor':edges,
            'required_score':score});",
            ids_interactive, string_taxid, networkType, networkEdges, networkScore
          )),
          # Keep stringEmbedded ID hardcoded - expected by STRING's external JS
          shiny::tags$div(id = "stringEmbedded")
        )
      })
    }
  )
)
