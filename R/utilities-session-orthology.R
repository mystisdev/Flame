# =============================================================================
# FLAME Orthology Session
# =============================================================================
#
# Manages the Orthology Search tab using g:Orth API (gprofiler2).
#
# Dependencies:
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - input-analytelist-unranked.R (for UnrankedAnalyteList)
# - infrastructure-config.R (for AnalyteType)
# - func-gorth.R (for gorth_ids)
#
# =============================================================================

# =============================================================================
# ORTHOLOGY UI FUNCTION
# =============================================================================

#' Generate the Orthology Search panel UI
#' @param id Module namespace ID (must match the ID used in OrthologySession$new)
#' @return A div containing the orthology UI
orthologyUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    shiny::tags$h3("Orthology Search"),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("select"),
          label = "1. Select list for orthology search:",
          choices = NULL
        ),
        shiny::actionButton(
          ns("button"),
          label = "Orthology search",
          icon = shiny::icon("paper-plane"),
          class = "submit_button"
        )
      ),
      shiny::column(
        8,
        shiny::selectizeInput(
          inputId = ns("organism"),
          label = "2. Select input organism:",
          choices = NULL,
          multiple = FALSE,
          selected = NULL,
          width = "80%",
          options = list(placeholder = "Select an option or start typing...")
        ),
        shiny::selectizeInput(
          inputId = ns("target"),
          label = "3. Select target organism:",
          choices = NULL,
          multiple = FALSE,
          selected = NULL,
          width = "80%",
          options = list(placeholder = "Select an option or start typing...")
        )
      )
    ),
    shiny::tags$hr(),
    shiny::tags$div(
      id = ns("resultsPanel"),
      style = "display:none",
      DT::dataTableOutput(ns("table")),
      shiny::fluidRow(
        shiny::column(
          2,
          shinyWidgets::radioGroupButtons(
            inputId = ns("dType"),
            label = "Select column to add:",
            choiceNames = c("Ortholog Name", "Ortholog ID"),
            choiceValues = c("name", "id")
          )
        ),
        shiny::column(
          2,
          shiny::tags$br(),
          shiny::actionButton(
            inputId = ns("addList"),
            label = "Add result to input",
            icon = shiny::icon("paper-plane")
          )
        ),
        shiny::column(
          6,
          shiny::tags$br(),
          shiny::tags$p(
            id = ns("addedInfo"),
            style = "display:none; font-size:12px",
            "Selected column has been added to the list of inputs!"
          )
        )
      )
    )
  )
}

# =============================================================================
# ORTHOLOGY SESSION
# =============================================================================

#' Orthology Session Class
#'
#' Manages the Orthology Search tab using g:Orth API.
#'
#' @section UI Components:
#' - List selector dropdown
#' - Source organism dropdown
#' - Target organism dropdown
#' - Orthology search button
#' - Results table with column selection and add to lists button
#'
#' @section Dependencies:
#' - AnalyteListRegistry for storing analyte lists
#' - gorth_ids() for orthology search
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' orthologySession <- OrthologySession$new("gorth", registry)
#' orthologySession$server(input, session)
#' }
#'
OrthologySession <- R6::R6Class(
  "OrthologySession",

  public = list(
    #' @field id Module namespace ID
    id = NULL,

    #' Initialize an OrthologySession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      self$id <- id
      private$.registry <- registry
      private$.orthologyResult <- NULL
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
      private$.orthologyResult <- NULL
    },

    #' Check if there is an orthology result
    hasResult = function() {
      !is.null(private$.orthologyResult) && nrow(private$.orthologyResult) > 0
    },

    #' Get the current orthology result
    getResult = function() {
      private$.orthologyResult
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

        # -------------------------------------------------------------------
        # INITIALIZATION
        # -------------------------------------------------------------------

        # Initialize organism dropdowns
        shiny::observe({
          sortedOrganisms <- ORGANISMS[order(ORGANISMS$print_name), ]
          gProfiler_printNames <- sortedOrganisms[!is.na(sortedOrganisms$short_name), ]$print_name
          selectedSource <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
          selectedTarget <- "Mus musculus (Mouse) [NCBI Tax. ID: 10090]"

          # Source organism
          shiny::updateSelectizeInput(
            session, "organism",
            choices = gProfiler_printNames,
            selected = selectedSource,
            server = TRUE
          )

          # Target organism (excludes source)
          shiny::updateSelectizeInput(
            session, "target",
            choices = gProfiler_printNames[gProfiler_printNames != selectedSource],
            selected = selectedTarget,
            server = TRUE
          )
        }, priority = 100)

        # Update list selector when registry changes
        shiny::observe({
          listNames <- private$.registry$getNamesReactive()
          currentSelection <- input$select
          selected <- if (!is.null(currentSelection) && currentSelection %in% listNames) {
            currentSelection
          } else {
            NULL
          }
          shiny::updateSelectInput(session, "select", choices = listNames, selected = selected)
        })

        # -------------------------------------------------------------------
        # OBSERVERS
        # -------------------------------------------------------------------

        # Source organism change - update target to exclude source
        private$.observers$organismChange <- shiny::observeEvent(input$organism, {
          private$handleOrganismChange(input, session)
        }, ignoreInit = TRUE)

        # Orthology search button
        private$.observers$search <- shiny::observeEvent(input$button, {
          private$handleSearch(input, output, session)
        }, ignoreInit = TRUE)

        # Add to lists button
        private$.observers$addList <- shiny::observeEvent(input$addList, {
          private$handleAddToInput(input, session)
        }, ignoreInit = TRUE)

      }) # end moduleServer
    }
  ),

  private = list(
    .registry = NULL,
    .parentSession = NULL,
    .moduleSession = NULL,
    .observers = list(),

    # Current orthology result
    .orthologyResult = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    handleOrganismChange = function(input, session) {
      tryCatch({
        sourceOrganism <- input$organism
        targetOrganism <- input$target

        # If source == target, update target choices to exclude source
        if (!is.null(sourceOrganism) && !is.null(targetOrganism) &&
            sourceOrganism == targetOrganism) {
          sortedOrganisms <- ORGANISMS[order(ORGANISMS$print_name), ]
          organismChoices <- sortedOrganisms[!is.na(sortedOrganisms$short_name), ]$print_name
          shiny::updateSelectizeInput(
            session, "target",
            choices = organismChoices[organismChoices != sourceOrganism],
            server = TRUE
          )
        }
      }, error = function(e) {
        cat(paste("[OrthologySession] Organism change error:", conditionMessage(e), "\n"))
        renderError("Error while choosing organism.")
      })
    },

    handleSearch = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Searching for homologs.</p>")

        # Get inputs
        selectedListName <- input$select
        sourceOrganismPrintName <- input$organism
        targetOrganismPrintName <- input$target

        # Validate inputs
        if (is.null(selectedListName) || selectedListName == "") {
          renderWarning("Please select a list for orthology search.")
          return()
        }

        if (is.null(sourceOrganismPrintName) || sourceOrganismPrintName == "" ||
            is.null(targetOrganismPrintName) || targetOrganismPrintName == "") {
          renderWarning("Select both an input and target organism.")
          return()
        }

        # Get the selected list
        analyteList <- private$.registry$get(selectedListName)
        if (is.null(analyteList)) {
          renderWarning("Selected list not found.")
          return()
        }

        # Convert organism print names to short names
        sourceOrganismShortName <- ORGANISMS[ORGANISMS$print_name == sourceOrganismPrintName, ]$short_name[1]
        targetOrganismShortName <- ORGANISMS[ORGANISMS$print_name == targetOrganismPrintName, ]$short_name[1]

        # Call the API wrapper
        result <- gorth_ids(
          ids = analyteList$getIds(),
          source_organism = sourceOrganismShortName,
          target_organism = targetOrganismShortName
        )

        if (is.null(result)) {
          renderWarning("No results found. Please try another organism or list.")
          return()
        }

        # Store result and prepare for display
        private$.orthologyResult <- result

        # Select and rename columns for display
        displayResult <- result[c("input", "input_ensg", "ortholog_name", "ortholog_ensg", "description")]
        colnames(displayResult) <- c("Input", "Input ID", "Ortholog Name", "Ortholog ID", "Description")

        # Show results panel and render table
        shinyjs::show("resultsPanel")
        output$table <- DT::renderDataTable(
          displayResult,
          server = FALSE,
          selection = "none",
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            scroller = TRUE,
            dom = "Blfiprt",
            buttons = createExportButtons(paste0("orthology_", selectedListName), c())
          ),
          rownames = FALSE,
          escape = FALSE
        )

      }, error = function(e) {
        cat(paste("[OrthologySession] Search error:", conditionMessage(e), "\n"))
        renderError("Error while searching for homologs.")
      }, finally = {
        removeModal()
      })
    },

    handleAddToInput = function(input, session) {
      tryCatch({
        shinyjs::hide("addedInfo")
        renderModal("<h2>Please wait.</h2><br /><p>Parsing your data.</p>")

        if (!self$hasResult()) {
          renderWarning("No orthology results available.")
          return()
        }

        # Get column selection
        dtype <- input$dType
        orthologyTable <- private$.orthologyResult

        # Extract the appropriate column
        if (dtype == "name") {
          resultIds <- unique(as.character(unlist(orthologyTable$ortholog_name)))
        } else {
          resultIds <- unique(as.character(unlist(orthologyTable$ortholog_ensg)))
        }

        # Clean and filter
        resultIds <- resultIds[!is.na(resultIds) & resultIds != ""]

        if (length(resultIds) == 0) {
          renderWarning("No valid results to add.")
          return()
        }

        # Generate unique name
        listName <- sprintf("gorth_%s", dtype)
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
          ids = resultIds
        )
        private$.registry$add(analyteList)

        shinyjs::show("addedInfo")

      }, error = function(e) {
        cat(paste("[OrthologySession] Add to list error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      }, finally = {
        removeModal()
      })
    }
  )
)
