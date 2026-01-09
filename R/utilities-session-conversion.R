# =============================================================================
# FLAME Conversion Session
# =============================================================================
#
# Manages the Gene ID Conversion tab using g:Convert API (gprofiler2).
#
# Dependencies:
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - input-analytelist-unranked.R (for UnrankedAnalyteList)
# - infrastructure-config.R (for AnalyteType)
# - func-gconvert.R (for gconvert_ids)
#
# =============================================================================

# =============================================================================
# CONVERSION UI FUNCTION
# =============================================================================

#' Generate the Gene ID Conversion panel UI
#' @param id Module namespace ID (must match the ID used in ConversionSession$new)
#' @return A div containing the conversion UI
conversionUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tags$div(
    shiny::tags$h3("Gene ID Conversion"),
    shiny::tags$br(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("select"),
          label = "1. Select list to convert:",
          choices = NULL
        ),
        shiny::actionButton(
          ns("button"),
          label = "Convert IDs",
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
        shiny::selectInput(
          ns("target"),
          label = "3. Select target namespace:",
          selected = "ENTREZGENE",
          width = "80%",
          choices = c(NAMESPACES[["CORE"]], unlist(NAMESPACES[["SPECIAL"]]))
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
            choiceNames = c("Name", "Target"),
            choiceValues = c("name", "target")
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
# CONVERSION SESSION
# =============================================================================

#' Conversion Session Class
#'
#' Manages the Gene ID Conversion tab using g:Convert API.
#'
#' @section UI Components:
#' - List selector dropdown
#' - Organism dropdown
#' - Target namespace dropdown
#' - Convert button
#' - Results table with column selection and add to lists button
#'
#' @section Dependencies:
#' - AnalyteListRegistry for storing analyte lists
#' - gconvert_ids() for ID conversion
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' conversionSession <- ConversionSession$new("gconvert", registry)
#' conversionSession$server(input, session)
#' }
#'
ConversionSession <- R6::R6Class(
  "ConversionSession",

  public = list(
    #' @field id Module namespace ID
    id = NULL,

    #' Initialize a ConversionSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      self$id <- id
      private$.registry <- registry
      private$.conversionResult <- NULL
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
      private$.conversionResult <- NULL
    },

    #' Check if there is a conversion result
    hasResult = function() {
      !is.null(private$.conversionResult) && nrow(private$.conversionResult) > 0
    },

    #' Get the current conversion result
    getResult = function() {
      private$.conversionResult
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

        # Initialize organism dropdown
        shiny::observe({
          sortedOrganisms <- ORGANISMS[order(ORGANISMS$print_name), ]
          gProfiler_printNames <- sortedOrganisms[!is.na(sortedOrganisms$short_name), ]$print_name
          selected <- "Homo sapiens (Human) [NCBI Tax. ID: 9606]"
          shiny::updateSelectizeInput(
            session, "organism",
            choices = gProfiler_printNames,
            selected = selected,
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

        # Convert button
        private$.observers$convert <- shiny::observeEvent(input$button, {
          private$handleConvert(input, output, session)
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

    # Current conversion result
    .conversionResult = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    handleConvert = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Converting list.</p>")

        # Get inputs
        selectedListName <- input$select
        organismPrintName <- input$organism
        targetNamespace <- input$target

        # Validate inputs
        if (is.null(selectedListName) || selectedListName == "") {
          renderWarning("Please select a list to convert.")
          return()
        }

        if (is.null(organismPrintName) || organismPrintName == "") {
          renderWarning("Select an input organism.")
          return()
        }

        # Get the selected list
        analyteList <- private$.registry$get(selectedListName)
        if (is.null(analyteList)) {
          renderWarning("Selected list not found.")
          return()
        }

        # Convert organism print name to short name
        organismShortName <- ORGANISMS[ORGANISMS$print_name == organismPrintName, ]$short_name[1]

        # Call the API wrapper
        result <- gconvert_ids(
          ids = analyteList$getIds(),
          organism = organismShortName,
          target_namespace = targetNamespace
        )

        if (is.null(result)) {
          renderWarning("No results found. Please try another organism or list.")
          return()
        }

        # Store result and prepare for display
        private$.conversionResult <- result

        # Select and rename columns for display
        displayResult <- result[c("input", "target", "name", "description")]
        colnames(displayResult) <- c("Input", "Target", "Name", "Description")

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
            buttons = createExportButtons(paste0("conversion_", selectedListName), c())
          ),
          rownames = FALSE,
          escape = FALSE
        )

      }, error = function(e) {
        cat(paste("[ConversionSession] Conversion error:", conditionMessage(e), "\n"))
        renderError("Error while converting the list input.")
      }, finally = {
        removeModal()
      })
    },

    handleAddToInput = function(input, session) {
      tryCatch({
        shinyjs::hide("addedInfo")
        renderModal("<h2>Please wait.</h2><br /><p>Parsing your data.</p>")

        if (!self$hasResult()) {
          renderWarning("No conversion results available.")
          return()
        }

        # Get column selection
        dtype <- input$dType
        conversionTable <- private$.conversionResult

        # Extract the appropriate column
        if (dtype == "name") {
          resultIds <- unique(as.character(unlist(conversionTable$name)))
        } else {
          resultIds <- unique(as.character(unlist(conversionTable$target)))
        }

        # Clean and filter
        resultIds <- resultIds[!is.na(resultIds) & resultIds != ""]

        if (length(resultIds) == 0) {
          renderWarning("No valid results to add.")
          return()
        }

        # Generate unique name
        listName <- sprintf("gconvert_%s", dtype)
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
        cat(paste("[ConversionSession] Add to list error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      }, finally = {
        removeModal()
      })
    }
  )
)
