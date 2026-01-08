# =============================================================================
# FLAME Text Mining Input Session
# =============================================================================
#
# Manages the Text Mining tab for extracting genes from text using EXTRACT API.
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - func-extract.R (for extract_entities, extract_annotated_html)
# - func-links.R (for attachTextMiningDBLinks)
#
# =============================================================================

# =============================================================================
# TEXT MINING INPUT UI FUNCTION
# =============================================================================

#' Generate the Text Mining input panel UI
#' @param id Module namespace ID (must match the ID used in TextMiningInputSession$new)
#' @return A Shiny tabPanel
textMiningInputUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Text-mining",
    icon = shiny::icon("envelope-open-text"),
    shiny::tags$br(),
    shiny::tags$div(
      shiny::fluidRow(
        shiny::column(
          5,
          shiny::tags$div(
            shiny::textAreaInput(
              inputId = ns("textmining_textinput"),
              label = "1. Input text (Max: 100,000 words or 500,000 characters):",
              placeholder = "Write or paste a text here.\n\nClick the 'Load Example' button to load an example text.",
              resize = "vertical", height = "200px", width = "90%"
            ),
            shiny::selectizeInput(
              inputId = ns("textmining_organism"),
              label = "2. Select organism:",
              choices = NULL,
              selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
              multiple = FALSE,
              width = "90%",
              options = list(placeholder = "Select an option or start typing...")
            ),
            shiny::actionButton(ns("textmining_submit"), "Submit", shiny::icon("paper-plane")),
            shiny::actionButton(ns("textmining_addExample"), "Example", shiny::icon("bookmark")),
            shiny::actionButton(ns("textmining_clear"), "Clear", shiny::icon("broom"))
          )
        ),
        shiny::column(
          7,
          shiny::tags$div(
            id = ns("textmining_tagger_results"),
            style = "display:none",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Identified Genes",
                DT::dataTableOutput(outputId = ns("extracted_terms"))
              ),
              shiny::tabPanel(
                "Annotated Text",
                shiny::wellPanel(shiny::htmlOutput(outputId = ns("extracted_text")))
              )
            ),
            shiny::actionButton(ns("textmining_selectAll"), "Select All", shiny::icon("check")),
            shiny::actionButton(ns("textmining_selectNone"), "De-select All", shiny::icon("x")),
            shiny::tags$br(),
            shiny::actionButton(ns("textmining_addList"), "Add selected to lists", shiny::icon("paper-plane")),
            shiny::actionButton(ns("textmining_delete"), "Delete", shiny::icon("broom"))
          )
        )
      )
    )
  )
}

# =============================================================================
# TEXT MINING INPUT SESSION
# =============================================================================

#' Text Mining Input Session Class
#'
#' Manages the Text Mining tab for extracting genes from text using EXTRACT API.
#'
#' Inherits from InputSession and uses the shared methods:
#' - addAnalyteList(): Add an analyte list to the registry
#' - generateUniqueName(): Generate unique list names
#' - cleanup(): Destroy observers
#'
#' @section UI Components:
#' - Input panel: text area, organism dropdown, submit/example/clear buttons
#' - Results panel: data table with checkboxes, annotated text view, add to lists button
#'
#' @section Dependencies:
#' - AnalyteListRegistry for storing analyte lists
#' - EXTRACT API (tagger.jensenlab.org) for text mining
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' textMiningInput <- TextMiningInputSession$new(ModuleIds$INPUT_TEXTMINING, registry)
#' textMiningInput$server(input, session)
#' }
#'
TextMiningInputSession <- R6::R6Class(
  "TextMiningInputSession",

  inherit = InputSession,

  public = list(
    #' Initialize a TextMiningInputSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      super$initialize(id, registry)
      private$.currentTextminingResult <- c()
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      private$.currentTextminingResult <- c()
      super$cleanup()
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

        # Populate organism dropdown
        shiny::updateSelectizeInput(
          session, "textmining_organism",
          choices = ORGANISMS$print_name,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          server = TRUE
        )

        # ---------------------------------------------------------------------
        # OBSERVERS
        # ---------------------------------------------------------------------

        # Example button
        private$.observers$example <- shiny::observeEvent(input$textmining_addExample, {
          private$loadTextMiningExample(session)
        }, ignoreInit = TRUE)

        # Clear button
        private$.observers$clear <- shiny::observeEvent(input$textmining_clear, {
          private$resetTextMiningFields(session)
        }, ignoreInit = TRUE)

        # Submit button
        private$.observers$submit <- shiny::observeEvent(input$textmining_submit, {
          private$handleTextMining(input, output)
        }, ignoreInit = TRUE)

        # Select All button - selects all rows via DT proxy
        private$.observers$selectAll <- shiny::observeEvent(input$textmining_selectAll, {
          proxy <- DT::dataTableProxy("extracted_terms")
          if (!is.null(private$.currentTextminingResult)) {
            DT::selectRows(proxy, seq_along(private$.currentTextminingResult))
          }
        }, ignoreInit = TRUE)

        # De-select All button - clears row selection via DT proxy
        private$.observers$selectNone <- shiny::observeEvent(input$textmining_selectNone, {
          proxy <- DT::dataTableProxy("extracted_terms")
          DT::selectRows(proxy, NULL)
        }, ignoreInit = TRUE)

        # Add selected genes to the registry as a new list
        private$.observers$addList <- shiny::observeEvent(input$textmining_addList, {
          private$addTextMiningToFiles(input, session)
        }, ignoreInit = TRUE)

        # Delete button
        private$.observers$delete <- shiny::observeEvent(input$textmining_delete, {
          private$deleteTextmining(output)
        }, ignoreInit = TRUE)

      }) # end moduleServer
    }

    # cleanup() and getRegistry() are inherited from InputSession
  ),

  private = list(
    # .registry, .parentSession, .moduleSession, .observers are inherited from InputSession

    .prefix = "textmining",

    # Text input limits
    .wordLimit = 100000,
    .charLimit = 500000,

    # Current text mining results (IDs for adding to lists)
    .currentTextminingResult = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    loadTextMiningExample = function(session) {
      tryCatch({
        examplePath <- tryCatch(
          getExamplesPath("textmining_example.RDS"),
          error = function(e) "examples/textmining_example.RDS"
        )
        txt_example <- readRDS(examplePath)
        shiny::updateTextAreaInput(session, "textmining_textinput", value = txt_example)
      }, error = function(e) {
        cat(paste("[TextMining] Example error:", conditionMessage(e), "\n"))
        renderError("Text-mining example error.")
      })
    },

    resetTextMiningFields = function(session) {
      tryCatch({
        shiny::updateTextAreaInput(session, "textmining_textinput", value = "")
        shiny::updateSelectizeInput(session, "textmining_organism",
                                    selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]")
      }, error = function(e) {
        cat(paste("[TextMining] Clear error:", conditionMessage(e), "\n"))
        renderError("Text clear error.")
      })
    },

    handleTextMining = function(input, output) {
      tryCatch({
        text <- input$textmining_textinput
        species <- ORGANISMS[ORGANISMS$print_name == input$textmining_organism, ]$taxid

        # JS call to change the default TAGGER_SPECIES VALUE to species
        shinyjs::runjs(sprintf("updateSpecies(%s)", species))

        if (private$hasMinimumLength(text) && private$isWithinLimits(text)) {
          renderModal("<h2>Please wait.</h2><p>Contacting the EXTRACT web server...</p>")

          # Use extract service functions for API calls
          extracted_terms <- extract_entities(text, species)
          if (private$hasTaggedProteins(extracted_terms)) {
            enriched_text <- extract_annotated_html(text, species)
            private$.currentTextminingResult <- unlist(extracted_terms$ID)
            extracted_terms <- private$prepareExtractedTermsForPrint(extracted_terms)
            private$printExtractResults(enriched_text, extracted_terms, output)
          }
        }
      }, error = function(e) {
        cat(paste("[TextMining] Analysis error:", conditionMessage(e), "\n"))
        renderError("An error occurred during text analysis.")
      }, finally = {
        removeModal()
      })
    },

    hasMinimumLength = function(text) {
      if (nchar(text) < 3) {
        renderWarning("Fill the text-search input with at least 3 characters.")
        return(FALSE)
      }
      return(TRUE)
    },

    isWithinLimits = function(text) {
      if (stringr::str_count(text, "\\S+") > private$.wordLimit ||
          nchar(text) > private$.charLimit) {
        renderWarning(sprintf(
          "The submitted text must not be longer than %s words or %s characters.",
          private$.wordLimit, private$.charLimit
        ))
        return(FALSE)
      }
      return(TRUE)
    },

    hasTaggedProteins = function(extracted_terms) {
      if (is.null(extracted_terms)) {
        renderWarning("No genes/proteins found in the submitted text for the selected organism.")
        return(FALSE)
      }
      return(TRUE)
    },

    # Format extracted terms for display in the results DataTable
    prepareExtractedTermsForPrint = function(extracted_terms) {
      extracted_terms <- attachTextMiningDBLinks(extracted_terms)
      extracted_terms <- subset(extracted_terms, select = c(Name, Type, ID))
      names(extracted_terms) <- c("Gene Name", "Species (TaxID)", "ID")
      return(extracted_terms)
    },

    printExtractResults = function(enriched_text, extracted_terms, output) {
      output$extracted_text <- shiny::renderUI({ shiny::HTML(enriched_text) })
      output$extracted_terms <- DT::renderDataTable(
        extracted_terms,
        server = FALSE,
        selection = list(mode = 'multiple', target = 'row'),
        extensions = 'Buttons',
        options = list(
          scrollY = "200px",
          scrollX = TRUE,
          scroller = TRUE,
          dom = 'Blfiprt',
          buttons = createExportButtons("text-mining", c())
        ),
        rownames = FALSE,
        escape = FALSE
      )
      shinyjs::show("textmining_tagger_results")
    },

    addTextMiningToFiles = function(input, session) {
      tryCatch({
        selectedRows <- input$extracted_terms_rows_selected

        if (is.null(selectedRows) || length(selectedRows) == 0) {
          renderWarning("Click on rows to select genes, then click 'Add selected to lists'.")
          return()
        }

        renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")

        ids <- private$.currentTextminingResult[selectedRows]
        ids <- as.character(ids[!is.na(ids) & ids != ""])

        if (length(ids) == 0) {
          renderWarning("No valid gene IDs found in selection.")
          return()
        }

        listName <- self$generateUniqueName(private$.prefix)
        self$addAnalyteList(listName, ids)

        # Clear selection after adding
        proxy <- DT::dataTableProxy("extracted_terms")
        DT::selectRows(proxy, NULL)

      }, error = function(e) {
        cat(paste("[TextMining] Add to list error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      }, finally = {
        removeModal()
      })
    },

    deleteTextmining = function(output) {
      tryCatch({
        private$.currentTextminingResult <- c()
        output$extracted_text <- shiny::renderUI({ shiny::HTML("") })
        output$extracted_terms <- DT::renderDataTable(NULL)
        # shinyjs handles namespace automatically - do NOT use session$ns()
        shinyjs::hide("textmining_tagger_results")
      }, error = function(e) {
        cat(paste("[TextMining] Delete error:", conditionMessage(e), "\n"))
        renderError("Text-mining reset error.")
      })
    }
  )
)
