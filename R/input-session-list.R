# =============================================================================
# FLAME List Input Session
# =============================================================================
#
# Manages the "Upload" tab for adding gene lists via text input or file upload.
# This session only handles CREATING lists - the sidebar/view/rename/remove
# functionality is handled by AnalyteListManagerSession.
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist-registry.R (for AnalyteListRegistry)
#
# =============================================================================

# =============================================================================
# LIST INPUT UI FUNCTION
# =============================================================================

#' Generate the upload panel UI for list input
#' @param id Module namespace ID (must match the ID used in ListInputSession$new)
#' @return A Shiny tabPanel
listInputUploadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Upload",
    icon = shiny::icon("upload"),
    shiny::tags$br(),
    shiny::tags$div(
      shiny::textAreaInput(
        inputId = ns("textAreaList"),
        label = "Paste input list:",
        placeholder = "Write or paste your list here.\nClick the Example Button to load an example list.",
        height = "250px",
        width = "290px"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink("circle-info") %>%
            bsplus::bs_embed_popover(
              title = "Input can consist of mixed typed of IDs separated by comma, space, new line or tab."
            )
        ),
      shiny::actionButton(ns("text_submit"), "Add to lists", shiny::icon("paper-plane")),
      shiny::actionButton(ns("example"), "Example", shiny::icon("bookmark")),
      shiny::actionButton(ns("input_clear"), "Clear", shiny::icon("broom"))
    ),
    shiny::tags$br(),
    shiny::tags$div(
      shiny::fileInput(ns("fileUpload"), "or Upload from file(s):", multiple = TRUE,
                       accept = c(".tsv", ".csv", ".txt")) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink("circle-info") %>%
            bsplus::bs_embed_popover(
              title = "Upload up to 10 files (up to 1MB each)."
            )
        )
    )
  )
}

# =============================================================================
# LIST INPUT SESSION
# =============================================================================

#' List Input Session Class
#'
#' Manages the "Upload" tab for adding gene lists via text input or file upload.
#' This session only handles CREATING lists - the sidebar/view/rename/remove
#' functionality is handled by AnalyteListManagerSession.
#'
#' Inherits from InputSession and uses the shared methods:
#' - addAnalyteList(): Add an analyte list to the registry
#' - generateUniqueName(): Generate unique list names
#' - cleanup(): Destroy observers
#'
#' @section UI Components:
#' - Upload panel: text area, submit/example/clear buttons, file upload
#'
#' @section Dependencies:
#' - AnalyteListRegistry for storing analyte lists
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' listInput <- ListInputSession$new(ModuleIds$INPUT_LIST, registry)
#' listInput$server(input, session)
#' }
#'
ListInputSession <- R6::R6Class(
  "ListInputSession",

  inherit = InputSession,

  public = list(
    #' Initialize a ListInputSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      super$initialize(id, registry)
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

        # ---------------------------------------------------------------------
        # OBSERVERS
        # ---------------------------------------------------------------------

        # Example button
        private$.observers$example <- shiny::observeEvent(input$example, {
          private$handleRandomExample(session)
        }, ignoreInit = TRUE)

        # Clear button
        private$.observers$clear <- shiny::observeEvent(input$input_clear, {
          private$handleClearText(session)
        }, ignoreInit = TRUE)

        # Submit button
        private$.observers$submit <- shiny::observeEvent(input$text_submit, {
          private$handleTextSubmit(input, session)
        }, ignoreInit = TRUE)

        # File upload
        private$.observers$fileUpload <- shiny::observeEvent(input$fileUpload, {
          private$handleInputFiles(input, session)
        }, ignoreInit = TRUE)

      }) # end moduleServer
    }

    # cleanup() and getRegistry() are inherited from InputSession
  ),

  private = list(
    # .registry, .parentSession, .moduleSession, .observers are inherited from InputSession

    .prefix = "gene_list",
    .randomGenesCount = 200,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    handleRandomExample = function(session) {
      tryCatch({
        examplePath <- tryCatch(
          getExamplesPath("random_genes_example.RDS"),
          error = function(e) "examples/random_genes_example.RDS"
        )
        exampleGeneList <- readRDS(examplePath)
        exampleGeneList <- sample(exampleGeneList,
                                  private$.randomGenesCount,
                                  replace = FALSE)
        exampleGeneList <- paste(exampleGeneList, collapse = "\n")
        shiny::updateTextAreaInput(session, "textAreaList", value = exampleGeneList)
      }, error = function(e) {
        cat(paste("[ListInput] Random list error:", conditionMessage(e), "\n"))
        renderError("Random list generation error.")
      })
    },

    handleClearText = function(session) {
      tryCatch({
        shiny::updateTextAreaInput(session, "textAreaList", value = "")
      }, error = function(e) {
        cat(paste("[ListInput] Clear error:", conditionMessage(e), "\n"))
        renderError("List clear error.")
      })
    },

    handleTextSubmit = function(input, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
        private$buildListFromText(input$textAreaList, private$.prefix, session)
      }, error = function(e) {
        cat(paste("[ListInput] Text submit error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      }, finally = {
        removeModal()
      })
    },

    buildListFromText = function(inputText, prefix, session) {
      inputDF <- private$parseTextIntoDataFrame(inputText)

      if (!private$isValidInput(inputDF, prefix)) {
        return(NULL)
      }

      # Create and add (entity/registry handle limit validation)
      listName <- self$generateUniqueName(prefix)
      ids <- as.character(inputDF[[1]])
      self$addAnalyteList(listName, ids)

      # Clear text area
      shiny::updateTextAreaInput(session, "textAreaList", value = "")
      return(listName)
    },

    parseTextIntoDataFrame = function(inputText) {
      # Handle NULL or NA input
      if (is.null(inputText) || length(inputText) == 0) {
        return(data.frame(id = character(0), stringsAsFactors = FALSE))
      }

      # Convert to character if needed
      inputText <- as.character(inputText)
      if (is.na(inputText) || inputText == "") {
        return(data.frame(id = character(0), stringsAsFactors = FALSE))
      }

      # Replace all delimiters with newline
      text <- gsub(",", "\n", inputText)
      text <- gsub(" ", "\n", text)
      text <- gsub("\t", "\n", text)
      text <- gsub("\r", "\n", text)

      # Split and convert to data frame
      items <- strsplit(text, "\n")[[1]]
      items <- items[items != ""]
      items <- unique(items)

      data.frame(id = items, stringsAsFactors = FALSE)
    },

    isValidInput = function(inputDF, prefix) {
      # Check empty
      if (nrow(inputDF) == 0) {
        if (prefix == private$.prefix) {
          renderWarning("Please, paste your input list in the text area first.")
        }
        return(FALSE)
      }

      # Check object size (uses inherited method from InputSession)
      if (private$isInvalidObjectSize(inputDF)) {
        return(FALSE)
      }

      # List limit and gene limit are enforced by registry/entity
      TRUE
    },

    # generateUniqueName() is inherited from InputSession

    handleInputFiles = function(input, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Uploading files.</p>")
        inputFiles <- input$fileUpload

        for (i in seq_len(nrow(inputFiles))) {
          listName <- inputFiles$name[i]

          # Check if name already exists
          if (private$.registry$exists(listName)) {
            renderWarning(paste0(
              "A list named ", listName, " already exists. File was not uploaded."
            ))
            next
          }

          # Read and parse file
          fileData <- readChar(inputFiles$datapath[i],
                               file.info(inputFiles$datapath[i])$size)
          inputDF <- private$parseTextIntoDataFrame(fileData)

          if (!private$isValidInput(inputDF, prefix = "")) {
            next
          }

          # Try to add - entity/registry handle validation
          tryCatch({
            ids <- as.character(inputDF[[1]])
            self$addAnalyteList(listName, ids)
          }, error = function(e) {
            renderWarning(paste0(listName, ": ", conditionMessage(e)))
          })
        }
      }, error = function(e) {
        cat(paste("[ListInput] File upload error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      }, finally = {
        removeModal()
      })
    }
  )
)
