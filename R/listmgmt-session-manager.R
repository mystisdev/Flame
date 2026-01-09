# =============================================================================
# FLAME AnalyteList Manager Session
# =============================================================================
#
# Manages the shared UI for viewing, renaming, and removing analyte lists.
# This is separate from InputSessions which CREATE analyte lists.
#
# Dependencies:
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - func-general.R (for renderModal, removeModal, renderWarning, renderError)
#
# Note: Module ID is defined in infrastructure-config.R (ModuleIds):
# - ModuleIds$LISTMGMT_MANAGER
#
# =============================================================================

# =============================================================================
# ANALYTELIST MANAGER UI FUNCTIONS
# =============================================================================

#' Generate the current lists sidebar UI
#' @param id Module namespace ID (must match the ID used in AnalyteListManagerSession$new)
#' @return A Shiny column
analyteListManagerSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::column(
    4,
    shiny::tags$div(
      class = "checkListDiv",
      shiny::checkboxGroupInput(
        inputId = ns("checkboxLists"),
        label = "Current lists:"
      ),
      shiny::checkboxInput(ns("selectAll"), "Select/Deselect All"),
      shiny::actionButton(ns("rename"), "Rename", shiny::icon("pencil")),
      shiny::actionButton(ns("remove"), "Remove", shiny::icon("trash"))
    )
  )
}

#' Generate the view panel UI for analyte lists
#' @param id Module namespace ID (must match the ID used in AnalyteListManagerSession$new)
#' @return A Shiny tabPanel
analyteListManagerViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "View",
    icon = shiny::icon("eye"),
    shiny::tags$br(),
    shiny::selectInput(
      inputId = ns("selectView"),
      label = "Select list to view:",
      choices = NULL,
      width = "100%"
    ),
    DT::dataTableOutput(ns("selectedListView"))
  )
}

# =============================================================================
# ANALYTELIST MANAGER SESSION CLASS
# =============================================================================

#' AnalyteList Manager Session Class
#'
#' Manages the shared UI for viewing, renaming, and removing analyte lists.
#' This is separate from InputSessions which CREATE analyte lists.
#'
#' @section UI Components:
#' - Sidebar: checkbox group for current lists, select all, rename/remove buttons
#' - View panel: select dropdown and data table
#'
#' @section Responsibilities:
#' - Display list of all analyte lists in sidebar checkboxes
#' - Handle select all/deselect all
#' - Handle renaming (inline edit via JS)
#' - Handle removal of selected lists
#' - Display selected list contents in view panel
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' manager <- AnalyteListManagerSession$new(
#'   ModuleIds$LISTMGMT_MANAGER, registry
#' )
#' manager$server(input, session)
#' }
#'
AnalyteListManagerSession <- R6::R6Class(
  "AnalyteListManagerSession",

  public = list(
    #' @field id Module namespace ID
    id = NULL,

    #' Initialize an AnalyteListManagerSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for managing analyte lists.
    initialize = function(id, registry) {
      if (!inherits(registry, "AnalyteListRegistry")) {
        stop("registry must be an AnalyteListRegistry")
      }
      self$id <- id
      private$.registry <- registry
      private$.observers <- list()
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      private$.renameQueue <- NULL
      private$.renameIndex <- NULL
      private$.currentRenameName <- NULL
      private$.moduleInput <- NULL
      private$.moduleSession <- NULL
      private$.parentSession <- NULL
      for (obs in private$.observers) {
        if (!is.null(obs)) obs$destroy()
      }
      private$.observers <- list()
    },

    #' Initialize server logic
    #' @param parentInput Parent session's input object
    #' @param parentSession Parent session object
    server = function(parentInput, parentSession) {
      private$.parentSession <- parentSession

      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.moduleInput <- input

        # -----------------------------------------------------------------
        # OBSERVERS
        # -----------------------------------------------------------------

        # Select all checkbox
        private$.observers$selectAll <- shiny::observeEvent(input$selectAll, {
          private$handleSelectAll(input, session)
        }, ignoreInit = TRUE)

        # Rename button - starts the rename modal sequence
        private$.observers$rename <- shiny::observeEvent(input$rename, {
          private$handlePrepareRename(input, session)
        }, ignoreInit = TRUE)

        # Rename modal - character count updates
        private$.observers$renameInput <- shiny::observeEvent(input$rename_newName, {
          if (!is.null(input$rename_newName)) {
            session$sendCustomMessage("handler_updateCharCount", list(
              count = nchar(input$rename_newName),
              limit = private$.maxNameLength,
              inputId = session$ns("rename_charCount")
            ))
          }
        }, ignoreInit = TRUE)

        # Rename modal - confirm button
        private$.observers$renameConfirm <- shiny::observeEvent(input$rename_confirm, {
          private$handleRenameConfirm(input, session)
        }, ignoreInit = TRUE)

        # Remove button
        private$.observers$remove <- shiny::observeEvent(input$remove, {
          private$handleRemove(input, output, session)
        }, ignoreInit = TRUE)

        # View selector
        private$.observers$selectView <- shiny::observeEvent(input$selectView, {
          private$handleSelectView(input, output, session)
        }, ignoreInit = TRUE)

        # -----------------------------------------------------------------
        # REACTIVE UI UPDATES
        # -----------------------------------------------------------------

        # Update checkbox list when registry changes
        private$.observers$updateCheckboxes <- shiny::observe({
          names <- private$.registry$getNamesReactive()
          # Preserve current selections when updating choices
          currentlySelected <- input$checkboxLists
          validSelections <- intersect(currentlySelected, names)
          shiny::updateCheckboxGroupInput(session, "checkboxLists",
                                          choices = names,
                                          selected = validSelections)
        })

        # Update view selector when registry changes
        private$.observers$updateViewSelector <- shiny::observe({
          names <- private$.registry$getNamesReactive()
          shiny::updateSelectInput(session, "selectView", choices = names)
        })

      }) # end moduleServer
    },

    #' Get the registry
    #' @return AnalyteListRegistry
    getRegistry = function() {
      private$.registry
    },

    #' Get currently selected list names from sidebar checkbox
    #' @return Character vector of selected names (NULL if not initialized)
    getSelectedNames = function() {
      if (is.null(private$.moduleInput)) return(NULL)
      private$.moduleInput$checkboxLists
    }
  ),

  private = list(
    # Maximum characters allowed for list names
    .maxNameLength = 100,

    # Reference to the AnalyteListRegistry
    .registry = NULL,

    # List of observers for cleanup
    .observers = list(),

    # Parent Shiny session
    .parentSession = NULL,

    # Module Shiny session
    .moduleSession = NULL,

    # Module input object (for cross-module access via getSelectedNames)
    .moduleInput = NULL,

    # Rename queue for sequential modal processing
    .renameQueue = NULL,
    .renameIndex = NULL,
    .currentRenameName = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    # Create the rename modal dialog
    createRenameModal = function(ns, currentName, charLimit) {
      shiny::modalDialog(
        title = paste("Rename list:", currentName),
        shiny::tagList(
          shiny::textInput(ns("rename_newName"), label = NULL, value = currentName, width = "100%"),
          shiny::tags$div(
            id = ns("rename_charCount"), class = "rename-char-count",
            paste0(nchar(currentName), "/", charLimit, " characters")
          ),
          shiny::tags$div(class = "rename-error", style = "display: none;")
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("rename_confirm"), "Rename", class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),
        easyClose = FALSE
      )
    },

    handleSelectAll = function(input, session) {
      tryCatch({
        names <- private$.registry$getNames()
        if (isFALSE(input$selectAll) || input$selectAll == 0) {
          shiny::updateCheckboxGroupInput(session, "checkboxLists",
                                          choices = names, selected = NULL)
        } else {
          shiny::updateCheckboxGroupInput(session, "checkboxLists",
                                          choices = names, selected = names)
        }
      }, error = function(e) {
        cat(paste("[Manager] Select all error:", conditionMessage(e), "\n"))
        renderError("List selection error.")
      })
    },

    handlePrepareRename = function(input, session) {
      tryCatch({
        selectedNames <- input$checkboxLists
        if (length(selectedNames) == 0) {
          renderWarning("Please select at least one list to rename.")
          return()
        }
        private$.renameQueue <- selectedNames
        private$.renameIndex <- 1
        private$showRenameModalForCurrent(session)
      }, error = function(e) {
        cat(paste("[Manager] Prepare rename error:", conditionMessage(e), "\n"))
        renderError("List rename error.")
      })
    },

    showRenameModalForCurrent = function(session) {
      if (private$.renameIndex > length(private$.renameQueue)) {
        # All done - cleanup
        private$.renameQueue <- NULL
        private$.renameIndex <- NULL
        private$.currentRenameName <- NULL
        shiny::updateCheckboxInput(session, "selectAll", value = FALSE)
        return()
      }
      private$.currentRenameName <- private$.renameQueue[private$.renameIndex]
      shiny::showModal(private$createRenameModal(
        session$ns,
        private$.currentRenameName,
        private$.maxNameLength
      ))
    },

    validateRename = function(input) {
      newName <- trimws(input$rename_newName)
      oldName <- private$.currentRenameName

      if (newName == "") {
        return(list(valid = FALSE, error = "Name cannot be empty."))
      }
      if (nchar(newName) > private$.maxNameLength) {
        return(list(valid = FALSE,
          error = paste0("Name exceeds ", private$.maxNameLength, " characters.")))
      }
      if (newName != oldName && private$.registry$exists(newName)) {
        return(list(valid = FALSE,
          error = paste0("A list named '", newName, "' already exists.")))
      }
      return(list(valid = TRUE, newName = newName))
    },

    handleRenameConfirm = function(input, session) {
      validation <- private$validateRename(input)
      if (!validation$valid) {
        session$sendCustomMessage("handler_showRenameError", validation$error)
        return()
      }

      oldName <- private$.currentRenameName
      newName <- validation$newName
      if (newName != oldName) {
        private$.registry$rename(oldName, newName)
      }

      shiny::removeModal()
      private$.renameIndex <- private$.renameIndex + 1
      private$showRenameModalForCurrent(session)
    },

    handleRemove = function(input, output, session) {
      tryCatch({
        selectedNames <- input$checkboxLists

        if (length(selectedNames) > 0) {
          for (name in selectedNames) {
            private$.registry$remove(name)
          }

          # Clear table if no lists left
          if (private$.registry$count() == 0) {
            output$selectedListView <- DT::renderDataTable({
              data.frame()
            })
          }

          shiny::updateCheckboxInput(session, "selectAll", value = FALSE)
        }
      }, error = function(e) {
        cat(paste("[Manager] Remove error:", conditionMessage(e), "\n"))
        renderError("Problem with list removal.")
      })
    },

    handleSelectView = function(input, output, session) {
      tryCatch({
        selectedName <- input$selectView

        if (!is.null(selectedName) && selectedName != "") {
          analyteList <- private$.registry$get(selectedName)
          if (!is.null(analyteList)) {
            outputData <- analyteList$toDataFrame()
            output$selectedListView <- DT::renderDataTable(
              outputData,
              server = FALSE,
              selection = "none",
              extensions = "Buttons",
              options = list(
                scrollX = TRUE,
                dom = "Blfiprt",
                buttons = list(
                  list(extend = "excel", filename = selectedName),
                  list(extend = "csv", filename = selectedName),
                  list(extend = "copy"),
                  list(extend = "pdf", filename = selectedName, orientation = "landscape"),
                  list(extend = "print")
                )
              ),
              rownames = FALSE,
              escape = FALSE
            )
          }
        }
      }, error = function(e) {
        cat(paste("[Manager] View error:", conditionMessage(e), "\n"))
        renderError("Problem with list viewing.")
      })
    }
  )
)
