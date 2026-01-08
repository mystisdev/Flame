# =============================================================================
# FLAME AnalyteList Manipulation
# =============================================================================
#
# Classes for manipulating existing analyte lists (viewing, renaming, deleting,
# and creating new lists from set operations like intersections/unions).
#
# This is separate from InputSessions which CREATE analyte lists from external
# input sources (files, text, APIs).
#
# Contains:
# - AnalyteListManagerSession: View, rename, delete existing lists
# - AnalyteListSetOperationsSession: Create new lists from set operations (UpSet plot)
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist.R (for AnalyteListRegistry)
# - func-general.R (for renderModal, removeModal, renderWarning, renderError)
# - upsetjs package
#
# =============================================================================

# Note: Module IDs are defined in infrastructure-config.R (ModuleIds):
# - ModuleIds$INPUT_MANIPULATION_MANAGER for AnalyteListManagerSession
# - ModuleIds$INPUT_MANIPULATION_SETOPERATIONS for AnalyteListSetOperationsSession

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
# UPSET PLOT UI FUNCTION
# =============================================================================

#' Generate the UpSet Plot panel UI
#' @param id Module namespace ID (must match the ID used in AnalyteListSetOperationsSession$new)
#' @return A Shiny tabPanel
upsetPlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "UpSet Plot",
    icon = shiny::icon("chart-column"),
    shiny::tags$div(
      shiny::tags$br(),
      shiny::radioButtons(
        inputId = ns("upsetMode"),
        label = "Select mode:",
        choices = c("Intersection", "Distinct Combinations", "Union"),
        inline = TRUE
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink("circle-info") %>%
            bsplus::bs_embed_popover(
              title = "Select at least 2 files to create the Upset plot from the checkbox file list.
                  \nThe UpSet plot Intersection option visualizes the total number of common elements among the selected sets, even though they may also participate in other sets.
                  \nThe Distinct Combinations option visualizes the common number of genes, among chosen sets, that do not exist in any other set. This option is the closest to a Venn diagram.
                  \nThe Union option appends the unique elements among chosen sets and creates all possible combinations."
            )
        ),
      shiny::actionButton(inputId = ns("submitUpset"), label = "Generate",
                          icon = shiny::icon("palette"), class = "submit_button"),
      upsetjs::upsetjsOutput(ns("upsetjsView")),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::fluidRow(
        shiny::tags$div(
          class = "upsetMargin",
          shiny::column(2, shiny::textOutput(ns("hoveredInfoLabel"))),
          shiny::column(2, shiny::textOutput(ns("hoveredListName"))),
          shiny::column(8, shiny::textOutput(ns("hoveredElements")))
        )
      )
    )
  )
}

# =============================================================================
# ANALYTELIST MANAGER SESSION
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
#'   ModuleIds$INPUT_MANIPULATION_MANAGER, registry
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

# =============================================================================
# ANALYTE LIST OPERATIONS SESSION
# =============================================================================

#' AnalyteList Operations Session Class
#'
#' Manages the UpSet Plot panel for visualizing and operating on set intersections.
#'
#' Inherits from InputSession and uses the shared methods:
#' - addAnalyteList(): Add an analyte list to the registry
#' - generateUniqueName(): Generate unique list names
#' - cleanup(): Destroy observers
#'
#' @section UI Components:
#' - Mode selector: Intersection, Distinct Combinations, Union
#' - Generate button
#' - UpSet plot visualization
#' - Hover info display
#'
#' @section Dependencies:
#' - AnalyteListRegistry for reading and writing analyte lists
#' - upsetjs package for visualization
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' manager <- AnalyteListManagerSession$new(
#'   ModuleIds$INPUT_MANIPULATION_MANAGER, registry
#' )
#' upsetSession <- AnalyteListSetOperationsSession$new(
#'   ModuleIds$INPUT_MANIPULATION_SETOPERATIONS, registry, manager
#' )
#' upsetSession$server(input, session)
#' }
#'
AnalyteListSetOperationsSession <- R6::R6Class(
  "AnalyteListSetOperationsSession",

  inherit = InputSession,

  public = list(
    #' Initialize an AnalyteListSetOperationsSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    #' @param manager AnalyteListManagerSession. Manager for selected lists.
    initialize = function(id, registry, manager) {
      super$initialize(id, registry)
      if (!inherits(manager, "AnalyteListManagerSession")) {
        stop("manager must be an AnalyteListManagerSession")
      }
      private$.manager <- manager
      private$.currentUpsetMode <- ""
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      private$.currentUpsetMode <- ""
      private$.pendingClickData <- NULL
      private$.parentInput <- NULL
      private$.moduleOutput <- NULL
      private$.manager <- NULL
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
      private$.parentInput <- parentInput

      shiny::moduleServer(self$id, function(input, output, session) {
        private$.moduleSession <- session
        private$.moduleOutput <- output

        # ---------------------------------------------------------------------
        # OBSERVERS
        # ---------------------------------------------------------------------

        # Tab visibility - show/hide based on list count (need 2+ for UpSet)
        # This replaces the external toggleUpsetTab() function
        private$.observers$tabVisibility <- shiny::observe({
          # Use reactive method - establishes dependency AND gets count in one call
          listNames <- self$getRegistry()$getNamesReactive()

          if (length(listNames) > 1) {
            shiny::showTab("inputPlots", "UpSet Plot", session = private$.parentSession)
            # Trigger pulse animation on the tab
            private$.parentSession$sendCustomMessage("handler_pulseUpsetTab", list())
          } else {
            shiny::hideTab("inputPlots", "UpSet Plot", session = private$.parentSession)
          }
        })

        # Generate button
        private$.observers$submit <- shiny::observeEvent(input$submitUpset, {
          private$handleUpset(input, output)
        }, ignoreInit = TRUE)

        # Hover event
        private$.observers$hover <- shiny::observeEvent(input$upsetjsView_hover, {
          private$handleUpsetHover(input, output)
        }, ignoreInit = TRUE)

        # Click event
        private$.observers$click <- shiny::observeEvent(input$upsetjsView_click, {
          private$handleUpsetClick(input, session)
        }, ignoreInit = TRUE)

        # Modal OK button - uses parent input since modal is not namespaced
        private$.observers$modalOk <- shiny::observeEvent(private$.parentInput$upsetClick_ok, {
          private$handleUpsetListAccept(session)
        }, ignoreInit = TRUE)

      }) # end moduleServer
    }

    # cleanup() and getRegistry() are inherited from InputSession
  ),

  private = list(
    # .registry, .parentSession, .moduleSession, .observers are inherited from InputSession

    # Reference to AnalyteListManagerSession for accessing selected lists
    .manager = NULL,

    # Parent input for accessing modal buttons
    .parentInput = NULL,

    # Module output for rendering
    .moduleOutput = NULL,

    # Current UpSet mode (Intersection, Distinct Combinations, Union)
    .currentUpsetMode = NULL,

    # Stored click data for modal confirmation
    .pendingClickData = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    handleUpset = function(input, output) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Generating UpSet Plot.</p>")

        selectedNames <- private$.manager$getSelectedNames()
        if (length(selectedNames) < 2) {
          renderWarning("Please, select at least 2 lists.")
          return()
        }

        private$createUpset(input, output, selectedNames)
      }, error = function(e) {
        cat(paste("[UpSet] handleUpset error:", conditionMessage(e), "\n"))
        renderError("Problem with UpSet Plot.")
      }, finally = {
        removeModal()
      })
    },

    createUpset = function(input, output, selectedNames) {
      private$.currentUpsetMode <- input$upsetMode
      upsetModeFunction <- switch(
        private$.currentUpsetMode,
        "Intersection" = upsetjs::generateIntersections,
        "Distinct Combinations" = upsetjs::generateDistinctIntersections,
        "Union" = upsetjs::generateUnions
      )
      private$renderUpsetPlot(output, selectedNames, upsetModeFunction)
      output$hoveredInfoLabel <- shiny::renderText({ "Hovered set:" })
    },

    renderUpsetPlot = function(output, selectedNames, upsetModeFunction) {
      # Build named list for upsetjs
      namedSets <- list()
      for (listName in selectedNames) {
        analyteList <- private$.registry$get(listName)
        if (!is.null(analyteList)) {
          namedSets[[listName]] <- analyteList$getIds()
        }
      }

      output$upsetjsView <- upsetjs::renderUpsetjs({
        upsetjs::upsetjs() %>%
          upsetjs::fromList(namedSets) %>%
          upsetModeFunction() %>%
          upsetjs::interactiveChart()
      })
    },

    handleUpsetHover = function(input, output) {
      tryCatch({
        hoverSet <- input$upsetjsView_hover
        hoveredElements <- paste(hoverSet$elems, collapse = ', ')
        output$hoveredListName <- shiny::renderText({ hoverSet$name })
        output$hoveredElements <- shiny::renderText({ hoveredElements })
      }, error = function(e) {
        cat(paste("[UpSet] handleUpsetHover error:", conditionMessage(e), "\n"))
        renderError("Problem with UpSet Plot hover.")
      })
    },

    handleUpsetClick = function(input, session) {
      tryCatch({
        upsetjs_click <- input$upsetjsView_click
        if (!identical(as.character(upsetjs_click$elems), character(0))) {
          # Store click data for use when modal is confirmed
          private$.pendingClickData <- upsetjs_click

          shiny::showModal(shiny::modalDialog(
            title = paste0(private$.currentUpsetMode, " clicked set."),
            paste0("Add: ", upsetjs_click$name, " to the lists?"),
            footer = shiny::tagList(
              # Modal buttons are NOT namespaced - they go to parent session
              shiny::actionButton("upsetClick_ok", "OK"),
              shiny::modalButton("Cancel")
            )
          ))
        }
      }, error = function(e) {
        cat(paste("[UpSet] handleUpsetClick error:", conditionMessage(e), "\n"))
        renderError("Problem with UpSet Plot click.")
      })
    },

    handleUpsetListAccept = function(session) {
      tryCatch({
        upsetjs_click <- private$.pendingClickData
        if (is.null(upsetjs_click)) {
          shiny::removeModal()
          return()
        }

        prefix <- switch(
          private$.currentUpsetMode,
          "Intersection" = "intersect",
          "Distinct Combinations" = "distinct",
          "Union" = "union"
        )

        clickedElements <- as.character(upsetjs_click$elems)

        listName <- paste(prefix, upsetjs_click$name, sep = "_")
        if (private$notExistsListName(listName)) {
          # Entity/registry handle limit validation
          self$addAnalyteList(listName, clickedElements)
        }

        private$.pendingClickData <- NULL
        shiny::removeModal()
      }, error = function(e) {
        cat(paste("[UpSet] handleUpsetListAccept error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      })
    },

    # Check if list name doesn't already exist in registry
    notExistsListName = function(listName) {
      if (private$.registry$exists(listName)) {
        renderWarning(paste0("Duplicate name: ", listName, ". List was not added."))
        return(FALSE)
      }
      return(TRUE)
    }
  )
)
