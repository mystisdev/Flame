# =============================================================================
# FLAME AnalyteList Set Operations Session
# =============================================================================
#
# Manages the UpSet Plot panel for visualizing and operating on set intersections.
# Creates new analyte lists from set operations (intersection, union, distinct).
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist-registry.R (for AnalyteListRegistry)
# - input-analytelist-unranked.R (for UnrankedAnalyteList)
# - infrastructure-config.R (for AnalyteType)
# - listmgmt-session-manager.R (for AnalyteListManagerSession)
# - func-general.R (for renderModal, removeModal, renderWarning, renderError)
# - upsetjs package
#
# Note: Module ID is defined in infrastructure-config.R (ModuleIds):
# - ModuleIds$INPUT_MANIPULATION_SETOPERATIONS
#
# =============================================================================

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
# ANALYTELIST SET OPERATIONS SESSION CLASS
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
