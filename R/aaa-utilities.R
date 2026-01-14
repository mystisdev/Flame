# =============================================================================
# SHARED UTILITY FUNCTIONS
# =============================================================================
# Functions used across the entire codebase that don't belong to any single
# session class. These are low-level utilities for rendering, alerts, and
# data export.
#
# NOTE: This file is named aaa-* to ensure it loads early in R's alphabetical
# loading order, as these utilities are used by many other files.
# =============================================================================

# --- Alert Functions ---------------------------------------------------------

renderError <- function(prompt) {
  if (exists("session"))
    shinyalert::shinyalert("Error!", prompt, type = "error")
}

renderWarning <- function(prompt) {
  if (exists("session"))
    shinyalert::shinyalert("Warning!", prompt, type = "warning")
}

renderModal <- function(prompt) {
  showModal(modalDialog(HTML(prompt), footer = NULL))
}

# --- Output Rendering --------------------------------------------------------

renderShinyText <- function(shinyOutputId, prompt) {
  output[[shinyOutputId]] <- renderText(prompt)
}

# --- API Utilities -----------------------------------------------------------

#' Check if a POST response is valid
#'
#' Validates HTTP response from external APIs. Returns FALSE and shows warning
#' if status code is not 200 or if content is empty.
#'
#' @param request httr response object
#' @return TRUE if response is valid, FALSE otherwise
isPOSTResponseValid <- function(request) {
  isValid <- TRUE
  if (request$status_code != 200) {
    isValid <- FALSE
    renderWarning("Invalid response from the called API.
                  Please try again in a while.")
  } else if (identical(request$content, raw(0))) {
    isValid <- FALSE
  }
  return(isValid)
}

# --- Export Utilities --------------------------------------------------------

#' Create export buttons for DT DataTables
#'
#' Creates a standard set of export buttons (Excel, CSV, Copy, PDF, Print)
#' with optional column exclusion for sensitive or internal columns.
#'
#' @param fileName Base filename for exports (without extension)
#' @param excludeColumns Vector of 0-indexed column indices to exclude from exports
#' @return List of button configurations for DT
createExportButtons <- function(fileName, excludeColumns = c()) {
  if (length(excludeColumns) > 0) {
    columnSelector <- JS(sprintf(
      "function(idx, data, node) { return [%s].indexOf(idx) === -1; }",
      paste(excludeColumns, collapse = ",")
    ))
    exportOpts <- list(columns = columnSelector)
  } else {
    exportOpts <- list()
  }

  list(
    list(extend = 'excel', filename = fileName, exportOptions = exportOpts),
    list(extend = 'csv', filename = fileName, exportOptions = exportOpts),
    list(extend = 'copy', exportOptions = exportOpts),
    list(extend = 'pdf', filename = fileName,
         exportOptions = c(exportOpts, list(orthogonal = "export")),
         orientation = "landscape"),
    list(extend = 'print', exportOptions = exportOpts)
  )
}

#' Render a standard DataTable with export buttons
#'
#' Renders a simple DT DataTable with export buttons (Excel, CSV, PDF, etc.)
#' for displaying tabular data. This is a basic table without expandable rows
#' or click handlers - use for simple data display like conversion tables.
#'
#' Used by:
#' - ORAEnrichmentSession: Gene conversion tables (input and background lists)
#' - CombinationSession: Combination network table
#'
#' @param shinyOutputId Output ID for the table
#' @param outputData Data frame to display
#' @param caption Optional table caption
#' @param fileName Base filename for exports
#' @param scrollY Optional scroll height
#' @param hiddenColumns Vector of 0-indexed column indices to hide
#' @param filter Filter type ("none", "top", "bottom")
renderShinyDataTable <- function(shinyOutputId, outputData,
                                 caption = NULL, fileName = "",
                                 scrollY = NULL, hiddenColumns = c(),
                                 filter = "none") {
  output[[shinyOutputId]] <- DT::renderDataTable(
    outputData,
    server = F,
    selection = 'none',
    extensions = 'Buttons',
    caption = caption,
    options = list(
      scrollY = scrollY,
      scrollX = TRUE,
      scroller = T,
      "dom" = 'Blfiprt',
      buttons = createExportButtons(fileName, hiddenColumns),
      columnDefs = list(
        list(visible = F, targets = hiddenColumns)
      )
    ),
    filter = filter,
    rownames = F,
    escape = F
  )
}
