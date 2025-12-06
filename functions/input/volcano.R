handleVolcanoPlot <- function(readCallBackFunction) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Loading Volcano data.</p>")
    volcanoInput <- readCallBackFunction()

    # Validate input (throws exception if invalid)
    if (!isValidVolcanoInput(volcanoInput)) {
      stop("Invalid format")
    }

    showTab("inputPlots", "Volcano Plot")
    updateTabsetPanel(session, "inputPlots", selected = "Volcano Plot")

    # Display data table preview
    renderShinyDataTable("volcanoViewer", volcanoInput,
                         fileName = "volcano")

    # Prepare plot data and populate dropdowns
    currentVolcano <<- prepareVolcanoPlot(volcanoInput)

    # Clear plot and reset UI
    output$volcanoPlot <- renderPlotly(c())
    renderShinyText("volcanoPlotStatus", "")
    renderShinyText("volcanoSelected", "")
    volcanoSelectedItems <<- c()
    shinyjs::show("volcanoSelectionInfo")
    shinyjs::show("volcanoPanel")
  }, error = function(e) {
    cat(paste0("[Volcano] ", e, "\n"))
    renderWarning(paste0(
      "Could not read file. Expected format:\n",
      "  \u2022 .csv (comma-separated) or .tsv/.txt (tab-separated)\n",
      "  \u2022 At least 3 columns\n",
      "  \u2022 At least 2 numeric columns (for logFC and p-value)\n",
      "You'll select which columns to use after upload."
    ))
  }, finally = {
    removeModal()
  })
}

readVolcanoInput <- function() {
  volcanoFile <- input$volcanoUpload
  fileType <- tolower(tools::file_ext(volcanoFile$name))

  # Treat empty strings as NA for consistency with reduction input
  if (fileType == "tsv" || fileType == "txt") {
    volcanoInput <- read.delim(volcanoFile$datapath, header = TRUE, na.strings = c("", "NA"))
  } else if (fileType == "csv") {
    volcanoInput <- read.csv(volcanoFile$datapath, header = TRUE, na.strings = c("", "NA"))
  }
  return(volcanoInput)
}

readVolcanoExample <- function() {
  volcanoInput <- readRDS("examples/volcano_example.RDS")
  return(volcanoInput)
}

isValidVolcanoInput <- function(volcanoInput) {
  isValid <- FALSE
  if (areInvalidVolcanoColumns(volcanoInput))
    return(isValid)
  if (isInvalidObjectSize(volcanoInput, prefix = "volcano"))
    return(isValid)
  isValid <- TRUE
  return(isValid)
}

areInvalidVolcanoColumns <- function(volcanoInput) {
  # Must have at least 3 columns total
  if (ncol(volcanoInput) < 3) {
    return(TRUE)
  }

  # Must have at least 2 numeric columns (for logFC and pvalue)
  numeric_cols <- sapply(volcanoInput, is.numeric)
  if (sum(numeric_cols) < 2) {
    return(TRUE)
  }

  return(FALSE)
}

prepareVolcanoPlot <- function(volcanoInput) {
  # Identify column types
  all_cols <- names(volcanoInput)
  numeric_cols <- all_cols[sapply(volcanoInput, is.numeric)]

  # Update Gene column dropdown (all columns)
  updateSelectInput(session, "volcano_gene_col",
                    choices = c("", all_cols),
                    selected = "")

  # Update LogFC dropdown (numeric only)
  updateSelectInput(session, "volcano_logfc_col",
                    choices = c("", numeric_cols),
                    selected = "")

  # Update P-value dropdown (numeric only)
  updateSelectInput(session, "volcano_pvalue_col",
                    choices = c("", numeric_cols),
                    selected = "")

  return(volcanoInput)
}

appendVolcanoColorColumn <- function() {
  pvalueThreshold <- input$volcano_pvalue_slider
  logFCThreshold <- input$volcano_fc_slider
  volcanoPlotData$expression <<- "default"
  if (nrow(volcanoPlotData[(volcanoPlotData$logfc > logFCThreshold) &
                        (volcanoPlotData$`-log10Pvalue` > pvalueThreshold), ]) > 0)
    volcanoPlotData[(volcanoPlotData$logfc > logFCThreshold) &
                   (volcanoPlotData$`-log10Pvalue` > pvalueThreshold), ]$expression <<- "overexpressed"
  if (nrow(volcanoPlotData[(volcanoPlotData$logfc < -logFCThreshold) &
                        (volcanoPlotData$`-log10Pvalue` > pvalueThreshold), ]) > 0)
    volcanoPlotData[(volcanoPlotData$logfc < -logFCThreshold) &
                   (volcanoPlotData$`-log10Pvalue` > pvalueThreshold), ]$expression <<- "underexpressed"
}

handleVolcanoRedraw <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Redrawing Volcano Plot.</p>")
    appendVolcanoColorColumn()
    renderVolcano()
  }, error = function(e) {
    cat(paste("Volcano redraw error:  ", e))
    renderError("Volcano redraw error.")
  }, finally = {
    removeModal()
  })
}

handleVolcanoSubmit <- function() {
  tryCatch({
    if (existVolcanoSelectedItems()) {
      showModal(modalDialog(
        title = "Volcano selected set",
        paste0("Add the selected items to the lists?"),
        footer = tagList(
          actionButton("volcano_ok", "OK"),
          modalButton("Cancel")
        )
      ))
    }
  }, error = function(e) {
    cat(paste("Volcano submit selected list error:  ", e))
    renderError("Volcano submit selected list error.")
  })
}

existVolcanoSelectedItems <- function() {
  if (length(volcanoSelectedItems) == 0) {
    renderWarning("Please select points on the plot using box or lasso selection tool.")
    return(FALSE)
  }
  return(TRUE)
}

handleVolcanoListAccept <- function() {
  tryCatch({
    volcanoDF <- as.data.frame(volcanoSelectedItems)
    listName <- getRandomListName(VOLCANO_PREFIX)
    updateUserInputLists(volcanoDF, listName)
    updateCheckboxInput(session, "selectAll", value = 0)
    updateListBoxes()
    removeModal()
  }, error = function(e) {
    cat(paste0("[Volcano] ", e, "\n"))
    renderError("Problem with Volcano Plot list selection.")
  })
}

# Update dropdown choices to exclude already-selected columns
updateVolcanoDropdownChoices <- function() {
  if (is.null(currentVolcano) || nrow(currentVolcano) == 0) return()

  # Get current selections
  gene_col <- input$volcano_gene_col
  logfc_col <- input$volcano_logfc_col
  pvalue_col <- input$volcano_pvalue_col

  # Get all columns
  all_cols <- names(currentVolcano)
  numeric_cols <- all_cols[sapply(currentVolcano, is.numeric)]

  # Build list of selected columns (excluding empty strings)
  selected_cols <- c(gene_col, logfc_col, pvalue_col)
  selected_cols <- selected_cols[selected_cols != "" & !is.null(selected_cols)]

  # Helper function: get available choices excluding other selections
  get_available <- function(base_choices, current_selection) {
    other_selections <- selected_cols[selected_cols != current_selection]
    setdiff(base_choices, other_selections)
  }

  # Update each dropdown with available choices
  updateSelectInput(session, "volcano_gene_col",
                    choices = c("", get_available(all_cols, gene_col)),
                    selected = gene_col)

  updateSelectInput(session, "volcano_logfc_col",
                    choices = c("", get_available(numeric_cols, logfc_col)),
                    selected = logfc_col)

  updateSelectInput(session, "volcano_pvalue_col",
                    choices = c("", get_available(numeric_cols, pvalue_col)),
                    selected = pvalue_col)
}

# Handle Volcano plot generation
handleVolcanoGenerate <- function() {
  tryCatch({
    # Validate required selections
    gene_col <- input$volcano_gene_col
    logfc_col <- input$volcano_logfc_col
    pvalue_col <- input$volcano_pvalue_col

    if (is.null(gene_col) || gene_col == "") {
      renderWarning("Please select the Gene column.")
      return()
    }

    if (is.null(logfc_col) || logfc_col == "") {
      renderWarning("Please select the LogFC column.")
      return()
    }

    if (is.null(pvalue_col) || pvalue_col == "") {
      renderWarning("Please select the P-value column.")
      return()
    }

    renderModal("<h2>Please wait.</h2><br /><p>Generating Volcano plot.</p>")

    # Calculate exclusions and prepare data
    total_genes <- nrow(currentVolcano)
    gene_col_data <- currentVolcano[[gene_col]]
    logfc_col_data <- currentVolcano[[logfc_col]]
    pvalue_col_data <- currentVolcano[[pvalue_col]]

    # Count different types of missing/invalid data
    missing_gene_names <- is.na(gene_col_data) | gene_col_data == ""
    missing_logfc <- is.na(logfc_col_data)
    missing_pvalue <- is.na(pvalue_col_data) | pvalue_col_data <= 0
    excluded <- missing_gene_names | missing_logfc | missing_pvalue

    total_excluded <- sum(excluded)
    total_plotted <- total_genes - total_excluded

    # Build status message
    if (total_excluded > 0) {
      exclusion_parts <- c()
      if (sum(missing_gene_names) > 0) {
        exclusion_parts <- c(exclusion_parts,
                            paste0(sum(missing_gene_names), " missing gene names"))
      }
      if (sum(missing_logfc) > 0) {
        exclusion_parts <- c(exclusion_parts,
                            paste0(sum(missing_logfc), " missing logFC values"))
      }
      if (sum(missing_pvalue) > 0) {
        exclusion_parts <- c(exclusion_parts,
                            paste0(sum(missing_pvalue), " invalid p-values"))
      }

      status_msg <- paste0(
        "Plotting ", total_plotted, " of ", total_genes, " genes (",
        total_excluded, " excluded: ", paste(exclusion_parts, collapse = ", "), ")"
      )
    } else {
      status_msg <- paste0("Plotting ", total_plotted, " genes")
    }

    renderShinyText("volcanoPlotStatus", status_msg)

    # Prepare plot data with transformed pvalue and apply color column
    prepareVolcanoForRendering()
    appendVolcanoColorColumn()

    # Render the plot
    renderVolcano()

    removeModal()

  }, error = function(e) {
    cat(paste0("[Volcano] ", e, "\n"))
    renderError("Volcano plot generation error.")
    removeModal()
  })
}

# Prepare volcano data for rendering (transform pvalue, update sliders)
prepareVolcanoForRendering <- function() {
  gene_col <- input$volcano_gene_col
  logfc_col <- input$volcano_logfc_col
  pvalue_col <- input$volcano_pvalue_col

  # Filter out invalid rows
  gene_col_data <- currentVolcano[[gene_col]]
  logfc_col_data <- currentVolcano[[logfc_col]]
  pvalue_col_data <- currentVolcano[[pvalue_col]]

  valid_rows <- !is.na(gene_col_data) & gene_col_data != "" &
                !is.na(logfc_col_data) &
                !is.na(pvalue_col_data) & pvalue_col_data > 0

  # Create working copy with standardized column names for rendering
  volcanoPlotData <<- data.frame(
    symbol = currentVolcano[[gene_col]][valid_rows],
    logfc = currentVolcano[[logfc_col]][valid_rows],
    `-log10Pvalue` = -log10(currentVolcano[[pvalue_col]][valid_rows]),
    check.names = FALSE
  )

  # Update sliders based on data range
  maxAbsoluteLogFC <- max(max(volcanoPlotData$logfc), abs(min(volcanoPlotData$logfc)))
  updateVolcanoSliders(max(volcanoPlotData$`-log10Pvalue`), maxAbsoluteLogFC)
}

# Handle clear button
handleVolcanoClear <- function() {
  tryCatch({
    updateSelectInput(session, "volcano_gene_col", selected = "")
    updateSelectInput(session, "volcano_logfc_col", selected = "")
    updateSelectInput(session, "volcano_pvalue_col", selected = "")

    # Clear plot and status
    output$volcanoPlot <- renderPlotly(c())
    renderShinyText("volcanoPlotStatus", "")
    renderShinyText("volcanoSelected", "")
    volcanoSelectedItems <<- c()
  }, error = function(e) {
    cat(paste0("[Volcano] ", e, "\n"))
    renderError("Volcano clear error.")
  })
}
