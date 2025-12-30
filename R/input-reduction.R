# Main handler for 2D reduction plot upload
handleReductionInput <- function(readCallBackFunction) {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Loading 2D Reduction data.</p>")
    reductionInput <- readCallBackFunction()

    # Validate input (throws exception if invalid)
    if (!isValidReductionInput(reductionInput)) {
      stop("Invalid format")
    }

    showTab("inputPlots", "2D Reduction Plot")
    updateTabsetPanel(session, "inputPlots", selected = "2D Reduction Plot")

    # Display data table preview
    renderShinyDataTable("reductionViewer", reductionInput,
                         fileName = "reduction")

    # Prepare plot data and populate dropdowns
    currentReduction <<- prepareReductionPlot(reductionInput)

    # Clear plot and reset UI
    output$reductionPlot <- renderPlotly(c())
    renderShinyText("reductionPlotStatus", "")
    renderShinyText("reductionSelected", "")
    reductionSelectedItems <<- c()
    shinyjs::show("reductionSelectionInfo")
    shinyjs::show("reductionPanel")
  }, error = function(e) {
    cat(paste0("[Reduction] ", e, "\n"))
    renderWarning(paste0(
      "Could not read file. Expected format:\n",
      "  \u2022 .csv (comma-separated) or .tsv/.txt (tab-separated)\n",
      "  \u2022 At least 1 identifier column (e.g. gene symbols)\n",
      "  \u2022 At least 2 numeric columns (X/Y coordinates)\n",
      "Additional columns optional (for color/size). You'll select which columns to use after upload"
    ))
  }, finally = {
    removeModal()
  })
}

# Read 2D reduction input file
readReductionInput <- function() {
  reductionFile <- input$reductionUpload
  fileType <- tolower(tools::file_ext(reductionFile$name))

  # Treat empty strings as NA for consistency
  if (fileType == "tsv" || fileType == "txt") {
    reductionInput <- read.delim(reductionFile$datapath, header = TRUE, na.strings = c("", "NA"))
  } else if (fileType == "csv") {
    reductionInput <- read.csv(reductionFile$datapath, header = TRUE, na.strings = c("", "NA"))
  } else {
    stop("Unsupported file type: ", fileType)
  }

  return(reductionInput)
}

# Load example 2D reduction data
readReductionExample <- function() {
  # Load full example data
  examplePath <- tryCatch(getExamplesPath("reduction_example.RDS"),
                          error = function(e) "examples/reduction_example.RDS")
  reductionInput <- readRDS(examplePath)

  # Define the three reduction method coordinate pairs
  reduction_methods <- list(
    c("PC1", "PC2"),
    c("UMAP1", "UMAP2"),
    c("TSNE1", "TSNE2")
  )

  # Randomly select one method to keep example compact and demonstrate
  # different coordinate types (PCA/UMAP/t-SNE) on repeated use
  selected_coords <- sample(reduction_methods, 1)[[1]]

  # Keep only: symbol, selected coordinates, and other attributes
  columns_to_keep <- c("symbol", selected_coords, "expression", "cluster", "gene_type")

  # Filter and return
  reductionInput <- reductionInput[, columns_to_keep]
  return(reductionInput)
}

# Validate 2D reduction input
isValidReductionInput <- function(reductionInput) {
  !areInvalidReductionColumns(reductionInput) &&
  !isInvalidObjectSize(reductionInput, prefix = REDUCTION_PREFIX)
}

# Validate columns
areInvalidReductionColumns <- function(reductionInput) {
  # Must have at least 3 columns total
  if (ncol(reductionInput) < 3) {
    return(TRUE)
  }

  # Must have at least 2 numeric columns
  numeric_cols <- sapply(reductionInput, is.numeric)
  if (sum(numeric_cols) < 2) {
    return(TRUE)
  }

  return(FALSE)
}

# Prepare 2D reduction plot data and populate dropdowns
prepareReductionPlot <- function(reductionInput) {
  # Identify column types
  all_cols <- names(reductionInput)
  numeric_cols <- all_cols[sapply(reductionInput, is.numeric)]

  # Update Gene column dropdown (all columns)
  updateSelectInput(session, "reduction_gene_col",
                    choices = c("", all_cols),
                    selected = "")

  # Update X/Y axis dropdowns (numeric only)
  updateSelectInput(session, "reduction_x_axis",
                    choices = c("", numeric_cols),
                    selected = "")
  updateSelectInput(session, "reduction_y_axis",
                    choices = c("", numeric_cols),
                    selected = "")

  # Update Color dropdown (all columns)
  updateSelectInput(session, "reduction_color",
                    choices = c("", all_cols),
                    selected = "")

  # Update Size dropdown (numeric only)
  updateSelectInput(session, "reduction_size",
                    choices = c("", numeric_cols),
                    selected = "")

  return(reductionInput)
}

# Update dropdown choices to exclude already-selected columns
updateReductionDropdownChoices <- function() {
  if (nrow(currentReduction) == 0) return()

  # Get current selections
  gene_col <- input$reduction_gene_col
  x_col <- input$reduction_x_axis
  y_col <- input$reduction_y_axis
  color_col <- input$reduction_color
  size_col <- input$reduction_size

  # Get all columns
  all_cols <- names(currentReduction)
  numeric_cols <- all_cols[sapply(currentReduction, is.numeric)]

  # Build list of selected columns (excluding empty strings)
  selected_cols <- c(gene_col, x_col, y_col, color_col, size_col)
  selected_cols <- selected_cols[selected_cols != "" & !is.null(selected_cols)]

  # Helper function: get available choices excluding other selections
  get_available <- function(base_choices, current_selection) {
    other_selections <- selected_cols[selected_cols != current_selection]
    setdiff(base_choices, other_selections)
  }

  # Update each dropdown with available choices
  updateSelectInput(session, "reduction_gene_col",
                    choices = c("", get_available(all_cols, gene_col)),
                    selected = gene_col)

  updateSelectInput(session, "reduction_x_axis",
                    choices = c("", get_available(numeric_cols, x_col)),
                    selected = x_col)

  updateSelectInput(session, "reduction_y_axis",
                    choices = c("", get_available(numeric_cols, y_col)),
                    selected = y_col)

  updateSelectInput(session, "reduction_color",
                    choices = c("", get_available(all_cols, color_col)),
                    selected = color_col)

  updateSelectInput(session, "reduction_size",
                    choices = c("", get_available(numeric_cols, size_col)),
                    selected = size_col)
}

# Handle 2D reduction plot generation
handleReductionGenerate <- function() {
  tryCatch({
    # Validate required selections
    gene_col <- input$reduction_gene_col
    x_col <- input$reduction_x_axis
    y_col <- input$reduction_y_axis

    if (is.null(gene_col) || gene_col == "") {
      renderWarning("Please select the Gene column.")
      return()
    }

    if (is.null(x_col) || x_col == "" || is.null(y_col) || y_col == "") {
      renderWarning("Please select both X axis and Y axis columns.")
      return()
    }

    renderModal("<h2>Please wait.</h2><br /><p>Generating 2D Reduction plot.</p>")

    # Calculate exclusions
    total_genes <- nrow(currentReduction)
    gene_col_data <- currentReduction[[gene_col]]
    x_col_data <- currentReduction[[x_col]]
    y_col_data <- currentReduction[[y_col]]

    # Count different types of missing data
    missing_gene_names <- is.na(gene_col_data) | gene_col_data == ""
    missing_coords <- is.na(x_col_data) | is.na(y_col_data)
    excluded <- missing_gene_names | missing_coords

    total_excluded <- sum(excluded)
    total_plotted <- total_genes - total_excluded

    # Build status message
    if (total_excluded > 0) {
      exclusion_parts <- c()
      if (sum(missing_gene_names) > 0) {
        exclusion_parts <- c(exclusion_parts,
                            paste0(sum(missing_gene_names), " missing gene names"))
      }
      if (sum(missing_coords) > 0) {
        exclusion_parts <- c(exclusion_parts,
                            paste0(sum(missing_coords), " missing coordinates"))
      }

      status_msg <- paste0(
        "Plotting ", total_plotted, " of ", total_genes, " genes (",
        total_excluded, " excluded: ", paste(exclusion_parts, collapse = ", "), ")"
      )
    } else {
      status_msg <- paste0("Plotting ", total_plotted, " genes")
    }

    renderShinyText("reductionPlotStatus", status_msg)

    # Render the plot
    renderReduction()

    removeModal()

  }, error = function(e) {
    cat(paste0("[Reduction] ", e, "\n"))
    renderError("2D Reduction plot generation error.")
    removeModal()
  })
}

# Handle clear button
handleReductionClear <- function() {
  tryCatch({
    updateSelectInput(session, "reduction_gene_col", selected = "")
    updateSelectInput(session, "reduction_x_axis", selected = "")
    updateSelectInput(session, "reduction_y_axis", selected = "")
    updateSelectInput(session, "reduction_color", selected = "")
    updateSelectInput(session, "reduction_size", selected = "")

    # Clear plot and status
    output$reductionPlot <- renderPlotly(c())
    renderShinyText("reductionPlotStatus", "")
    renderShinyText("reductionSelected", "")
  }, error = function(e) {
    cat(paste0("[Reduction] ", e, "\n"))
    renderError("2D Reduction clear error.")
  })
}

# Handle 2D reduction plot selection submission
handleReductionSubmit <- function() {
  tryCatch({
    if (existReductionSelectedItems()) {
      showModal(modalDialog(
        title = "2D Reduction selected set",
        paste0("Add the selected genes to the lists?"),
        footer = tagList(
          actionButton("reduction_ok", "OK"),
          modalButton("Cancel")
        )
      ))
    }
  }, error = function(e) {
    cat(paste0("[Reduction] ", e, "\n"))
    renderError("2D Reduction submit selected list error.")
  })
}

# Check if items selected
existReductionSelectedItems <- function() {
  if (length(reductionSelectedItems) == 0) {
    renderWarning("Please select points on the plot using box or lasso selection tool.")
    return(FALSE)
  }
  return(TRUE)
}

# Accept 2D reduction selection and create gene list
handleReductionListAccept <- function() {
  tryCatch({
    reductionDF <- as.data.frame(reductionSelectedItems)
    listName <- getRandomListName(REDUCTION_PREFIX)
    updateUserInputLists(reductionDF, listName)
    updateCheckboxInput(session, "selectAll", value = 0)
    updateListBoxes()
    removeModal()
  }, error = function(e) {
    cat(paste0("[Reduction] ", e, "\n"))
    renderError("Problem with 2D Reduction list selection.")
  })
}
