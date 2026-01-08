# =============================================================================
# FLAME SNPs Input Session
# =============================================================================
#
# Manages the SNPs tab for converting SNP IDs to gene lists using gprofiler2.
#
# Dependencies:
# - input-session-base.R (for InputSession)
# - input-analytelist.R (for AnalyteListRegistry)
# - utilities-gsnpense.R (for gsnpense_convert)
# - func-links.R (for attachVariantTableLinks)
#
# =============================================================================

# =============================================================================
# SNPS INPUT UI FUNCTION
# =============================================================================

#' Generate the SNPs input panel UI
#' @param id Module namespace ID (must match the ID used in SNPsInputSession$new)
#' @return A Shiny tabPanel
snpsInputUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "SNPs",
    icon = shiny::icon("dna"),
    shiny::tags$br(),
    shiny::tags$h5(shiny::HTML(
      "<b>Note:</b> this feature is currently available <u>only</u> for <i>Homo sapiens</i> (human) variants."
    )),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::tags$div(
          shiny::textAreaInput(
            inputId = ns("snp_textAreaList"),
            label = "Paste SNP list:",
            placeholder = "Write or paste your list here.\nClick the Example Button to load an example list.",
            resize = "vertical", height = "200px", width = "90%"
          ) %>%
            bsplus::shinyInput_label_embed(
              bsplus::shiny_iconlink("circle-info") %>%
                bsplus::bs_embed_popover(
                  title = "Input consists of dbSNP ids separated by spaces, new lines, tabs or commas."
                )
            ),
          shiny::actionButton(ns("snp_submit"), "Submit", shiny::icon("paper-plane")),
          shiny::actionButton(ns("snp_example"), "Example", shiny::icon("bookmark")),
          shiny::actionButton(ns("snp_clear"), "Clear", shiny::icon("broom"))
        ),
        shiny::tags$br(),
        shiny::tags$div(
          shiny::fileInput(ns("snp_fileUpload"), "or Upload from file(s):", multiple = TRUE,
                    accept = c(".tsv", ".csv", ".txt")) %>%
            bsplus::shinyInput_label_embed(
              bsplus::shiny_iconlink("circle-info") %>%
                bsplus::bs_embed_popover(
                  title = "Upload up to 10 files (up to 1MB each)."
                )
            )
        )
      ),
      shiny::column(
        9,
        shiny::tags$div(
          id = ns("snp_results"), style = "display:none",
          DT::dataTableOutput(ns("snpViewer")),
          shiny::tags$br(),
          shiny::radioButtons(
            inputId = ns("snp_choose_column"),
            label = "Select column:",
            choiceNames = c("Gene", "ENSEMBL ID"),
            choiceValues = c("gene_names", "ensgs"),
            selected = "gene_names",
            inline = TRUE
          ),
          shiny::actionButton(ns("snp_addList"), "Add to lists", shiny::icon("paper-plane")),
          shiny::actionButton(ns("snp_delete"), "Delete", shiny::icon("broom"))
        )
      )
    )
  )
}

# =============================================================================
# SNPS INPUT SESSION
# =============================================================================

#' SNPs Input Session Class
#'
#' Manages the SNPs tab for converting SNP IDs to gene lists using gprofiler2.
#'
#' Inherits from InputSession and uses the shared methods:
#' - addAnalyteList(): Add an analyte list to the registry
#' - generateUniqueName(): Generate unique list names
#' - cleanup(): Destroy observers
#'
#' @section UI Components:
#' - Input panel: text area, submit/example/clear buttons, file upload
#' - Results panel: data table with gene/ENSEMBL selection, add to lists button
#'
#' @section Dependencies:
#' - AnalyteListRegistry for storing analyte lists
#' - gprofiler2::gsnpense for SNP to gene conversion
#' - Uses AnalyteType$GENE for all lists created here
#'
#' @examples
#' \dontrun{
#' # In server.R
#' registry <- AnalyteListRegistry$new()
#' snpsInput <- SNPsInputSession$new(ModuleIds$INPUT_SNPS, registry)
#' snpsInput$server(input, session)
#' }
#'
SNPsInputSession <- R6::R6Class(
  "SNPsInputSession",

  inherit = InputSession,

  public = list(
    #' Initialize a SNPsInputSession
    #' @param id Character. Module namespace ID.
    #' @param registry AnalyteListRegistry. Registry for storing analyte lists.
    initialize = function(id, registry) {
      super$initialize(id, registry)
      private$.currentVariantResults <- data.frame()
    },

    #' Clean up observers and clear instance state
    cleanup = function() {
      private$.currentVariantResults <- data.frame()
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

        # ---------------------------------------------------------------------
        # OBSERVERS
        # ---------------------------------------------------------------------

        # Example button
        private$.observers$example <- shiny::observeEvent(input$snp_example, {
          private$loadVariantExample(session)
        }, ignoreInit = TRUE)

        # Clear button
        private$.observers$clear <- shiny::observeEvent(input$snp_clear, {
          private$resetVariantFields(session)
        }, ignoreInit = TRUE)

        # Submit button
        private$.observers$submit <- shiny::observeEvent(input$snp_submit, {
          private$handleVariantSubmit(input, output, session)
        }, ignoreInit = TRUE)

        # File upload
        private$.observers$fileUpload <- shiny::observeEvent(input$snp_fileUpload, {
          private$handleVariantUpload(input, output, session)
        }, ignoreInit = TRUE)

        # Add to lists button
        private$.observers$addList <- shiny::observeEvent(input$snp_addList, {
          private$addVariantsToFiles(input, session)
        }, ignoreInit = TRUE)

        # Delete button
        private$.observers$delete <- shiny::observeEvent(input$snp_delete, {
          private$deleteVariants(output, session)
        }, ignoreInit = TRUE)

      }) # end moduleServer
    }

    # cleanup() and getRegistry() are inherited from InputSession
  ),

  private = list(
    # .registry, .parentSession, .moduleSession, .observers are inherited from InputSession

    # Current variant results from API
    .currentVariantResults = NULL,

    # =========================================================================
    # HANDLER METHODS
    # =========================================================================

    loadVariantExample = function(session) {
      tryCatch({
        examplePath <- tryCatch(
          getExamplesPath("variants.RDS"),
          error = function(e) "examples/variants.RDS"
        )
        snp_example <- paste(unlist(readRDS(examplePath)), collapse = "\n")
        shiny::updateTextAreaInput(session, "snp_textAreaList", value = snp_example)
      }, error = function(e) {
        cat(paste("[SNPs] Example error:", conditionMessage(e), "\n"))
        renderError("SNP example error.")
      })
    },

    resetVariantFields = function(session) {
      tryCatch({
        shiny::updateTextAreaInput(session, "snp_textAreaList", value = "")
      }, error = function(e) {
        cat(paste("[SNPs] Clear error:", conditionMessage(e), "\n"))
        renderError("SNP clear error.")
      })
    },

    handleVariantSubmit = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
        private$convertVariantsToGenes(input$snp_textAreaList, output, session)
        shinyjs::show("snp_results")
      }, error = function(e) {
        cat(paste("[SNPs] Text submit error:", conditionMessage(e), "\n"))
        renderError("Text submit error.")
      }, finally = {
        removeModal()
      })
    },

    handleVariantUpload = function(input, output, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
        variantInput <- private$readVariantUpload(input)
        private$convertVariantsToGenes(variantInput, output, session)
        shinyjs::show("snp_results")  # shinyjs handles namespace automatically
      }, error = function(e) {
        cat(paste("[SNPs] File upload error:", conditionMessage(e), "\n"))
        renderError("File upload error.")
      }, finally = {
        removeModal()
      })
    },

    readVariantUpload = function(input) {
      variantFile <- input$snp_fileUpload
      fileType <- tolower(substr(variantFile$name, nchar(variantFile$name) - 2,
                                 nchar(variantFile$name)))
      if (fileType == "tsv" || fileType == "txt") {
        variantInput <- read.delim(variantFile$datapath, header = FALSE)
      } else if (fileType == "csv") {
        variantInput <- read.csv(variantFile$datapath, header = FALSE)
      } else {
        stop("Unsupported file type")
      }
      return(variantInput)
    },

    convertVariantsToGenes = function(variantList, output, session) {
      if (private$isInputNotEmpty(variantList)) {
        # Handle both text input (character string) and file upload (data frame)
        if (is.character(variantList) && length(variantList) == 1) {
          # Text area input: split on common delimiters
          variants <- strsplit(variantList, "[\n\r\t, ]+")[[1]]
          variants <- variants[variants != ""]
        } else {
          # Data frame or vector input: flatten to character vector
          variants <- as.character(unlist(variantList))
          variants <- variants[variants != "" & !is.na(variants)]
        }

        if (length(variants) == 0) {
          renderWarning("No valid SNP IDs found in input.")
          return(invisible(NULL))
        }

        # Use gsnpense service function
        result <- gsnpense_convert(variants)
        if (is.null(result)) {
          renderWarning("No gene mappings found for the provided SNP IDs.")
          return(invisible(NULL))
        }
        result$index <- seq_len(nrow(result))
        variant_cols <- colnames(result)[grep("variants.", colnames(result))]

        # Fix: Use tidyselect::all_of() instead of : operator on character vectors
        names <- result %>%
          tidyr::gather(key = "colname", value = "value",
                 tidyselect::all_of(variant_cols)) %>%
          dplyr::filter(value != 0) %>%
          dplyr::mutate(value = paste(colname, sep = ", ")) %>%
          dplyr::group_by(index) %>%
          dplyr::summarise(value = paste(value, collapse = ", "))

        names$effect <- lapply(names$value, function(i) {
          tp <- gsub("_", " ",
                     gsub("variants.", "",
                          gsub("_variant", "", i)
                     )
          )
          tp_str <- paste(tp, collapse = ", ")
          return(tp_str)
        })

        result <- result %>%
          dplyr::left_join(names, by = "index")

        result_formatted <- subset(result,
                                   select = c(rs_id, gene_names, ensgs, chromosome,
                                              start, end, strand, effect))
        result_formatted <- attachVariantTableLinks(result_formatted)
        names(result_formatted) <- c("SNP", "Gene", "ENSEMBL ID", "Chromosome",
                                     "Start", "End", "Strand", "Effect Type")
        output$snpViewer <- DT::renderDataTable(
          result_formatted,
          server = FALSE,
          selection = 'none',
          extensions = 'Buttons',
          options = list(
            scrollY = "200px",
            scrollX = TRUE,
            scroller = TRUE,
            "dom" = 'Blfiprt',
            buttons = createExportButtons("snp", c())
          ),
          rownames = FALSE,
          escape = FALSE
        )
        private$.currentVariantResults <- result
      }
    },

    isInputNotEmpty = function(inputVal) {
      isNotEmpty <- TRUE

      # Handle NULL or empty inputs
      if (is.null(inputVal) || length(inputVal) == 0) {
        isNotEmpty <- FALSE
      } else if (is.data.frame(inputVal)) {
        # Data frame from file upload
        isNotEmpty <- nrow(inputVal) > 0
      } else if (is.character(inputVal)) {
        # Text area input - check if empty or whitespace only
        isNotEmpty <- nchar(trimws(paste(inputVal, collapse = ""))) > 0
      } else {
        # Other types - check if not empty
        isNotEmpty <- length(unlist(inputVal)) > 0
      }

      if (!isNotEmpty) {
        renderWarning("Please, paste your input list in the text area first.")
      }

      return(isNotEmpty)
    },

    addVariantsToFiles = function(input, session) {
      tryCatch({
        renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
        choice <- input$snp_choose_column
        if (choice == "ensgs") {
          list_of_ids <- unlist(private$.currentVariantResults$ensgs)
          nametype <- "variants_ensg"
        } else {
          list_of_ids <- unlist(private$.currentVariantResults$gene_names)
          nametype <- "variants_geneName"
        }

        # Use inherited method from InputSession
        listName <- self$generateUniqueName(nametype)
        ids <- as.character(list_of_ids)
        # Filter out NA and empty strings
        ids <- ids[!is.na(ids) & ids != ""]
        if (length(ids) > 0) {
          self$addAnalyteList(listName, ids)
        } else {
          renderWarning("No valid gene IDs found in results.")
        }
      }, error = function(e) {
        cat(paste("[SNPs] Add to list error:", conditionMessage(e), "\n"))
        renderWarning(conditionMessage(e))
      }, finally = {
        removeModal()
      })
    },

    deleteVariants = function(output, session) {
      tryCatch({
        output$snpViewer <- DT::renderDataTable(NULL)
        private$.currentVariantResults <- data.frame()
        shinyjs::hide("snp_results")  # shinyjs handles namespace automatically
      }, error = function(e) {
        cat(paste("[SNPs] Delete error:", conditionMessage(e), "\n"))
        renderError("SNP reset error.")
      })
    }
  )
)
