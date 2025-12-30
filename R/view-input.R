generateInputPage <- function() {
  tags$div(
    tags$h3("Input Lists"),
    tags$br(),
    fluidRow(
      class = "inputControlRow",
      generateCurrentListsColumn(),
      column(
        8,
        tabsetPanel(
          generateUploadPanel(),
          generateVariantPanel(),
          generateTextMiningPanel(),
          generateVolcanoPanel(),
          generateReductionPanel()
        )
      )
    ),
    tags$hr(),
    tags$h3("View and Filter Options"),
    tabsetPanel(
      id = "inputPlots",
      generateViewPanel(),
      generateUpsetPlotPanel(),
      generateVolcanoPlotPanel(),
      generateReductionPlotPanel()
    )
  )
}

generateCurrentListsColumn <- function() {
  column(
    4,
    tags$div(
      class = "checkListDiv",
      verbatimTextOutput("url_checked"),
      checkboxGroupInput(
        inputId = "checkboxLists",
        label = "Current lists:"
      ),
      checkboxInput("selectAll", "Select/Deselect All"),
      actionButton("rename", "Rename", icon("pencil")),
      actionButton("remove", "Remove", icon("trash"))
    )
  )
}

generateUploadPanel <- function() {
  tabPanel(
    title = "Upload", 
    icon = icon("upload"),
    tags$br(),
    tags$div(
      textAreaInput(
        inputId = "textAreaList",
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
      actionButton("text_submit", "Add to lists", icon("paper-plane")),
      actionButton("example", "Example", icon("bookmark")),
      actionButton("input_clear", "Clear", icon("broom"))
    ),
    tags$br(),
    tags$div(
      fileInput("fileUpload", "or Upload from file(s):", multiple = T,
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

generateTextMiningPanel <- function() {
  tabPanel(
    title = "Text-mining",
    icon = icon("envelope-open-text"),
    tags$br(),
    tags$div(
      fluidRow(
        column(
          5,
          tags$div(
            textAreaInput(
              inputId = "textmining_textinput",
              label = "1. Input text (Max: 100,000 words or 500,000 characters):",
              placeholder = "Write or paste a text here.\n\nClick the 'Load Example' button to load an example text.",
              resize = "vertical", height = "200px", width = "90%"),
            selectizeInput(
              inputId = "textmining_organism",
              label = "2. Select organism:",
              choices = NULL,
              selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
              multiple = F,
              width = "90%",
              options = list(placeholder = "Select an option or start typing...")
            ),
            actionButton("textmining_submit", "Submit", icon("paper-plane")),
            actionButton("textmining_addExample", "Example", icon("bookmark")),
            actionButton("textmining_clear", "Clear", icon("broom"))
          )
        ),
        column(
          7,
          tags$div(
            id = "textmining_tagger_results",
            style = "display:none",
            tabsetPanel(
              tabPanel(
                "Identified Genes",
                dataTableOutput(outputId = "extracted_terms")
              ),
              tabPanel(
                "Annotated Text",
                wellPanel(htmlOutput(outputId = "extracted_text"))
              )
            ),
            actionButton("textmining_selectAll", "Select All", icon("check")),
            actionButton("textmining_selectNone", "De-select All", icon("x")),
            tags$br(),
            actionButton("textmining_addList", "Add selected to lists", icon("paper-plane"), onclick='textmining_getAllSelected()'),
            actionButton("textmining_delete", "Delete", icon("broom"))
          )
        )
      )
    )
  )
}

generateVolcanoPanel <- function() {
  tabPanel(
    title = "Volcano",
    icon = icon("volcano"),
    tags$br(),
    fluidRow(
      column(
        4,
        fileInput("volcanoUpload", "Upload gene fold change file:", multiple = FALSE,
                  accept = c(".tsv", ".csv", ".txt")) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Upload file with at least: 1 column for gene identifiers + 2 numeric columns (logFC, p-value). You'll select which columns to use after upload."
              )
          ),
        actionButton("volcano_addExample", "Example", icon("bookmark")),
      ),
      column(
        8,
        dataTableOutput(outputId = "volcanoViewer")
      )
    )
  )
}

generateReductionPanel <- function() {
  tabPanel(
    title = "2D Reduction",
    icon = icon("braille"),
    tags$br(),
    fluidRow(
      column(
        4,
        fileInput("reductionUpload", "Upload 2D reduction data:", multiple = FALSE,
                  accept = c(".tsv", ".csv", ".txt")) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Upload file with at least: 1 column for gene identifiers + 2 numeric columns for coordinates (e.g., PC1, PC2). Additional columns for color/size (optional)."
              )
          ),
        actionButton("reduction_addExample", "Example", icon("bookmark")),
      ),
      column(
        8,
        dataTableOutput(outputId = "reductionViewer")
      )
    )
  )
}

generateVariantPanel <- function() {
  tabPanel(
    title = "SNPs",
    icon = icon("dna"),
    tags$br(),
    tags$h5(HTML("<b>Note:</b> this feature is currently available <u>only</u> for <i>Homo sapiens</i> (human) variants.")),
    fluidRow(
      column(
        3,
        tags$div(
          textAreaInput(
            inputId = "snp_textAreaList",
            label = "Paste SNP list:",
            placeholder = "Write or paste your list here.\nClick the Example Button to load an example list.",
            resize = "vertical", height = "200px", width = "90%") %>%
            bsplus::shinyInput_label_embed(
              bsplus::shiny_iconlink("circle-info") %>%
                bsplus::bs_embed_popover(
                  title = "Input consists of dbSNP ids separated by spaces, new lines, tabs or commas."
                )
            ),
          actionButton("snp_submit", "Submit", icon("paper-plane")),
          actionButton("snp_example", "Example", icon("bookmark")),
          actionButton("snp_clear", "Clear", icon("broom"))
        ),
        tags$br(),
        tags$div(
          fileInput("snp_fileUpload", "or Upload from file(s):", multiple = T,
                    accept = c(".tsv", ".csv", ".txt")) %>%
            bsplus::shinyInput_label_embed(
              bsplus::shiny_iconlink("circle-info") %>%
                bsplus::bs_embed_popover(
                  title = "Upload up to 10 files (up to 1MB each)."
                )
            )
        )
      ),
      column(
        9,
        tags$div(
          id = "snp_results", style = "display:none",
          dataTableOutput(outputId = "snpViewer"),
          tags$br(),
          radioButtons(
            inputId = "snp_choose_column", 
            label = "Select column:", 
            choiceNames = c("Gene", "ENSEMBL ID"), 
            choiceValues = c("gene_names", "ensgs"), 
            selected = "gene_names",
            inline = T
          ),
          actionButton("snp_addList", "Add to lists", icon("paper-plane")),
          actionButton("snp_delete", "Delete", icon("broom"))
        )
      )
    )
  )
}

generateViewPanel <- function() {
  tabPanel(
    title = "View",
    icon = icon("eye"),
    tags$br(),
    selectInput(
      inputId = "selectView",
      label = "Select list to view:",
      choices = NULL,
      width = "100%"
    ),
    DT::dataTableOutput("selectedListView")
  )
}

generateUpsetPlotPanel <- function() {
  tabPanel(
    title = "UpSet Plot",
    icon = icon("chart-column"),
    tags$div(
      tags$br(),
      radioButtons(
        inputId = "upsetMode",
        label = "Select mode:",
        choices = c("Intersection", "Distinct Combinations", "Union"),
        inline = TRUE
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink("circle-info") %>%
            bsplus::bs_embed_popover(
              title = "Select at least 2 files to create the Upset plot from the checkbox file list.
                  \nThe UpSet plot Intersection option visualizes the total number of common elements amongthe selected sets, even though they may also participate in other sets.
                  \nThe Distinct Combinations option visualizes the common number of genes, among chosen sets, that do not exist in any other set. This option is the closest to a Venn diagram.
                  \nThe Union option appends the unique elements among chosen sets and creates all possible combinations."
            )
        ),
      actionButton(inputId = "submitUpset", label = "Generate",
                   icon = icon("palette"), class = "submit_button"),
      upsetjs::upsetjsOutput("upsetjsView"),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$div(
          class = "upsetMargin",
          column(2, textOutput("hoveredInfoLabel")),
          column(2, textOutput("hoveredListName")),
          column(8, textOutput("hoveredElements"))
        )
      )
    )
  )
}

generateVolcanoPlotPanel <- function() {
  tabPanel(
    title = "Volcano Plot",
    icon = icon("volcano"),
    tags$br(),
    fluidRow(
      column(
        9,
        textOutput("volcanoSelectionInfo"),
        plotlyOutput("volcanoPlot", height = "750px")
      ),
      column(
        3,
        tags$div(
          id = "volcanoPanel",
          tags$h5("Plot Controls", class = "section-header"),
          selectInput(
            inputId = "volcano_gene_col",
            label = "Gene column *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          selectInput(
            inputId = "volcano_logfc_col",
            label = "LogFC column *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          selectInput(
            inputId = "volcano_pvalue_col",
            label = "P-value column *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          sliderInput(
            inputId = "volcano_pvalue_slider",
            label = "Choose -log10Pvalue threshold:",
            min = 0, max = 5,
            value = DEFAULT_VOLCANO_LOG10PVALUE_THRESHOLD,
            step = DEFAULT_VOLCANO_LOG10PVALUE_STEP,
            width = "100%"
          ) %>%
            bsplus::shinyInput_label_embed(
              bsplus::shiny_iconlink("circle-info") %>%
                bsplus::bs_embed_popover(
                  title = "0.05 pvalue == 1.30103 -log10pvalue\n0.01 pvalue == 2 -log10pvalue"
                )
            ),
          sliderInput(
            inputId = "volcano_fc_slider",
            label = "Choose |log2FC| threshold:",
            min = 0, max = 5,
            value = DEFAULT_VOLCANO_LOG2FC_THRESHOLD,
            step = DEFAULT_VOLCANO_LOG2FC_STEP,
            width = "100%"
          ),
          tags$br(),
          actionButton("volcanoGenerate", "Generate Plot",
                       icon("palette"), class = "submit_button"),
          actionButton("volcanoClear", "Clear",
                       icon("broom"), class = "submit_button"),
          actionButton("volcano_submit", "Add to lists", icon("paper-plane")),
          tags$br(),
          tags$br(),
          tags$h5("Plot Information", class = "section-header"),
          tags$div(
            class = "plot-info-content",
            textOutput("volcanoPlotStatus"),
            textOutput("volcanoMetricConversions")
          ),
          tags$h5("Selected Genes", class = "section-header"),
          verbatimTextOutput("volcanoSelected")
        )
      )
    )
  )
}

generateReductionPlotPanel <- function() {
  tabPanel(
    title = "2D Reduction Plot",
    icon = icon("braille"),
    tags$br(),
    fluidRow(
      column(
        9,
        textOutput("reductionSelectionInfo"),
        plotlyOutput("reductionPlot", height = "750px")
      ),
      column(
        3,
        tags$div(
          id = "reductionPanel",
          tags$h5("Plot Controls", class = "section-header"),
          selectInput(
            inputId = "reduction_gene_col",
            label = "Gene column *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          selectInput(
            inputId = "reduction_x_axis",
            label = "X axis *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          selectInput(
            inputId = "reduction_y_axis",
            label = "Y axis *:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          selectInput(
            inputId = "reduction_color",
            label = "Color by:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          selectInput(
            inputId = "reduction_size",
            label = "Size by:",
            choices = c(""),
            selected = "",
            width = "100%"
          ),
          tags$br(),
          actionButton("reductionGenerate", "Generate Plot",
                       icon("palette"), class = "submit_button"),
          actionButton("reductionClear", "Clear",
                       icon("broom"), class = "submit_button"),
          actionButton("reduction_submit", "Add to lists", icon("paper-plane")),
          tags$br(),
          tags$br(),
          tags$h5("Plot Information", class = "section-header"),
          tags$div(
            class = "plot-info-content",
            textOutput("reductionPlotStatus")
          ),
          tags$h5("Selected Genes", class = "section-header"),
          verbatimTextOutput("reductionSelected")
        )
      )
    )
  )
}
