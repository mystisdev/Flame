# Module IDs are now defined in infrastructure-config.R (ModuleIds)
# This ensures a single source of truth shared by both UI and server.

generateInputPage <- function() {
  tags$div(
    tags$h3("Input Lists"),
    tags$br(),
    fluidRow(
      class = "inputControlRow",
      analyteListManagerSidebarUI(ModuleIds$LISTMGMT_MANAGER),
      column(
        8,
        tabsetPanel(
          listInputUploadUI(ModuleIds$INPUT_LIST),
          snpsInputUI(ModuleIds$INPUT_SNPS),
          textMiningInputUI(ModuleIds$INPUT_TEXTMINING),
          volcanoInputUI(ModuleIds$INPUT_VOLCANO),
          reductionInputUI(ModuleIds$INPUT_REDUCTION)
        )
      )
    ),
    tags$hr(),
    tags$h3("View and Filter Options"),
    tabsetPanel(
      id = "inputPlots",
      analyteListManagerViewUI(ModuleIds$LISTMGMT_MANAGER),
      upsetPlotUI(ModuleIds$LISTMGMT_SETOPS)
      # Volcano and Reduction plot tabs are inserted dynamically by their session classes
      # when data is loaded (using insertTab), not pre-created here
    )
  )
}

# NOTE: UI components are defined in session class files:
#
# List Management (R/listmgmt-session-*.R):
# - AnalyteListManagerSession: sidebar (analyteListManagerSidebarUI) and view panel (analyteListManagerViewUI)
# - AnalyteListSetOperationsSession: UpSet plot panel (upsetPlotUI)
#
# Input Sessions (R/input-session-*.R):
# - ListInputSession: upload panel (listInputUploadUI)
# - SNPsInputSession: SNPs tab (snpsInputUI)
# - TextMiningInputSession: text mining tab (textMiningInputUI)
# - VolcanoInputSession: volcano tab (volcanoInputUI) and plot panel (volcanoPlotUI)
# - ReductionInputSession: reduction tab (reductionInputUI) and plot panel (reductionPlotUI)
