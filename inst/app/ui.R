# FLAME UI Definition

# Determine package root and source required files
pkgRoot <- normalizePath(file.path(getwd(), "..", ".."))
source(file.path(pkgRoot, "R", "config-b-global_variables.R"), local = TRUE)
source(file.path(pkgRoot, "R", "config-f-ui_variables.R"), local = TRUE)
source(file.path(pkgRoot, "R", "func-init.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-welcome.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-input.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-enrichment.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-plots.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-network.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-conversion.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-help.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-about.R"), local = TRUE)
source(file.path(pkgRoot, "R", "view-footer.R"), local = TRUE)

dashboardPage(
  title = "Flame",
  skin = "yellow",
  dashboardHeader(
    titleWidth = "356px",
    title = tags$a(
      href = './',
      tags$img(src = 'logo.png')
    )
  ),
  dashboardSidebar(
    width = "356px",
    sidebarMenu(
      id = "sideBarId",
      menuItem("Welcome", tabName = "welcome", icon = icon("house")),
      tags$hr(),
      menuItem("Input Lists", tabName = "file_handler", icon = icon("file-contract")),
      tags$hr(),
      menuItem("Functional Enrichment", tabName = "functional_enrichment", icon = icon("gears")),
      tags$hr(),
      menuItem("Literature Enrichment", tabName = "literature_enrichment", icon = icon("book")),
      tags$hr(),
      menuItem("Network Analysis", tabName = "string_network", icon=icon("diagram-project")),
      tags$hr(),
      menuItem("Conversion", tabName = "Conversion", icon = icon("right-left"),
               menuSubItem("Gene ID Conversion", tabName = "gconvert", icon = icon("angles-right")),
               menuSubItem("Orthology Search", tabName = "gorth", icon = icon("angles-right"))),
      tags$hr(),
      menuItem("Help", tabName = "help", icon = icon("question")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/Flame.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/extract_popups.css")),
    tags$head(tags$script(src = "js/rshiny_handlers.js")),
    tags$head(tags$script(src = "js/update_rshiny_values.js")),
    tags$head(tags$script(src = "js/extract_popups.js")),
    tags$head(tags$script(src = 'https://string-db.org/javascript/combined_embedded_network_v2.0.2.js')),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem("welcome", welcomePage),
      tabItem("file_handler", generateInputPage()),
      tabItem("functional_enrichment", generateEnrichmentPage("functional")),
      tabItem("literature_enrichment", generateEnrichmentPage("literature")),
      tabItem("string_network", generateStringNetworkPage()),
      tabItem("gconvert", generateConvertDiv("gconvert")),
      tabItem("gorth", generateConvertDiv("gorth")),
      tabItem("help", generateHelpPage()),
      tabItem("about", aboutPage)
    ),
    footer
  )
)
