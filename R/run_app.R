#' Run the FLAME Shiny Application
#'
#' Launches the FLAME (Functional Analysis of Multi-omics
#' Enrichment) Shiny application.
#'
#' @param launch.browser Logical. If TRUE, opens the app in a browser window.
#'   Default is TRUE.
#' @param port The port number to run the app on. Default is NULL (auto-select).
#'
#' @return Invisible NULL. Runs the Shiny application.
#'
#' @examples
#' \dontrun{
#' run_flame()
#' }
#'
#' @export
run_flame <- function(launch.browser = TRUE, port = NULL) {
  # Find app directory (works both installed and during development)
  appDir <- system.file("app", package = "flame")

  if (appDir == "" || !file.exists(file.path(appDir, "server.R"))) {
    # Not installed or inst/app/ doesn't have the app files
    # Try to find the package source root (development mode)
    packageRoot <- find.package("flame", quiet = TRUE)

    if (length(packageRoot) > 0) {
      # Check if this is a source package with original files at root
      if (file.exists(file.path(packageRoot, "server.R"))) {
        appDir <- packageRoot
        message("Running FLAME in development mode from: ", appDir)
      } else if (file.exists(file.path(packageRoot, "inst", "app", "server.R"))) {
        # Development mode but app is in inst/app/
        appDir <- file.path(packageRoot, "inst", "app")
        message("Running FLAME from inst/app/: ", appDir)
      } else {
        stop("Could not find FLAME app files (server.R). ",
             "Make sure you're in the package directory or the package is installed.",
             call. = FALSE)
      }
    } else {
      # Try current working directory as last resort
      if (file.exists(file.path(getwd(), "server.R"))) {
        appDir <- getwd()
        message("Running FLAME from current directory: ", appDir)
      } else {
        stop("Could not find FLAME app directory. ",
             "Make sure the package is installed or use devtools::load_all()",
             call. = FALSE)
      }
    }
  }

  # Run the Shiny app
  shiny::runApp(
    appDir = appDir,
    launch.browser = launch.browser,
    port = port
  )
}
