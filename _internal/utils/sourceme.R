#' Download and run a Shiny app from the teal.gallery repository.
#'
#' This function downloads a Shiny app from the teal.gallery repository by calling
#' `download_apps_repo()` function and runs the specified app using `load_and_run_app()`.
#' This function is a wrapper around `shiny::runGitHub()`
#'
#' @param app_name The name of the app to run.
#' @param username The GitHub username or organization name.
#' @param repo The name of the repository.
#' @param ref The reference (commit, branch, or tag) to download from the repository. (default: "HEAD")
#' @param package_repo The R-Universe mirror where the teal packages would be instealled from.
#' Main version: https://pharmaverse.r-universe.dev and Release version: https://insightsengineering.r-universe.dev
#' @param ... Additional arguments passed to `shiny::runApp()`.
#'
#' @examples
#' restore_and_run("app1", username = "insightsengineering", repo = "teal.gallery", ref = "HEAD")
restore_and_run <- function(
    app_name, username = "insightsengineering", repo = "teal.gallery", ref = "HEAD",
    package_repo = "https://pharmaverse.r-universe.dev", ...) {
  #' Download a GitHub repository, unzip it, and return the repository directory.
  #'
  #' This function downloads a given GitHub repository as a tar.gz file, unzips it,
  #' and returns the directory where the repository is extracted.
  #'
  #' @param username The GitHub username or organization name.
  #' @param repo The name of the GitHub repository.
  #' @param ref The reference (commit, branch, or tag) to download from the repository.
  #' @param file_dir The directory where the downloaded file will be saved.
  #' @param file_path The file path of the downloaded tar.gz file.
  #'
  #' @return The directory path of the extracted repository.
  download_apps_repo <- function(username, repo, ref, file_dir, file_path) {
    url <- paste("https://github.com/", username, "/", repo, "/archive/", ref, ".tar.gz", sep = "")
    message("Downloading ", url)
    dir.create(file_dir, showWarnings = FALSE)
    if (download.file(url, file_path, mode = "wb", quiet = TRUE) != 0) {
      stop("Failed to download URL ", url)
    }
    repo_directory <- shiny:::untar2(file_path, list = TRUE)[1]
    shiny:::untar2(file_path, exdir = file_dir)
    return(repo_directory)
  }

  #' Get the available app directories in the given repository root.
  #'
  #' This function retrieves the available app directories in the given repository root
  #' by listing all the directories and excluding non-app directories.
  #'
  #' @param repo_root The root directory of the repository.
  #'
  #' @return A character vector of available app directories.
  get_available_apps <- function(repo_root) {
    ignore_dirs <- c(".github", "_internal")
    dirs_present <- list.dirs(repo_root, full.names = FALSE, recursive = FALSE)
    setdiff(dirs_present, ignore_dirs)
  }

  #' Load and run a Shiny app from the specified directory.
  #'
  #' This function makes sure that the renv project is loaded and restored before running the shiny app.
  #'
  #' @param app_directory The directory path of the Shiny app.
  #' @param ... Additional arguments passed to `shiny::runApp()`.
  load_and_run_app <- function(app_directory, package_repo, ...) {
    install.packages(
      renv::dependencies(app_directory)$Package,
      repos = c(
        Pharmaverse = package_repo,
        CRAN = "https://cloud.r-project.org",
        BioC = BiocManager::repositories()
      )
    )
    shiny::runApp(app_directory, ...)
  }

  file_path <- tempfile("shinyapp", fileext = ".tar.gz")
  file_dir <- tempfile("shinyapp")
  on.exit(unlink(file_path))
  on.exit(unlink(file_dir, recursive = TRUE), add = TRUE)
  repo_directory <- download_apps_repo(username, repo, ref, file_dir, file_path)
  available_apps <- get_available_apps(file.path(file_dir, repo_directory))
  if (!app_name %in% available_apps) {
    stop(
      paste0(
        "'", app_name, "' not available. Please choose from: ",
        paste(paste0("'", available_apps, "'"), collapse = ", ")
      )
    )
  }
  load_and_run_app(file.path(file_dir, repo_directory, app_name), package_repo, ...)
}
