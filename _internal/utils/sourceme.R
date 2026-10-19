#' Download and run a Shiny app from the teal.gallery repository.
#'
#' This function downloads a Shiny app from the teal.gallery repository by calling
#' `download_apps_repo()` function and runs the specified app using `load_and_run_app()`.
#' This function is a wrapper around `shiny::runGitHub()`
#'
#' @param app_name The name of the app to run.
#' @param ref The reference (commit, branch, or tag) to download from the repository. (default: "HEAD")
#' @param ... Additional arguments passed to `shiny::runApp()`.
#'
#' @examples
#' restore_and_run("app1", ref = "HEAD")
restore_and_run <- function(app_name, ref = "HEAD", ...) {
  owner <- "insightsengineering"
  repo <- "teal.gallery"

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to run this function. Please install it first.")
  }
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop("The 'renv' package is required to run this function. Please install it first.")
  }

  #' Download a GitHub repository, unzip it, and return the repository directory.
  #'
  #' This function downloads a given GitHub repository as a tar.gz file, unzips it,
  #' and returns the directory where the repository is extracted.
  #'
  #' @param owner The GitHub username or organization name of the owner of the repository.
  #' @param repo The name of the GitHub repository.
  #' @param ref The reference (commit, branch, or tag) to download from the repository.
  #' @param file_dir The directory where the downloaded file will be saved.
  #' @param file_path The file path of the downloaded tar.gz file.
  #'
  #' @return The directory path of the extracted repository.
  download_apps_repo <- function(owner, repo, ref, file_dir, file_path) {
    url <- paste("https://github.com/", owner, "/", repo, "/archive/", ref, ".tar.gz", sep = "")
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
  load_and_run_app <- function(app_directory, ...) {
    new_libpath <- tempfile("local_libpaths-")
    on.exit(unlink(new_libpath, recursive = TRUE), add = TRUE)
    dir.create(new_libpath, showWarnings = FALSE, recursive = TRUE)
    new_libpath <- suppressWarnings(normalizePath(new_libpath))
    renv::install("callr", library = new_libpath) # run the app in a separate R process to avoid conflicts with the current R session.

    setwd(app_directory)
    dots <- rlang::dots_list(...)
    # Should not use "shiny.launch.browser" option as it may be caught by IDE (VS Code, Positron)
    # own shims that are not available in the R process launched by callr::r()
    dots$launch.browser <- dots$launch.browser %||% TRUE
    callr::r(
      function(...) {
        if (file.exists(".Rprofile")) {
          source(".Rprofile")
        } else if (requireNamespace("renv", quietly = TRUE)) {
          renv::load()
        } else {
          stop("Failed to find .Rprofile or renv package. Please create issue in the teal.gallery repository.")
        }
        renv::restore(prompt = FALSE)
        shiny::runApp(...)
      },
      args = c(app_directory, dots),
      show = TRUE,
      libpath = NULL,
      env = c(R_BROWSER = Sys.getenv("R_BROWSER"))
    )
  }

  file_path <- tempfile("shinyapp-", fileext = ".tar.gz")
  file_dir <- tempfile("shinyapp-")
  on.exit(unlink(file_path), add = TRUE)
  on.exit(unlink(file_dir, recursive = TRUE), add = TRUE)
  repo_directory <- download_apps_repo(owner, repo, ref, file_dir, file_path)
  available_apps <- get_available_apps(file.path(file_dir, repo_directory))
  if (!app_name %in% available_apps) {
    stop(
      paste0(
        "'", app_name, "' not available. Please choose from: ",
        paste(paste0("'", available_apps, "'"), collapse = ", ")
      )
    )
  }
  current_dir <- getwd()
  on.exit(setwd(current_dir), add = TRUE)
  load_and_run_app(file.path(file_dir, repo_directory, app_name), ...)
}
