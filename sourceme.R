#' Deploy an app from the gallery to any Posit Connect server
#'
#' Not a lot of flexibility in configuring the app
#' as the options adhere to a certain set of requirements
#'
#' @param app_name (`character(1)`) name of the the teal app - technically, name of the app
#'  directory in the `inst/`. Do not confuse with the `name` argument in the [connectapi::deploy()] .
#' @param app_title (`character(1)`, optional) The title to be used for the content on the server.
#'  Unique app name deployed on the `server` is generated from `app_title` by removing
#' @param api_key (`character(1)`) API key/token.
#' @param vanity_url (`character(1)`) The path component of the URL where app will be available.
#' @param server (`character(1)`) The URL for accessing the Posit Connect server.
#' @param overwrite (`logical(1)`) whether remote app should be overwritten by the current deployment.
#' @param api_secret (`character(1)`) If deploying to a shinyapps.io account, the token's secret.
#' @param create_manifest (`logical(1)`) Whether to create the manifest.json for the app.
#' @examples
#' \dontrun{
#' teal.gallery::deploy_app(
#'   app_name = "basic-teal",
#'   app_title = "Basic Teal App - TEST",
#'   vanity_url = "/NEST/main/basic-teal",
#'   api_key = "SHINYAPPS_OR_POSIT_CONNECT_API_TOKEN",
#'   overwrite = FALSE
#' )
#' }
#' @export
deploy_app <- function(app_name,
                       app_title,
                       api_key,
                       vanity_url,
                       server,
                       overwrite = TRUE,
                       api_secret = "",
                       create_manifest = TRUE) {
  checkmate::assert_string(app_name)
  checkmate::assert_string(app_title, min.chars = 3)
  checkmate::assert_string(api_key)
  checkmate::assert_string(server)
  checkmate::assert_flag(overwrite)
  checkmate::assert(app_name %in% list_apps())

  if (!requireNamespace("rsconnect", quietly = TRUE) || !requireNamespace("connectapi", quietly = TRUE)) {
    stop("deploy_app requires packages 'rsconnect' and 'connectapi' to be installed")
  }

  app_dir <- get_app_dir(app_name)

  # Init client
  if (endsWith(server, "shinyapps.io")) {
    rsconnect::setAccountInfo(
      name = strsplit(gsub("https:\\/\\/", "", server), "\\.")[[1]][1],
      token = api_key,
      secret = api_secret,
      server = "shinyapps.io"
    )
  } else {
    client <- connectapi::connect(
      server = server,
      api_key = api_key
    )
  }

  if (create_manifest) {
    # Write deployment manifest
    rsconnect::writeManifest(
      appDir = app_dir,
      # Assuming all apps use apps.R instead of the
      # ui.R, server.R & global.R pattern
      appFiles = "app.R"
    )
  }

  # Deploy content
  # Full list of params:
  # https://github.com/rstudio/connectapi/blob/43329ed6ac409175063af72537ef65216b002fe1/R/ptype.R#L39-L71
  if (endsWith(server, "shinyapps.io")) {
    content <- rsconnect::deployApp(
      appDir = app_dir,
      appFiles = "app.R",
      appFileManifest = "manifest.json",
      appName = rsconnect::generateAppName(app_title),
      appTitle = app_title,
      account = strsplit(gsub("https:\\/\\/", "", server), "\\.")[[1]][1],
      upload = TRUE,
      logLevel = "normal",
      lint = FALSE,
      forceUpdate = overwrite
    )
  } else {
    content <- connectapi::deploy(
      client,
      connectapi::bundle_dir(app_dir),
      name = rsconnect::generateAppName(app_title),
      title = app_title,
      access_type = "all"
    )
    connectapi::set_image_url(
      content,
      "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/thumbs/teal.png"
    )
    connectapi::set_vanity_url(content, vanity_url, force = overwrite)
  }
}

#' Wrapper to deploy all teal sample apps at once
#'
#' Deploys all teal sample apps at once.
#'
#' @param api_key (`character(1)`) API key/token.
#' @param api_secret (`character(1)`) If deploying to a shinyapps.io account, the token's secret.
#' @param server (`character(1)`) The URL for accessing the Posit Connect or shinyapps.io server.
#' @param title_prefix (`character(1)`) Prefix to add to all app titles.
#' @param title_suffix (`character(1)`) Suffix to add to all app titles.
#' @param vanity_url_prefix (`character(1)`) Prefix to add to the apps' vanity URLs.
#' @param create_manifest (`logical(1)`) Whether to create the manifest.json for the app.
#' @examples
#' \dontrun{
#' teal.gallery::deploy_all_apps()
#' }
#' @export
deploy_all_apps <- function(api_key,
                            api_secret = "",
                            server,
                            title_prefix = "",
                            title_suffix = format(Sys.Date(), format = "%Y_%m_%d"),
                            vanity_url_prefix = "/NEST",
                            create_manifest = FALSE) {
  checkmate::assert_string(api_key)
  checkmate::assert_string(api_secret)
  checkmate::assert_string(server)
  checkmate::assert_string(title_prefix)
  checkmate::assert_string(title_suffix)
  checkmate::assert_string(vanity_url_prefix)
  apps <- list_apps()
  app_titles <- paste(title_prefix, apps, title_suffix)
  vanity_urls <- paste(vanity_url_prefix, title_suffix, apps, sep = "/")

  mapply(
    deploy_app,
    app_name = apps,
    app_title = app_titles,
    api_key = rep(api_key, length(apps)),
    vanity_url = vanity_urls,
    server = rep(server, length(apps)),
    api_secret = rep(api_secret, length(apps)),
    create_manifest = rep(create_manifest, length(apps))
  )
}

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
#' @param ... Additional arguments passed to `shiny::runApp()`.
#'
#' @examples
#' run_teal_gallery("app1", username = "insightsengineering", repo = "teal.gallery", ref = "HEAD")
run_teal_gallery <- function(app_name, username = "insightsengineering", repo = "teal.gallery", ref = "HEAD", ...) {
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
    ignore_dirs <- c(".github", "_quarto")
    dirs_present <- list.dirs(repo_root, full.names = FALSE, recursive = FALSE)
    setdiff(dirs_present, ignore_dirs)
  }

  #' Load and run a Shiny app from the specified directory.
  #'
  #' This function makes sure that the renv project is loaded and restored before running the shiny app.
  #'
  #' @param app_directory The directory path of the Shiny app.
  #' @param ... Additional arguments passed to `shiny::runApp()`.
  #'
  #' @examples
  #' load_and_run_app("~/repos/teal.gallery/app1/")
  load_and_run_app <- function(app_directory, ...) {
    renv::load(app_directory)
    renv::restore(clean = TRUE, prompt = FALSE)
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
  load_and_run_app(file.path(file_dir, repo_directory, app_name), ...)
}
