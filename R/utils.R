#' List the `teal` apps in the gallery
#'
#' @return vector of app names in the gallery
#' @examples
#' if (interactive()) {
#'   list_apps()
#' }
#' @export
list_apps <- function() {
  dir(system.file("apps", package = utils::packageName()))
}


#' Get the source code of an app
#' @param app_name `character`: the name of the app. Run `list_apps()` to see the
#'   names of the apps in the gallery
#' @return (`character`) The code for the chosen app
#' @examples
#' if (interactive()) {
#'   get_app_code("basic-teal")
#'
#'   # code can be saved into a file
#'   code <- get_app_code("basic-teal")
#'   # writeLines(code, con = file("app.R"))
#' }
#' @export
get_app_code <- function(app_name) {
  checkmate::assert_string(app_name)
  app_file <- file.path(get_app_dir(app_name), "app.R")

  if (!file.exists(app_file)) {
    stop("app.R for application ", app_name, " does not exist in location ", app_file)
  }

  tryCatch(
    expr = {
      x <- paste(readLines(con = app_file), collapse = "\n")
      cat(paste("\n", x, "\n", sep = "\n"))
      invisible(x)
    },
    warning = function(cond) stop("Unable to read file ", app_file, " ", cond$message)
  )
}

#' Run an app from the gallery
#'
#' @details The package uses [renv::dependencies()] to check for
#'   dependencies the app requires and if they are not installed then
#'   the app will not load and an error will list the required packages to run the app
#' @param app_name `character`: the name of the app. Run `list_apps()` to see the
#'   names of the apps in the gallery
#' @param ... Additional arguments passed to [shiny::runApp()]
#' @examples
#' \dontrun{
#' launch_app("basic-teal")
#' }
#' @export
launch_app <- function(app_name, ...) {
  checkmate::assert_string(app_name)

  required_deps <- get_app_dependencies(app_name)

  for (dep in required_deps) {
    if (!requireNamespace(dep, quietly = TRUE)) {
      stop(
        "Unable to run app as package ",
        dep,
        " is not installed. Please ensure the following packages are installed: ",
        paste(required_deps, collapse = ", ")
      )
    }
  }

  # copy application to temporary directory
  tryCatch(
    expr = {
      temp_location <- tempfile()
      dir.create(temp_location)
      file.copy(from = get_app_dir(app_name), to = temp_location, recursive = TRUE)
    },
    error = function(cond) stop("Unable to copy app to temporary location: ", cond$message)
  )

  # run application
  shiny::runApp(file.path(temp_location, app_name), ...)
}

# get the directory the app is stored in
get_app_dir <- function(app_name) {
  app_dir <- system.file("apps", app_name, package = utils::packageName())
  if (app_dir == "") {
    stop("Cannot find app '", app_name, "'. Please choose from ", paste(list_apps(), collapse = ", "))
  }
  app_dir
}

# check the dependencies of the chosen app are available
get_app_dependencies <- function(app_name) {
  renv::dependencies(get_app_dir(app_name))$Package
}
