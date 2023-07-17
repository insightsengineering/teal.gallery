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
