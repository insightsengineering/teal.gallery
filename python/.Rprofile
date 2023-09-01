source("renv/activate.R")

# Solution of using python packages in shinyapps.io deployment from: https://github.com/ranikay/shiny-reticulate-app
if (Sys.info()[["user"]] == "shiny") {
  # Running on shinyapps.io
  Sys.setenv(PYTHON_PATH = "python3")
  Sys.setenv(VIRTUALENV_NAME = "example_env_name") # Installs into default shiny virtualenvs dir
  Sys.setenv(RETICULATE_PYTHON = paste0("/home/shiny/.virtualenvs/", "example_env_name", "/bin/python"))
} else if (Sys.info()[["user"]] == "rstudio-connect") {
  # Running on remote server
  Sys.setenv(PYTHON_PATH = "/opt/python/3.7.7/bin/python3")
  Sys.setenv(VIRTUALENV_NAME = paste0("example_env_name", "/")) # include '/' => installs into rstudio-connect/apps/
  Sys.setenv(RETICULATE_PYTHON = paste0("example_env_name", "/bin/python"))
} else {
  # Running locally
  options(shiny.port = 7450)
  Sys.setenv(PYTHON_PATH = "python3")
}
