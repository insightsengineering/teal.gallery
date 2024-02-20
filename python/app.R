library(reticulate)
library(teal)
library(teal.modules.general)
library(nestcolor)
# optional libraries
library(colourpicker)
library(ggExtra)
library(ggpmisc)
library(sparkline)

options(shiny.useragg = FALSE)

nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"

data <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    actionButton(ns("submit"), label = "Load data")
  },
  server = function(id) {
    moduleServer(id, function(input, output, session, python_code) {
      eventReactive(input$submit, {
        # Prepare the Python environment by installing virtualenvironment
        # Tags lines of code with comment "# @linksto py_dict" to hint the code
        # parser on eval_code as these lines are necessary
        data <- teal.code::eval_code(
          teal_data(),
          "
            library(reticulate)
            python_dependencies <- c(\"pip\", \"numpy\", \"pandas\") # @linksto py_dict
            virtualenv_dir <- Sys.getenv(\"VIRTUALENV_NAME\", \"example_env_name\") # @linksto py_dict
            python_path <- Sys.getenv(\"PYTHON_PATH\") # @linksto py_dict
            if (python_path == \"\") python_path <- NULL
            reticulate::virtualenv_create(
              envname = virtualenv_dir, python = python_path
            ) # @linksto py_dict
            reticulate::virtualenv_install(
              virtualenv_dir,
              packages = python_dependencies,
              ignore_installed = TRUE
            ) # @linksto py_dict
            reticulate::use_virtualenv(virtualenv_dir, required = TRUE) # @linksto py_dict
            iris_raw <- cbind(id = seq_len(nrow(iris)), iris) # @linksto py_dict
          "
        )

        # Run python code to generate the IRIS dataset
        data <- within(
          data,
          {
            # python code needs to be un-indented
            python_code <- "
import pandas as pd
data = r.iris_raw
def svd_whiten(dat):
  import numpy as np
  X = np.matrix(dat)
  U, s, Vt = np.linalg.svd(X, full_matrices=False)
  X_white = np.dot(U, Vt)
  return X_white

data_columns = data.columns
global numeric_cols_ix
global numeric_cols
numeric_cols_ix = list(range(5))[1:]
numeric_cols = [x for i,x in enumerate(data_columns) if i in numeric_cols_ix]
svd_res = svd_whiten(data.iloc[:, numeric_cols_ix])
data_new = pd.concat([data, pd.DataFrame(svd_res)], axis = 1)
data_new.columns = list(data_columns) + [i + '.whiten' for i in numeric_cols]
data_new = data_new.round(10)
data_new
            "
            withr::with_options(
              list(reticulate.engine.environment = environment()),
              py_dict <- py_run_string(python_code)
            )
            IRIS <- py_dict$data_new
          }
        )
        datanames(data) <- c("IRIS")
        data
      })
    })
  }
)

app <- teal::init(
  data = data,
  title = build_app_title("Python Dataset Teal Demo App", nest_logo),
  modules = modules(
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_g_scatterplot(
      "Scatterplot",
      x = data_extract_spec(
        dataname = "IRIS",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices("IRIS", c(
            "Sepal.Length", "Sepal.Width",
            "Petal.Length", "Petal.Width",
            "Sepal.Length.whiten", "Sepal.Width.whiten",
            "Petal.Length.whiten", "Petal.Width.whiten"
          )),
          selected = "Petal.Length.whiten",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y = data_extract_spec(
        dataname = "IRIS",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices("IRIS", c(
            "Sepal.Length", "Sepal.Width",
            "Petal.Length", "Petal.Width",
            "Sepal.Length.whiten", "Sepal.Width.whiten",
            "Petal.Length.whiten", "Petal.Width.whiten"
          )),
          selected = "Petal.Width.whiten",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color_by = data_extract_spec(
        dataname = "IRIS",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices("IRIS", c("Species")),
          selected = "Species",
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  ),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
    tags$span(
      style = "font-size: 30px;",
      "Example teal app using python dataset connector"
    ),
    tags$span(
      style = "display: flex; align-items: center;",
      tags$img(src = nest_logo, alt = "NEST logo", height = "45px", style = "margin-right:10px;"),
      tags$span(style = "font-size: 24px;", "NEST @ Roche")
    )
  ),
  footer = tags$p(
    actionLink("showAboutModal", "About,"),
    tags$a(
      href = "https://github.com/insightsengineering/teal.gallery/tree/main/python",
      target = "_blank",
      "Source Code,"
    ),
    tags$a(
      href = "https://github.com/insightsengineering/teal.gallery/issues",
      target = "_blank",
      "Report Issues"
    )
  )
)

body(app$server)[[length(body(app$server)) + 1]] <- quote(
  observeEvent(input$showAboutModal, {
    showModal(modalDialog(
      tags$p(
        "This teal app is brought to you by the NEST Team at Roche/Genentech.
        For more information, please visit:"
      ),
      tags$ul(
        tags$li(tags$a(
          href = "https://github.com/insightsengineering", "Insights Engineering",
          target = "blank"
        )),
        tags$li(tags$a(
          href = "https://pharmaverse.org", "Pharmaverse",
          target = "blank"
        ))
      ),
      easyClose = TRUE
    ))
  })
)

shinyApp(app$ui, app$server)
