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

reticulate::use_virtualenv("python3_env", required = TRUE)
iris_raw <- cbind(id = 1:nrow(iris), iris)

python_code <- "import pandas as pd
data = r.data_raw
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
data_new
"

app <- teal::init(
  data = teal_data(python_dataset_connector(
    "IRIS",
    code = python_code,
    object = "data_new",
    keys = "id",
    vars = list(data_raw = iris_raw)
  )) %>% mutate_data("IRIS$id <- as.integer(IRIS$id)"),
  title = "Example teal app using python connector",
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
  header = "Example teal app using python connector",
  footer = ""
)

shinyApp(app$ui, app$server)
