library(teal)

data <- teal_data()
data <- within(data, {
  library(dplyr)

  IRIS <- iris %>%
    mutate(
      Species1 = Species,
      Species2 = Species,
      Species3 = Species,
      Species4 = Species,
      Species5 = Species
    )
  MTCARS <- mtcars
})

filters <- teal_slices(
  teal_slice(dataname = "MTCARS", varname = "gear", multiple = FALSE),
  teal_slice(dataname = "IRIS", varname = "Species"),
  teal_slice(dataname = "IRIS", varname = "Sepal.Length"),
  teal_slice(
    dataname = "IRIS", varname = "Species3",
    fixed = TRUE, anchored = FALSE
  ),
  teal_slice(
    dataname = "IRIS", varname = "Species4",
    fixed = FALSE, anchored = TRUE, multiple = TRUE
  ),
  teal_slice(
    dataname = "IRIS", varname = "Species5",
    fixed = TRUE, anchored = TRUE
  ),
  teal_slice(
    dataname = "IRIS", id = "custom_expr", title = "Custom Expression",
    expr = "Sepal.Width > 2.5 & Petal.Length > 1.5",
  )
)

nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/basic-teal"
gh_issues_page <- "https://github.com/insightsengineering/teal.gallery/issues"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("My first teal app", style = "font-size: 30px;"),
  tags$span(
    style = "display: flex; align-items: center;",
    tags$img(src = nest_logo, alt = "NEST logo", height = "45px", style = "margin-right:10px;"),
    tags$span(style = "font-size: 24px;", "NEST @ Roche")
  )
)

footer <- tags$p(
  "This teal app is brought to you by the NEST Team at Roche/Genentech.
        For more information, please visit:",
  tags$a(href = app_source, target = "_blank", "Source Code"), ", ",
  tags$a(href = gh_issues_page, target = "_blank", "Report Issues")
)

app <- init(
  data = data,
  filter = filters,
  modules = modules(example_module()),
  title = build_app_title("Basic Teal Demo App", nest_logo),
  header = header,
  footer = footer
)

shinyApp(app$ui, app$server)
