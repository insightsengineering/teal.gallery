library(teal)

data <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    fluidPage(
      selectInput(
        ns("datasets"),
        "Choose datasets",
        choices = c("iris", "mtcars", "CO2", "airquality"),
        selected = c("iris", "mtcars"),
        multiple = TRUE
      ),
      actionButton(ns("load"), "Load Datasets")
    )
  },
  server = function(id, ...) {
    moduleServer(id, function(input, output, session) {
      eventReactive(input$load, {
        data <- teal.data::teal_data() |>
          within(
            {
              if ("iris" %in% datasets) {
                iris <- iris
              }
              if ("mtcars" %in% datasets) {
                mtcars <- mtcars
              }
              if ("CO2" %in% datasets) {
                CO2 <- CO2
              }
              if ("airquality" %in% datasets) {
                airquality <- airquality
              }
            },
            datasets = input$datasets
          )
        data
      })
    })
  },
  once = FALSE
)

nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/basic-teal"
gh_issues_page <- "https://github.com/insightsengineering/teal.gallery/issues"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("Teal app with delayed data loading", style = "font-size: 30px;"),
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
  modules = modules(
    example_module()
  )
) |>
  modify_title(
    title = "Safety Analysis Teal Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

shinyApp(app$ui, app$server)
