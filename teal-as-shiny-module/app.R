library(teal)

data_1 <- teal_data()
data_1 <- within(data_1, {
  IRIS <- head(iris)
  MTCARS <- head(mtcars)
})

data_2 <- teal_data()
data_2 <- within(data_2, {
  CO2 <- head(CO2)
  AIRQUALITY <- head(airquality)
})

modules_1 <- modules(
  example_module("Single Example Module")
)

modules_2 <- modules(
  example_module("Example Module 1"),
  example_module("Example Module 2")
)

nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/basic-teal"
gh_issues_page <- "https://github.com/insightsengineering/teal.gallery/issues"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("Shiny app with teal as shiny modules", style = "font-size: 30px;"),
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

ui <- fluidPage(
  title = tags$head(
    tags$link(rel = "icon", href = nest_logo),
    tags$title("Teal as a Shiny Module")
  ),
  header,
  ui_teal("teal_1", modules_1),
  ui_teal("teal_2", modules_2),
  footer,
  ui_session_info("sessioninfo")
)

server <- function(input, output, session) {
  srv_teal("teal_1", data_1, modules_1)
  srv_teal("teal_2", data_2, modules_2)
  srv_session_info("sessioninfo")
}

shinyApp(ui, server)
