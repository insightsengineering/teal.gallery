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

ui <- fluidPage(
  title = tags$head(
    tags$link(rel = "icon", href = nest_logo),
    tags$title("Teal as a Shiny Module")
  ),
  ui_teal("teal_1", modules_1),
  ui_teal("teal_2", modules_2),
  ui_session_info("sessioninfo")
)

server <- function(input, output, session) {
  srv_teal("teal_1", data_1, modules_1)
  srv_teal("teal_2", data_2, modules_2)
  srv_session_info("sessioninfo")
}

shinyApp(ui, server)
