library(teal)
library(bsafe)
library(teal.modules.bsafe)

data <- teal.modules.bsafe::test_data

data <- teal.data::teal_data(
  bsafe_data = data,
  code = expression({
    bsafe_data <- teal.modules.bsafe::test_data
  })
) |>
  teal.data::verify()

app <- teal::init(
  data = data,
  modules = list(
    teal.modules.bsafe:::tm_bsafe(
      label = "teal.modules.bsafe",
      dataset_name = "bsafe_data"
    )
  ),
  header = "BSAFE Application"
)
shiny::shinyApp(app$ui, app$server)
