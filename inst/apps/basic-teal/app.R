library(teal)

app <- init(
  data = teal_data(
    dataset("IRIS", iris),
    dataset("MTCARS", mtcars)
  ),
  modules = modules(example_module()),
  header = "My first teal application"
)

shinyApp(app$ui, app$server)
