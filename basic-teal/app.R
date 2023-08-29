library(teal)

app <- init(
  data = list(IRIS = iris, MTCARS = mtcars),
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", multiple = FALSE, locked = TRUE)
  ),
  modules = modules(example_module()),
  header = "My first teal application"
)

shinyApp(app$ui, app$server)
