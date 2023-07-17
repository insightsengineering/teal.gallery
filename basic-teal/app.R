library(teal)

app <- init(
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", multiple = FALSE, locked = TRUE)
  ),
  data = list(IRIS = iris, MTCARS = mtcars),
  modules = modules(example_module()),
  header = "My first teal application"
)

shinyApp(app$ui, app$server)
