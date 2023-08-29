library(teal)

app <- init(
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", multiple = FALSE, locked = TRUE)
  ),
  data = list(IRIS = iris, MTCARS = mtcars),
  modules = modules(example_module()),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between;",
    tags$h2("My first teal app"),
    tags$span(
      style = "display: flex; align-items: center;",
      tags$img(src = "nest.png", alt = "NEST logo", height = "60px"),
      tags$h3("NEST @ Roche")
    )
  )
)

shinyApp(app$ui, app$server)
