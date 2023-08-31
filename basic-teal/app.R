library(teal)

app <- init(
  data = list(IRIS = iris, MTCARS = mtcars),
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", multiple = FALSE)
  ),
  modules = modules(example_module()),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
    tags$head(tags$link(rel = "shortcut icon", href = "nest.png"), tags$title("Basic Teal")),
    tags$span(
      style = "font-size: 30px;",
      "My first teal app"
    ),
    tags$span(
      style = "display: flex; align-items: center;",
      tags$img(src = "nest.png", alt = "NEST logo", height = "45px"),
      tags$span(style = "font-size: 24px;", "NEST @ Roche")
    )
  ),
  footer = tags$p(
    actionLink("showAboutModal", "About,"),
    tags$a(
      href = "https://github.com/insightsengineering/teal.gallery/tree/main/basic-teal",
      target = "_blank",
      "Source Code,"
    ),
    tags$a(
      href = "https://github.com/insightsengineering/teal.gallery/issues",
      target = "_blank",
      "Report Issues"
    )
  )
)

body(app$server)[[length(body(app$server)) + 1]] <- quote(
  observeEvent(input$showAboutModal, {
    showModal(modalDialog(
      tags$p("This teal app is brought to you by the NEST Team at Roche/Genentech. For more information, please visit:"),
      tags$ul(
        tags$li(tags$a(
          href = "https://github.com/insightsengineering", "Insights Engineering",
          target = "blank"
        )),
        tags$li(tags$a(
          href = "https://pharmaverse.org", "Pharmaverse",
          target = "blank"
        ))
      ),
      easyClose = TRUE
    ))
  })
)

shinyApp(app$ui, app$server)
