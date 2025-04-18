library(teal)

my_transformators <- list(
  teal_transform_module(
    label = "Keep first n-observations from IRIS",
    ui = function(id) {
      ns <- NS(id)
      div(
        checkboxInput(ns("check"), label = "Toggle `head(iris)`"),
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        eventReactive(input$check, {
          req(data())
          if (input$check) {
            within(data(), iris <- head(iris, 6))
          } else {
            data()
          }
        })
      })
    }
  ),
  teal_transform_module(
    label = "Merge datasets to get ANL",
    ui = function(id) {
      ns <- NS(id)
      tagList(
        div("Choose the two datasets to merge:"),
        teal.widgets::optionalSelectInput(ns("merge_a"), "Merge A", choices = NULL),
        teal.widgets::optionalSelectInput(ns("merge_b"), "Merge B", choices = NULL)
      )
    },
    server = function(id, data) {
      checkmate::assert_class(data, "reactive")
      moduleServer(id, function(input, output, session) {
        iv <- shinyvalidate::InputValidator$new()
        iv$add_rule("merge_a", shinyvalidate::sv_required("Please select dataset A"))
        iv$add_rule("merge_b", shinyvalidate::sv_required("Please select dataset B"))
        iv$enable()

        reactive_datanames <- reactive({
          req(data())
          teal.data::datanames(data())
        })
        observeEvent(reactive_datanames(), {
          selected_a <- isolate(input$merge_a)
          if (identical(selected_a, "")) selected_a <- restoreInput(session$ns("merge_a"), NULL)
          teal.widgets::updateOptionalSelectInput(
            session = session,
            inputId = "merge_a",
            choices = reactive_datanames(),
            selected = restoreInput(session$ns("merge_a"), selected_a)
          )

          selected_b <- isolate(input$merge_b)
          if (identical(selected_b, "")) selected <- restoreInput(session$ns("merge_b"), NULL)
          teal.widgets::updateOptionalSelectInput(
            session = session,
            inputId = "merge_b",
            choices = reactive_datanames(),
            selected = restoreInput(session$ns("merge_b"), selected_b)
          )
        })

        reactive({
          req(input$merge_a, input$merge_b)
          new_data <- within(
            data(),
            ANL <- dplyr::left_join(merge_a, merge_b),
            merge_a = as.name(input$merge_a),
            merge_b = as.name(input$merge_b)
          )
          teal.data::datanames(new_data) <- c(teal.data::datanames(new_data), "ANL")
          new_data
        })
      })
    }
  )
)

data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
  iris <- iris
  CO2 <- CO2
  factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
  CO2[factors] <- lapply(CO2[factors], as.character)
})
join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]

nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/basic-teal"
gh_issues_page <- "https://github.com/insightsengineering/teal.gallery/issues"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("Teal app with custom transform", style = "font-size: 30px;"),
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
  filter = teal_slices(
    teal_slice("ADSL", "SEX"),
    teal_slice("ADSL", "AGE", selected = c(18L, 65L))
  ),
  modules = modules(
    example_module("Module with transformations", transformators = my_transformators),
    example_module("Module with only iris transformation", transformators = my_transformators[1]),
    example_module("Module with no transformations")
  )
) |>
  modify_title(
    title = "Safety Analysis Teal Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

shinyApp(app$ui, app$server)
