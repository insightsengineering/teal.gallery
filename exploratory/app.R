library(teal.modules.general)
options(
  shiny.useragg = FALSE,
  teal.ggplot2_args = teal.widgets::ggplot2_args(labs = list(caption = "NEST PROJECT"))
)

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
  library(random.cdisc.data)
  library(dplyr)
  library(tidyr)
  library(ggExtra)
  library(ggpmisc)
  library(ggpp)
  library(goftest)
  library(gridExtra)
  library(htmlwidgets)
  library(jsonlite)
  library(lattice)
  library(MASS)
  library(rlang)
  library(rtables)
  library(nestcolor)
  library(broom)
  library(colourpicker)
  library(sparkline)


  ADSL <- radsl(seed = 1)
  ADRS <- radrs(ADSL, seed = 1)
  ADLB <- radlb(ADSL, seed = 1)
  ADLBPCA <- ADLB %>%
    dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) %>%
    tidyr::pivot_wider(
      values_from = "AVAL",
      names_from = c("PARAMCD", "AVISIT"),
      names_sep = " - "
    )
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADRS", "ADLB", "ADLBPCA")]

## Reusable Configuration For Modules
ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]
ADLB <- data[["ADLB"]]
ADLBPCA <- data[["ADLBPCA"]]

fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
numeric_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.numeric)))

pick_adsl_age <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL), selected = "AGE", multiple = FALSE)
)
pick_adsl_bmrkr1 <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL), selected = "BMRKR1", multiple = FALSE)
)
pick_adsl_armcd <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL), selected = "ARMCD", multiple = FALSE)
)
pick_adsl_strata2 <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL, subset = fact_vars_adsl), selected = "STRATA2", multiple = FALSE)
)
pick_adsl_armcd_multi <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL), selected = "ARMCD", multiple = TRUE)
)
pick_adsl_numeric_bmrkr1 <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL, subset = numeric_vars_adsl), selected = "BMRKR1", multiple = FALSE)
)
pick_adsl_factor <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL, subset = fact_vars_adsl), selected = NULL, multiple = FALSE)
)
pick_adsl_multi <- picks(
  datasets("ADSL", "ADSL"),
  variables(choices = variable_choices(ADSL), selected = c("AGE", "BMRKR1"), multiple = TRUE)
)

pick_adrs_response <- picks(
  datasets("ADRS", "ADRS"),
  variables(choices = variable_choices(ADRS, c("AVALC", "AVAL")), selected = "AVALC", multiple = FALSE)
)
fact_vars_adrs <- names(Filter(isTRUE, sapply(ADRS, is.factor)))
pick_adrs_response_fct <- picks(
  datasets("ADRS", "ADRS"),
  variables(choices = variable_choices(ADRS, subset = fact_vars_adrs), selected = "AVALC", multiple = FALSE)
)

pick_adlb_aval <- picks(
  datasets("ADLB", "ADLB"),
  variables(choices = variable_choices(ADLB, c("AVAL", "CHG", "PCHG", "ANRIND", "BASE")), selected = "AVAL", multiple = FALSE)
)
pick_adlb_outlier <- picks(
  datasets("ADLB", "ADLB"),
  variables(choices = variable_choices(ADLB, c("AVAL", "CHG", "PCHG", "BASE")), selected = "AVAL", multiple = FALSE)
)
pick_adlb_categorical <- picks(
  datasets("ADLB", "ADLB"),
  variables(choices = variable_choices(ADLB, c("PARAM", "PARAMCD")), selected = NULL, multiple = FALSE)
)

numeric_vars_adlbpca <- names(Filter(isTRUE, sapply(ADLBPCA, is.numeric)))
pick_adlbpca <- picks(
  datasets("ADLBPCA", "ADLBPCA"),
  variables(
    choices = variable_choices(ADLBPCA, subset = numeric_vars_adlbpca),
    selected = c("ALT - WEEK 5 DAY 36", "CRP - WEEK 5 DAY 36", "IGA - WEEK 5 DAY 36"),
    multiple = TRUE
  )
)

adrs_endpoint_filter <- teal_transform_filter(
  picks(
    datasets("ADRS", "ADRS"),
    variables(choices = "PARAMCD", selected = "PARAMCD", multiple = FALSE, fixed = TRUE),
    values(choices = c("BESRSPI", "INVET"), selected = "BESRSPI", multiple = FALSE)
  ),
  label = "Choose endpoint"
)
adlb_lab_filter <- teal_transform_filter(
  picks(
    datasets("ADLB", "ADLB"),
    variables(choices = "PARAMCD", selected = "PARAMCD", multiple = FALSE, fixed = TRUE),
    values(selected = levels(ADLB$PARAMCD)[1], multiple = FALSE)
  ),
  label = "Select lab"
)
adlb_visit_filter <- teal_transform_filter(
  picks(
    datasets("ADLB", "ADLB"),
    variables(choices = "AVISIT", selected = "AVISIT", multiple = FALSE, fixed = TRUE),
    values(selected = levels(ADLB$AVISIT)[1], multiple = FALSE)
  ),
  label = "Select visit"
)

## App header and footer ----
nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/exploratory"
gh_issues_page <- "https://github.com/insightsengineering/teal.gallery/issues"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("My first teal app", style = "font-size: 30px;"),
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
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SEX"),
    teal_slice(dataname = "ADSL", varname = "AGE")
  ),
  modules = modules(
    tm_front_page(
      label = "App Info",
      header_text = c(
        "Info about input data source" =
          "This app uses CDISC ADaM datasets randomly generated by `random.cdisc.data` R packages"
      ),
      tables = list(
        `NEST packages used in this demo app` = data.frame(Packages = c("teal.modules.general", "random.cdisc.data"))
      )
    ),
    tm_file_viewer(
      label = "File viewer",
      input_path = list(
        png = "https://www.r-project.org/logo/Rlogo.png",
        Rmd = "https://raw.githubusercontent.com/tidyverse/dplyr/master/README.Rmd",
        pdf = "https://cran.r-project.org/web/packages/shinyTree/shinyTree.pdf",
        "example directory" = "./packrat/desc/"
      )
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_missing_data("Missing Data"),
    tm_g_distribution(
      "Distribution",
      dist_var = pick_adsl_numeric_bmrkr1,
      strata_var = pick_adsl_factor,
      group_var = pick_adsl_factor
    ),
    tm_outliers(
      "Outliers",
      outlier_var = pick_adlb_outlier,
      categorical_var = pick_adlb_categorical
    ),
    tm_g_association(
      ref = pick_adsl_age,
      vars = pick_adsl_armcd_multi
    ),
    tm_g_bivariate(
      x = pick_adsl_age,
      y = pick_adlb_aval,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      use_density = FALSE,
      plot_height = c(600L, 200L, 2000L),
      ggtheme = "gray",
      transformators = list(adlb_lab_filter, adlb_visit_filter)
    ),
    tm_a_regression(
      label = "Regression",
      response = pick_adsl_numeric_bmrkr1,
      regressor = pick_adrs_response,
      transformators = list(adrs_endpoint_filter)
    ),
    tm_g_response(
      response = pick_adrs_response_fct,
      x = pick_adsl_strata2,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      coord_flip = FALSE,
      transformators = list(adrs_endpoint_filter)
    ),
    tm_g_scatterplotmatrix(
      label = "Scatterplot Matrix",
      variables = list(pick_adsl_multi)
    ),
    tm_g_scatterplot(
      "Scatterplot",
      x = pick_adsl_age,
      y = pick_adsl_bmrkr1,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      color_by = pick_adsl_factor,
      size = 3, alpha = 1,
      plot_height = c(600L, 200L, 2000L)
    ),
    tm_t_crosstable(
      "Table Choices",
      x = pick_adsl_strata2,
      y = pick_adsl_armcd
    ),
    tm_a_pca(
      "Principal Component Analysis",
      dat = pick_adlbpca,
      plot_height = c(600L, 200L, 2000L),
      plot_width = c(600L, 200L, 2000L)
    )
  )
) |>
  modify_title(
    title = "Exploratory Analysis Teal Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

shinyApp(app$ui, app$server)
