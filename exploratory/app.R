library(teal.modules.general)
options(
  shiny.useragg = FALSE,
  teal.ggplot2_args = teal.widgets::ggplot2_args(labs = list(caption = "NEST PROJECT"))
)

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
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

  ADSL <- random.cdisc.data::cadsl
  ADRS <- random.cdisc.data::cadrs
  ADLB <- random.cdisc.data::cadlb
  ADLBPCA <- ADLB %>%
    dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) %>%
    tidyr::pivot_wider(
      values_from = "AVAL",
      names_from = c("PARAMCD", "AVISIT"),
      names_sep = " - "
    )
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADRS", "ADLB", "ADLBPCA")]

## Reusable configuration for modules ----
## teal.modules.general (>= 0.8.0) uses `teal.picks` (datasets/variables/values)
## as the recommended way to select datasets, variables and values in modules.
ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]
ADLB <- data[["ADLB"]]
ADLBPCA <- data[["ADLBPCA"]]

fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
numeric_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.numeric)))
fact_vars_adrs <- names(Filter(isTRUE, sapply(ADRS, is.factor)))
numeric_vars_adlbpca <- names(Filter(isTRUE, sapply(ADLBPCA, is.numeric)))

# --- ADSL variable pickers ---
pick_adsl_age <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL), selected = "AGE", multiple = FALSE)
)
pick_adsl_bmrkr1 <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL), selected = "BMRKR1", multiple = FALSE)
)
pick_adsl_armcd <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL), selected = "ARMCD", multiple = FALSE)
)
pick_adsl_strata2 <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL, subset = fact_vars_adsl), selected = "STRATA2", multiple = FALSE)
)
pick_adsl_armcd_multi <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL), selected = "ARMCD", multiple = TRUE)
)
pick_adsl_numeric_bmrkr1 <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL, subset = numeric_vars_adsl), selected = "BMRKR1", multiple = FALSE)
)
# Optional factor picker reused for facets / colour / strata / group (nothing selected by default)
pick_adsl_factor <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL, subset = fact_vars_adsl), selected = NULL, multiple = FALSE)
)
pick_adsl_multi <- teal.picks::picks(
  teal.picks::datasets("ADSL", "ADSL"),
  teal.picks::variables(choices = variable_choices(ADSL), selected = c("AGE", "BMRKR1"), multiple = TRUE)
)

# --- ADRS variable pickers (used together with the endpoint row-filter below) ---
pick_adrs_response <- teal.picks::picks(
  teal.picks::datasets("ADRS", "ADRS"),
  teal.picks::variables(choices = variable_choices(ADRS, c("AVALC", "AVAL")), selected = "AVALC", multiple = FALSE)
)
pick_adrs_response_fct <- teal.picks::picks(
  teal.picks::datasets("ADRS", "ADRS"),
  teal.picks::variables(choices = variable_choices(ADRS, subset = fact_vars_adrs), selected = "AVALC", multiple = FALSE)
)

# --- ADLB variable pickers ---
pick_adlb_aval <- teal.picks::picks(
  teal.picks::datasets("ADLB", "ADLB"),
  teal.picks::variables(choices = variable_choices(ADLB, c("AVAL", "CHG", "PCHG", "ANRIND", "BASE")), selected = "AVAL", multiple = FALSE)
)
pick_adlb_outlier <- teal.picks::picks(
  teal.picks::datasets("ADLB", "ADLB"),
  teal.picks::variables(choices = variable_choices(ADLB, c("AVAL", "CHG", "PCHG", "BASE")), selected = "AVAL", multiple = FALSE)
)
pick_adlb_categorical <- teal.picks::picks(
  teal.picks::datasets("ADLB", "ADLB"),
  teal.picks::variables(choices = variable_choices(ADLB, c("PARAM", "PARAMCD")), selected = NULL, multiple = FALSE)
)

# --- ADLBPCA variable picker ---
pick_adlbpca <- teal.picks::picks(
  teal.picks::datasets("ADLBPCA", "ADLBPCA"),
  teal.picks::variables(
    choices = variable_choices(ADLBPCA, subset = numeric_vars_adlbpca),
    selected = c("ALT - WEEK 5 DAY 36", "CRP - WEEK 5 DAY 36", "IGA - WEEK 5 DAY 36"),
    multiple = TRUE
  )
)

# --- Row filters (replace the old `filter_spec`) ---
# `teal_transform_filter()` builds a module transformator that filters rows by the
# chosen values of a fixed variable - the picks-era equivalent of `filter_spec()`.
adrs_endpoint_filter <- teal.picks::teal_transform_filter(
  teal.picks::picks(
    teal.picks::datasets("ADRS", "ADRS"),
    teal.picks::variables(choices = "PARAMCD", selected = "PARAMCD", multiple = FALSE, fixed = TRUE),
    teal.picks::values(choices = c("BESRSPI", "INVET"), selected = "BESRSPI", multiple = FALSE)
  ),
  label = "Choose endpoint"
)
adlb_lab_filter <- teal.picks::teal_transform_filter(
  teal.picks::picks(
    teal.picks::datasets("ADLB", "ADLB"),
    teal.picks::variables(choices = "PARAMCD", selected = "PARAMCD", multiple = FALSE, fixed = TRUE),
    teal.picks::values(selected = levels(ADLB$PARAMCD)[1], multiple = FALSE)
  ),
  label = "Select lab"
)
adlb_visit_filter <- teal.picks::teal_transform_filter(
  teal.picks::picks(
    teal.picks::datasets("ADLB", "ADLB"),
    teal.picks::variables(choices = "AVISIT", selected = "AVISIT", multiple = FALSE, fixed = TRUE),
    teal.picks::values(selected = levels(ADLB$AVISIT)[1], multiple = FALSE)
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
    teal.modules.general::tm_front_page(
      label = "App Info",
      header_text = c(
        "Info about input data source" =
          "This app uses CDISC ADaM datasets randomly generated by `random.cdisc.data` R packages"
      ),
      tables = list(
        `NEST packages used in this demo app` = data.frame(Packages = c("teal.modules.general", "random.cdisc.data"))
      )
    ),
    teal.modules.general::tm_file_viewer(
      label = "File viewer",
      input_path = list(
        png = "https://www.r-project.org/logo/Rlogo.png",
        Rmd = "https://raw.githubusercontent.com/tidyverse/dplyr/master/README.Rmd",
        pdf = "https://cran.r-project.org/web/packages/shinyTree/shinyTree.pdf",
        "example directory" = "./packrat/desc/"
      )
    ),
    teal.modules.general::tm_data_table("Data Table"),
    teal.modules.general::tm_variable_browser("Variable Browser"),
    teal.modules.general::tm_missing_data("Missing Data"),
    teal.modules.general::tm_g_distribution(
      "Distribution",
      dist_var = pick_adsl_numeric_bmrkr1,
      strata_var = pick_adsl_factor,
      group_var = pick_adsl_factor
    ),
    teal.modules.general::tm_outliers(
      "Outliers",
      outlier_var = pick_adlb_outlier,
      categorical_var = pick_adlb_categorical
    ),
    teal.modules.general::tm_g_association(
      ref = pick_adsl_age,
      vars = pick_adsl_armcd_multi
    ),
    teal.modules.general::tm_g_bivariate(
      x = pick_adsl_age,
      y = pick_adlb_aval,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      use_density = FALSE,
      plot_height = c(600L, 200L, 2000L),
      ggtheme = "gray",
      transformators = list(adlb_lab_filter, adlb_visit_filter)
    ),
    teal.modules.general::tm_a_regression(
      label = "Regression",
      response = pick_adsl_numeric_bmrkr1,
      regressor = pick_adrs_response,
      transformators = list(adrs_endpoint_filter)
    ),
    teal.modules.general::tm_g_response(
      response = pick_adrs_response_fct,
      x = pick_adsl_strata2,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      coord_flip = FALSE,
      transformators = list(adrs_endpoint_filter)
    ),
    teal.modules.general::tm_g_scatterplotmatrix(
      label = "Scatterplot Matrix",
      variables = list(pick_adsl_multi)
    ),
    teal.modules.general::tm_g_scatterplot(
      "Scatterplot",
      x = pick_adsl_age,
      y = pick_adsl_bmrkr1,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      color_by = pick_adsl_factor,
      size = 3, alpha = 1,
      plot_height = c(600L, 200L, 2000L)
    ),
    teal.modules.general::tm_t_crosstable(
      "Table Choices",
      x = pick_adsl_strata2,
      y = pick_adsl_armcd
    ),
    teal.modules.general::tm_a_pca(
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
