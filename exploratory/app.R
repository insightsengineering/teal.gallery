library(teal.modules.general)
options(
  shiny.useragg = FALSE,
  teal.ggplot2_args = teal.widgets::ggplot2_args(labs = list(caption = "NEST PROJECT"))
)

nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
  library(scda)
  library(scda.2022)
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


  ADSL <- synthetic_cdisc_data("latest")$adsl
  ADRS <- synthetic_cdisc_data("latest")$adrs
  ADLB <- synthetic_cdisc_data("latest")$adlb

  ADLBPCA <- ADLB %>%
    dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) %>%
    tidyr::pivot_wider(
      values_from = "AVAL",
      names_from = c("PARAMCD", "AVISIT"),
      names_sep = " - "
    )
})

datanames <- c("ADSL", "ADRS", "ADLB", "ADLBPCA")
datanames(data) <- datanames

join_keys(data) <- default_cdisc_join_keys[datanames]
join_keys(data)["ADLBPCA"] <- c("USUBJID", "STUDYID", "PARAMCD", "AVISIT")
join_keys(data)["ADLBPCA", "ADSL"] <- c("USUBJID", "STUDYID")

## Reusable Configuration For Modules
ADSL <- data[["ADSL"]]
ADRS <- data[["ADRS"]]
ADLB <- data[["ADLB"]]
ADLBPCA <- data[["ADLBPCA"]]

adsl_extracted_num <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL),
    selected = "AGE",
    multiple = FALSE,
    fixed = FALSE
  )
)
adsl_extracted_num2 <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )
)
adsl_extracted_fct <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL),
    selected = "ARMCD",
    multiple = FALSE,
    fixed = FALSE
  )
)
fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
adsl_extracted_fct2 <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL, subset = fact_vars_adsl),
    selected = "STRATA2",
    multiple = FALSE,
    fixed = FALSE
  )
)
adsl_extracted_fct3 <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL),
    selected = "ARMCD",
    multiple = TRUE,
    fixed = FALSE
  )
)
numeric_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.numeric)))
adsl_extracted_numeric <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL, subset = numeric_vars_adsl),
    selected = "BMRKR1",
    multiple = FALSE,
    fixed = FALSE
  )
)
adsl_extracted_factors <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL, subset = fact_vars_adsl),
    selected = NULL,
    multiple = FALSE,
    fixed = FALSE
  )
)

adsl_extracted_multi <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL),
    selected = c("AGE", "BMRKR1"),
    multiple = TRUE,
    fixed = FALSE
  )
)

adrs_filters <- filter_spec(
  vars = "PARAMCD",
  sep = " - ",
  choices = value_choices(ADRS, "PARAMCD", "PARAM", c("BESRSPI", "INVET")),
  selected = "BESRSPI",
  multiple = FALSE,
  label = "Choose endpoint"
)

adrs_extracted_response <- data_extract_spec(
  dataname = "ADRS",
  filter = adrs_filters,
  select = select_spec(
    choices = variable_choices(ADRS, c("AVALC", "AVAL")),
    selected = "AVALC",
    multiple = FALSE,
    fixed = FALSE
  )
)

fact_vars_adrs <- names(Filter(isTRUE, sapply(ADRS, is.factor)))
adrs_extracted_response_fct <- data_extract_spec(
  dataname = "ADRS",
  filter = adrs_filters,
  select = select_spec(
    choices = variable_choices(ADRS, subset = fact_vars_adrs),
    selected = "AVALC",
    multiple = FALSE,
    fixed = FALSE
  )
)

adlb_filter_paramcd <- filter_spec(
  vars = "PARAMCD",
  choices = value_choices(ADLB, "PARAMCD", "PARAM"),
  selected = levels(ADLB$PARAMCD)[1],
  multiple = FALSE,
  label = "Select lab:"
)
adlb_filter_paramcd2 <- filter_spec(
  vars = "PARAMCD",
  choices = value_choices(ADLB, "PARAMCD", "PARAM"),
  selected = levels(ADLB$PARAMCD)[2],
  multiple = FALSE,
  label = "Select lab:"
)
adlb_filter_visit <- filter_spec(
  vars = "AVISIT",
  choices = levels(ADLB$AVISIT),
  selected = levels(ADLB$AVISIT)[1],
  multiple = FALSE,
  label = "Select visit:"
)
adlb_extracted_aval <- data_extract_spec(
  dataname = "ADLB",
  select = select_spec(
    choices = variable_choices(ADLB, c("AVAL", "CHG", "PCHG", "ANRIND", "BASE")),
    selected = "AVAL",
    multiple = FALSE,
    fixed = FALSE
  ),
  filter = list(
    adlb_filter_paramcd,
    adlb_filter_visit
  )
)

numeric_vars_adlbpca <- names(Filter(isTRUE, sapply(ADLBPCA, is.numeric)))

distr_filter_spec <- filter_spec(
  vars = choices_selected(
    variable_choices(ADSL, fact_vars_adsl),
    selected = NULL
  ),
  multiple = TRUE
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
      header_text = c("Info about input data source" = "This app uses CDISC ADaM datasets randomly generated by `scda` & `scda.2022` R packages"),
      tables = list(`NEST packages used in this demo app` = data.frame(Packages = c("teal.modules.general", "scda", "scda.2022")))
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
      dist_var = adsl_extracted_numeric,
      strata_var = data_extract_spec(
        dataname = "ADSL",
        filter = distr_filter_spec
      ),
      group_var = data_extract_spec(
        dataname = "ADSL",
        filter = distr_filter_spec
      )
    ),
    tm_outliers(
      "Outliers",
      outlier_var = data_extract_spec(
        dataname = "ADLB",
        select = select_spec(
          choices = variable_choices(ADLB, c("AVAL", "CHG", "PCHG", "BASE")),
          selected = "AVAL",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      categorical_var = data_extract_spec(
        dataname = "ADLB",
        select = select_spec(
          choices = variable_choices(ADLB, c("PARAM", "PARAMCD")),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      )
    ),
    tm_g_association(
      ref = adsl_extracted_num,
      vars = adsl_extracted_fct3
    ),
    tm_g_bivariate(
      x = adsl_extracted_num,
      y = adlb_extracted_aval,
      row_facet = adsl_extracted_factors,
      col_facet = adsl_extracted_factors,
      use_density = FALSE,
      plot_height = c(600L, 200L, 2000L),
      ggtheme = "gray"
    ),
    tm_a_regression(
      label = "Regression",
      response = adsl_extracted_numeric,
      regressor = adrs_extracted_response
    ),
    tm_g_response(
      response = adrs_extracted_response_fct,
      x = adsl_extracted_fct2,
      row_facet = adsl_extracted_factors,
      col_facet = adsl_extracted_factors,
      coord_flip = FALSE
    ),
    tm_g_scatterplotmatrix(
      label = "Scatterplot matrix",
      variables = adsl_extracted_multi
    ),
    tm_g_scatterplot(
      "Scatterplot",
      x = adsl_extracted_num,
      y = adsl_extracted_num2,
      row_facet = adsl_extracted_factors,
      col_facet = adsl_extracted_factors,
      color_by = adsl_extracted_factors,
      size = 3, alpha = 1,
      plot_height = c(600L, 200L, 2000L)
    ),
    tm_t_crosstable(
      "Table Choices",
      x = adsl_extracted_fct2,
      y = adsl_extracted_fct
    ),
    tm_a_pca(
      "Principal Component Analysis",
      dat = data_extract_spec(
        dataname = "ADLBPCA",
        select = select_spec(
          choices = variable_choices(ADLBPCA, numeric_vars_adlbpca),
          selected = c("ALT - WEEK 5 DAY 36", "CRP - WEEK 5 DAY 36", "IGA - WEEK 5 DAY 36"),
          multiple = TRUE,
          fixed = FALSE,
          label = "Variable"
        ),
      ),
      plot_height = c(600L, 200L, 2000L),
      plot_width = c(600L, 200L, 2000L)
    )
  ),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
    tags$head(tags$link(rel = "shortcut icon", href = nest_logo), tags$title("Exploratory Analysis Teal Demo App")),
    tags$span(
      style = "font-size: 30px;",
      "Example teal app for general dataset exploration with teal.modules.general"
    ),
    tags$span(
      style = "display: flex; align-items: center;",
      tags$img(src = nest_logo, alt = "NEST logo", height = "45px", style = "margin-right:10px;"),
      tags$span(style = "font-size: 24px;", "NEST @ Roche")
    )
  ),
  footer = tags$p(
    actionLink("showAboutModal", "About,"),
    tags$a(
      href = "https://github.com/insightsengineering/teal.gallery/tree/main/exploratory",
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
      tags$p(
        "This teal app is brought to you by the NEST Team at Roche/Genentech.
        For more information, please visit:"
      ),
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
