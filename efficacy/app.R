library(teal.modules.general)
library(teal.modules.clinical)
options(shiny.useragg = FALSE)

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
  library(dplyr)
  library(random.cdisc.data)
  library(nestcolor)
  # optional libraries
  library(sparkline)

  ADSL <- radsl(seed = 1)
  .adsl_labels <- teal.data::col_labels(ADSL, fill = FALSE)

  .char_vars_asl <- names(Filter(isTRUE, sapply(ADSL, is.character)))

  .adsl_labels <- c(
    .adsl_labels,
    AGEGR1 = "Age Group"
  )
  ADSL <- ADSL %>%
    mutate(
      AGEGR1 = factor(case_when(
        AGE < 45 ~ "<45",
        AGE >= 45 ~ ">=45"
      ))
    ) %>%
    mutate_at(.char_vars_asl, factor)

  teal.data::col_labels(ADSL) <- .adsl_labels

  ADTTE <- radtte(ADSL, seed = 1)

  ADRS <- radrs(ADSL, seed = 1)
  .adrs_labels <- teal.data::col_labels(ADRS, fill = FALSE)
  ADRS <- filter(ADRS, PARAMCD == "BESRSPI" | AVISIT == "FOLLOW UP")
  teal.data::col_labels(ADRS) <- .adrs_labels

  ADQS <- radqs(ADSL, seed = 1)
  .adqs_labels <- teal.data::col_labels(ADQS, fill = FALSE)
  ADQS <- ADQS %>%
    filter(ABLFL != "Y" & ABLFL2 != "Y") %>%
    filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) %>%
    mutate(
      AVISIT = as.factor(AVISIT),
      AVISITN = rank(AVISITN) %>%
        as.factor() %>%
        as.numeric() %>%
        as.factor()
    )
  teal.data::col_labels(ADQS) <- .adqs_labels
})

# set join_keys
join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE", "ADRS", "ADQS")]

## Reusable Configuration For Modules
ADSL <- data[["ADSL"]]
ADTTE <- data[["ADTTE"]]
ADRS <- data[["ADRS"]]
ADQS <- data[["ADQS"]]
char_vars_asl <- data[["char_vars_asl"]]

arm_vars <- c("ARMCD", "ARM")
strata_vars <- c("STRATA1", "STRATA2")
facet_vars <- c("AGEGR1", "BMRKR2", "SEX", "COUNTRY")
cov_vars <- c("AGE", "SEX", "BMRKR1", "BMRKR2", "REGION1")
visit_vars <- c("AVISIT", "AVISITN")

cs_arm_var <- choices_selected(
  choices = variable_choices(ADSL, subset = arm_vars),
  selected = "ARM"
)

cs_strata_var <- choices_selected(
  choices = variable_choices(ADSL, subset = strata_vars),
  selected = "STRATA1"
)

cs_facet_var <- choices_selected(
  choices = variable_choices(ADSL, subset = facet_vars),
  selected = "AGEGR1"
)

cs_cov_var <- choices_selected(
  choices = variable_choices(ADSL, subset = cov_vars),
  selected = "AGE"
)

cs_paramcd_tte <- choices_selected(
  choices = value_choices(ADTTE, "PARAMCD", "PARAM"),
  selected = "OS"
)

cs_paramcd_rsp <- choices_selected(
  choices = value_choices(ADRS, "PARAMCD", "PARAM"),
  selected = "BESRSPI"
)

cs_paramcd_qs <- choices_selected(
  choices = value_choices(ADQS, "PARAMCD", "PARAM"),
  selected = "FKSI-FWB"
)

cs_visit_var_qs <- choices_selected(
  choices = variable_choices(ADQS, subset = visit_vars),
  selected = "AVISIT"
)

fact_vars_asl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
fact_vars_asl_orig <- fact_vars_asl[!fact_vars_asl %in% char_vars_asl]

date_vars_asl <- names(ADSL)[vapply(ADSL, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
demog_vars_asl <- names(ADSL)[!(names(ADSL) %in% c("USUBJID", "STUDYID", date_vars_asl))]

# reference & comparison arm selection when switching the arm variable
# ARMCD is given in a delayed fashion using value choices and
# ARM is given with the ref and comp levels supplied explicitly
arm_ref_comp <- list(
  ARMCD = list(
    ref = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = "ARM A"),
    comp = value_choices("ADSL", var_choices = "ARMCD", var_label = "ARM", subset = c("ARM B", "ARM C"))
  ),
  ARM = list(ref = "A: Drug X", comp = c("B: Placebo", "C: Combination"))
)

## App header and footer ----
nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/efficacy"
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

## Setup App
app <- init(
  data = data,
  filter = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "ITTFL", selected = "Y"),
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
      tables = list(`NEST packages used in this demo app` = data.frame(
        Packages = c("teal.modules.general", "teal.modules.clinical", "random.cdisc.data")
      ))
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = cs_arm_var,
      summarize_vars = choices_selected(
        choices = variable_choices(ADSL, demog_vars_asl),
        selected = c("SEX", "AGE", "RACE")
      )
    ),
    modules(
      label = "Forest Plots",
      tm_g_forest_tte(
        label = "Survival Forest Plot",
        dataname = "ADTTE",
        arm_var = cs_arm_var,
        strata_var = cs_strata_var,
        subgroup_var = cs_facet_var,
        paramcd = cs_paramcd_tte,
        plot_height = c(800L, 200L, 4000L)
      ),
      tm_g_forest_rsp(
        label = "Response Forest Plot",
        dataname = "ADRS",
        arm_var = cs_arm_var,
        strata_var = cs_strata_var,
        subgroup_var = cs_facet_var,
        paramcd = cs_paramcd_rsp,
        plot_height = c(800L, 200L, 4000L)
      )
    ),
    tm_g_km(
      label = "Kaplan Meier Plot",
      dataname = "ADTTE",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_tte,
      facet_var = cs_facet_var,
      strata_var = cs_strata_var,
      plot_height = c(1800L, 200L, 4000L)
    ),
    tm_t_binary_outcome(
      label = "Response Table",
      dataname = "ADRS",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_rsp,
      strata_var = cs_strata_var,
      rsp_table = TRUE
    ),
    tm_t_tte(
      label = "Time To Event Table",
      dataname = "ADTTE",
      arm_var = cs_arm_var,
      paramcd = cs_paramcd_tte,
      strata_var = cs_strata_var,
      time_points = choices_selected(c(182, 365, 547), 182),
      event_desc_var = choices_selected(
        choices = variable_choices("ADTTE", "EVNTDESC"),
        selected = "EVNTDESC",
        fixed = TRUE
      )
    ),
    tm_t_crosstable(
      "Cross Table",
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(ADSL, fact_vars_asl_orig),
          selected = fact_vars_asl_orig[1]
        )
      ),
      y = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(ADSL, fact_vars_asl_orig),
          selected = fact_vars_asl_orig[4]
        )
      )
    ),
    tm_t_coxreg(
      label = "Cox Reg",
      dataname = "ADTTE",
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_tte,
      strata_var = cs_strata_var,
      cov_var = cs_cov_var
    ),
    tm_t_logistic(
      label = "Logistic Reg",
      dataname = "ADRS",
      arm_var = cs_arm_var,
      arm_ref_comp = NULL,
      paramcd = cs_paramcd_rsp,
      cov_var = cs_cov_var
    ),
    tm_a_mmrm(
      label = "MMRM",
      dataname = "ADQS",
      aval_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
      id_var = choices_selected(c("USUBJID", "SUBJID"), "USUBJID"),
      arm_var = cs_arm_var,
      visit_var = cs_visit_var_qs,
      arm_ref_comp = arm_ref_comp,
      paramcd = cs_paramcd_qs,
      cov_var = choices_selected(c("BASE", "AGE", "SEX", "BASE:AVISIT"), NULL),
      conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95)
    ),
    tm_t_binary_outcome(
      label = "Binary Response",
      dataname = "ADRS",
      arm_var = cs_arm_var,
      paramcd = cs_paramcd_rsp,
      strata_var = cs_strata_var
    ),
    tm_t_ancova(
      label = "ANCOVA",
      dataname = "ADQS",
      avisit = choices_selected(value_choices(ADQS, "AVISIT"), "WEEK 1 DAY 8"),
      arm_var = cs_arm_var,
      arm_ref_comp = arm_ref_comp,
      aval_var = choices_selected(variable_choices(ADQS, c("AVAL", "CHG", "PCHG")), "CHG"),
      cov_var = choices_selected(variable_choices(ADQS, c("BASE", "STRATA1", "SEX")), "STRATA1"),
      paramcd = cs_paramcd_qs
    )
  )
) |>
  modify_title(
    title = "Efficacy Analysis Teal Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

shinyApp(app$ui, app$server)
