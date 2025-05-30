library(teal.modules.clinical)
library(teal.modules.general)
library(teal.osprey)
options(shiny.useragg = FALSE)

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
  library(random.cdisc.data)
  library(dplyr)
  library(nestcolor)
  # optional libraries
  library(sparkline)

  ADSL <- radsl(seed = 1)

  # derive ADSL treatment duration
  .adsl_labels <- teal.data::col_labels(ADSL, fill = FALSE)
  ADSL <- ADSL %>%
    mutate(
      TRTDURD = as.numeric(as.Date(TRTEDTM) - as.Date(TRTSDTM)) + 1,
      DTHFL = ifelse(!is.na(DTHDT), "Y", NA),
      EOSSTT = factor(EOSSTT, levels = c("COMPLETED", "ONGOING", "DISCONTINUED"))
    ) %>%
    teal.data::col_relabel(
      TRTDURD = "Treatment Duration in Days",
      DTHFL = "Death Flag",
      DCSREAS = "Reason for Study Discontinuation",
      EOSSTT = "End of Study Status"
    ) %>%
    droplevels()
  teal.data::col_labels(ADSL)[c(names(.adsl_labels))] <- .adsl_labels

  ADAE <- radae(ADSL, seed = 1)

  # derive common flags for AEs
  ADAE <- ADAE %>%
    mutate_at(c("AESOC", "AEBODSYS", "AEHLT", "AEDECOD", "AETERM", "AELLT"), as.character) %>%
    mutate(
      RELFL = ifelse(AEREL == "Y", "Y", "N"),
      CTC35FL = ifelse(AETOXGR %in% c("3", "4", "5"), "Y", "N"),
      SERFL = ifelse(AESER == "Y", "Y", "N"),
      RELSERFL = ifelse(AEREL == "Y" & AESER == "Y", "Y", "N"),
      AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X"),
      AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo"),
      ASTDT = as.Date(ASTDTM),
      AENDT = as.Date(AENDTM)
    ) %>%
    teal.data::col_relabel(
      RELFL = "Related AE",
      CTC35FL = "Grade >=3 AE",
      SERFL = "Serious AE",
      RELSERFL = "Related Serious AE",
      AEREL1 = "AE related to A: Drug X",
      AEREL2 = "AE related to B: Placebo",
      ASTDT = "Analysis Start Date",
      AENDT = "Analysis End Date",
      AESOC = "Primary System Organ Class",
      AEBODSYS = "Body System or Organ Class",
      AEHLT = "High Level Term",
      AEDECOD = "Dictionary-Derived Term",
      AETERM = "Reported Term for the Adverse Event",
      AELLT = "Lowest Level Term"
    )

  ADCM <- radcm(ADSL, seed = 1)

  # process ADCM
  ADCM <- ADCM %>%
    mutate(
      ASTDT = as.Date(ASTDTM),
      AENDT = as.Date(AENDTM)
    ) %>%
    teal.data::col_relabel(
      ASTDT = "Analysis Start Date",
      AENDT = "Analysis End Date"
    )

  ADEX <- radex(ADSL, seed = 1)

  # process ADEX
  ADEX <- ADEX %>%
    mutate(
      ASTDT = as.Date(ASTDTM),
      AENDT = as.Date(AENDTM)
    ) %>%
    teal.data::col_relabel(
      ASTDT = "Analysis Start Date",
      AENDT = "Analysis End Date"
    )

  ADTR <- radtr(ADSL, seed = 1)

  # process ADTR
  .adtr_labels <- teal.data::col_labels(ADTR, fill = FALSE)
  ADTR <- ADTR %>%
    mutate(
      PCHG = ifelse(AVISIT == "BASELINE", 0, PCHG),
      CHG = ifelse(AVISIT == "BASELINE", 0, CHG),
      AVAL = ifelse(AVISIT == "BASELINE", BASE, AVAL),
      AVALC = ifelse(AVISIT == "BASELINE", as.character(BASE), AVALC)
    ) %>%
    filter(AVISIT != "SCREENING")
  teal.data::col_labels(ADTR) <- .adtr_labels

  ADTRWF <- ADTR %>%
    filter(AVISIT != "BASELINE")
  teal.data::col_labels(ADTRWF) <- teal.data::col_labels(ADTR, fill = FALSE)


  # process ADRS
  ADRSSWIM <- radrs(ADSL, seed = 1) %>%
    filter(PARAMCD == "OVRINV") %>%
    arrange(USUBJID)

  ADRS <- radrs(ADSL, seed = 1)
  ADRS <- ADRS %>%
    filter(PARAMCD %in% c("BESRSPI", "INVET")) %>%
    mutate(ADT = as.Date(ADTM)) %>%
    droplevels()
  teal.data::col_labels(ADRS)["ADT"] <- "Analysis Date"

  ADLB <- radlb(ADSL, seed = 1)

  # process ADLB
  ADLB <- ADLB %>%
    mutate(
      ADT = as.Date(ADTM),
      LBSTRESN = as.numeric(gsub("[^0-9]", "", LBSTRESC))
    ) %>%
    teal.data::col_relabel(
      ADT = "Analysis Date",
      LBSTRESN = "Numeric Result/Finding in Standard Units"
    )
})

# set join keys
join_keys(data) <- default_cdisc_join_keys[
  c("ADSL", "ADAE", "ADCM", "ADEX", "ADTR", "ADTRWF", "ADRS", "ADRSSWIM", "ADLB")
] # get default keys by name
join_keys(data)["ADTR", "ADTR"] <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
join_keys(data)["ADTRWF", "ADTRWF"] <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
join_keys(data)["ADRSSWIM", "ADRSSWIM"] <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
join_keys(data)["ADSL", "ADTR"] <- c("STUDYID", "USUBJID")
join_keys(data)["ADSL", "ADTRWF"] <- c("STUDYID", "USUBJID")
join_keys(data)["ADSL", "ADRSSWIM"] <- c("STUDYID", "USUBJID")

## App configuration ----
# reuse object from teal_data
ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]
ADRS <- data[["ADRS"]]
ADTR <- data[["ADTR"]]
ADEX <- data[["ADEX"]]
ADCM <- data[["ADCM"]]
ADTRWF <- data[["ADTRWF"]]
ADRSSWIM <- data[["ADRSSWIM"]]
ADLB <- data[["ADLB"]]


fact_vars_asl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))

arm_vars <- c("ARMCD", "ARM", "ACTARMCD", "ACTARM", "EOSSTT")
aeflag_vars <- c("RELFL", "CTC35FL", "SERFL", "RELSERFL")
facet_vars <- c("SEX", "BMRKR2", "RACE", "STRATA1", "STRATA2")
ds_vars <- c("EOSSTT", "DCSREAS")

cs_arm_var <- choices_selected(
  choices = variable_choices(ADSL, subset = arm_vars),
  selected = "ARM"
)

cs_aeflag_var <- choices_selected(
  choices = variable_choices(ADAE, subset = aeflag_vars),
  selected = NULL
)

cs_aeterm_var <- choices_selected(
  choices = variable_choices(ADAE, subset = c("AEDECOD", "AETERM", "AELLT", "AESOC", "AEBODSYS", "AEHLT")),
  selected = "AEDECOD"
)

cs_facet_var <- choices_selected(
  choices = variable_choices(ADSL, subset = facet_vars),
  selected = NULL
)

cs_ds_var <- choices_selected(
  choices = variable_choices(ADSL, ds_vars),
  selected = ds_vars
)

cs_bar_var <- choices_selected(
  choices = variable_choices(ADSL, c("TRTDURD", "EOSDY")),
  selected = "TRTDURD"
)

cs_paramcd_rsp <- choices_selected(
  choices = value_choices(ADRS, "PARAMCD", "PARAM", subset = c("BESRSPI", "INVET")),
  selected = "BESRSPI"
)

cs_paramcd_tr <- choices_selected(
  choices = value_choices(ADTR, "PARAMCD", "PARAM", subset = "SLDINV"),
  selected = "SLDINV"
)

## App header and footer ----
nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/early-dev"
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
    teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
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
        `NEST packages used in this demo app` =
          data.frame(Packages = c("teal.modules.general", "teal.modules.clinical", "teal.osprey", "random.cdisc.data"))
      )
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    modules(
      label = "Adverse Events",
      tm_g_events_term_id(
        label = "Common AE Plot",
        dataname = "ADAE",
        term_var = cs_aeterm_var,
        arm_var = cs_arm_var,
        plot_height = c(800, 200, 2000)
      ),
      tm_g_ae_oview(
        label = "AE Overview Plot",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        flag_var_anl = choices_selected(
          choices = variable_choices(ADAE, c("AEREL1", "AEREL2")),
          selected = NULL
        ),
        plot_height = c(800, 200, 2000)
      ),
      tm_g_butterfly(
        label = "Butterfly Plot",
        dataname = "ADAE",
        filter_var = cs_aeflag_var,
        right_var = cs_arm_var,
        left_var = cs_arm_var,
        category_var = choices_selected(
          choices = variable_choices(ADAE, subset = c("AEDECOD", "AETERM", "AESOC", "AEBODSYS")),
          selected = "AEBODSYS"
        ),
        color_by_var = choices_selected(
          choices = c(variable_choices(ADAE, "AETOXGR"), "None"),
          selected = "AETOXGR"
        ),
        count_by_var = choices_selected(
          choices = c("# of patients", "# of AEs"),
          selected = "# of patients"
        ),
        facet_var = cs_facet_var,
        sort_by_var = choices_selected(choices = c("count", "alphabetical"), selected = "count"),
        legend_on = TRUE,
        plot_height = c(600, 200, 2000)
      ),
      tm_g_ae_sub(
        label = "AE by Subgroup",
        dataname = "ADAE",
        arm_var = choices_selected(
          selected = "ACTARMCD",
          choices = c("ACTARM", "ACTARMCD")
        ),
        group_var = choices_selected(
          selected = c("SEX", "REGION1", "RACE"),
          choices = c("SEX", "REGION1", "RACE")
        ),
        plot_height = c(600, 200, 2000)
      )
    ),
    tm_t_summary(
      label = "Disposition Table",
      dataname = "ADSL",
      arm_var = cs_arm_var,
      summarize_vars = cs_ds_var,
      useNA = "no"
    ),
    tm_g_spiderplot(
      label = "Spider plot",
      dataname = "ADTR",
      paramcd = cs_paramcd_tr,
      x_var = choices_selected(choices = variable_choices(ADTR, "ADY"), selected = "ADY"),
      y_var = choices_selected(
        choices = variable_choices(ADTR, c("PCHG", "CHG", "AVAL")),
        selected = "PCHG"
      ),
      marker_var = choices_selected(
        choices = variable_choices(ADSL, c("SEX", "RACE")),
        selected = "SEX"
      ),
      line_colorby_var = cs_arm_var,
      xfacet_var = cs_facet_var,
      yfacet_var = cs_facet_var,
      vref_line = "42",
      href_line = "-20, 0, 30",
      anno_txt_var = TRUE,
      legend_on = FALSE,
      plot_height = c(600, 200, 2000)
    ),
    tm_g_swimlane(
      label = "Swimlane Plot",
      dataname = "ADRSSWIM",
      bar_var = cs_bar_var,
      bar_color_var = choices_selected(
        choices = variable_choices(ADSL, fact_vars_asl),
        selected = "EOSSTT"
      ),
      sort_var = choices_selected(
        choices = variable_choices(ADSL, c(arm_vars, "TRTDURD")),
        selected = arm_vars[1]
      ),
      marker_pos_var = choices_selected(
        choices = c(variable_choices(ADRS, "ADY")),
        selected = "ADY"
      ),
      marker_shape_var = choices_selected(c(variable_choices(ADRS, c("AVALC", "AVISIT"))),
        selected = "AVALC"
      ),
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "Death" = 8),
      marker_color_var = choices_selected(
        choices = c(variable_choices(ADRS, c("AVALC", "AVISIT"))),
        selected = "AVALC"
      ),
      marker_color_opt = c(
        "CR" = "green", "PR" = "blue", "SD" = "goldenrod",
        "PD" = "red", "Death" = "black"
      ),
      vref_line = c(30, 60),
      anno_txt_var = choices_selected(
        choices = variable_choices(ADSL, intersect(arm_vars, fact_vars_asl)),
        selected = arm_vars[1]
      )
    ),
    tm_g_waterfall(
      label = "Waterfall Plot",
      dataname_tr = "ADTRWF",
      dataname_rs = "ADRS",
      bar_paramcd = cs_paramcd_tr,
      bar_var = choices_selected(variable_choices(ADTRWF, "PCHG"), "PCHG"),
      bar_color_var = choices_selected(variable_choices(ADSL, fact_vars_asl), "ARMCD"),
      sort_var = choices_selected(variable_choices(ADSL, fact_vars_asl), NULL),
      add_label_var_sl = choices_selected(variable_choices(ADSL, fact_vars_asl), NULL),
      add_label_paramcd_rs = cs_paramcd_rsp,
      anno_txt_var_sl = choices_selected(variable_choices(ADSL, fact_vars_asl), NULL),
      anno_txt_paramcd_rs = cs_paramcd_rsp,
      facet_var = cs_facet_var,
      ytick_at = 20,
      href_line = "-30, 20",
      gap_point_val = NULL,
      plot_height = c(1200L, 400L, 5000L)
    ),
    tm_g_patient_profile(
      label = "Patient Profile Plot",
      patient_id = choices_selected(
        choices = unique(ADSL$USUBJID),
        selected = unique(ADSL$USUBJID)[1]
      ),
      sl_dataname = "ADSL",
      ex_dataname = "ADEX",
      ae_dataname = "ADAE",
      rs_dataname = "ADRS",
      cm_dataname = "ADCM",
      lb_dataname = "ADLB",
      sl_start_date = choices_selected(
        selected = "TRTSDTM",
        choices = variable_choices(ADSL, subset = c("TRTSDTM", "RANDDT"))
      ),
      ex_var = choices_selected(
        selected = "PARCAT2",
        choices = variable_choices(ADEX, "PARCAT2")
      ),
      ae_var = choices_selected(
        selected = "AEDECOD",
        choices = variable_choices(ADAE, c("AEDECOD", "AESOC"))
      ),
      ae_line_col_var = choices_selected(
        selected = "AESER",
        choices = variable_choices(ADAE, c("AESER", "AEREL"))
      ),
      ae_line_col_opt = c("Y" = "red", "N" = "blue"),
      rs_var = choices_selected(
        selected = "PARAMCD",
        choices = variable_choices(ADRS, c("PARAMCD", "PARAM"))
      ),
      cm_var = choices_selected(
        selected = "CMDECOD",
        choices = variable_choices(ADCM, c("CMDECOD", "CMCAT"))
      ),
      lb_var = choices_selected(
        selected = "PARAMCD",
        choices = variable_choices(ADLB, c("PARAMCD", "PARAM"))
      ),
      x_limit = "-28, 750",
      plot_height = c(1200, 400, 5000)
    )
  )
) |>
  modify_title(
    title = "Early Development Analysis Teal Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

## Start Teal Shiny App ----
shinyApp(app$ui, app$server)
