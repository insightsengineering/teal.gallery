library(teal.modules.general)
library(teal.modules.clinical)
options(shiny.useragg = FALSE)

## Data reproducible code ----
data <- teal_data()
data <- within(data, {
  library(dplyr)
  library(nestcolor)
  # optional libraries
  library(sparkline)

  ADSL <- random.cdisc.data::cadsl
  ADAE <- random.cdisc.data::cadae
  ADAETTE <- random.cdisc.data::cadaette
  ADAETTE <- ADAETTE %>%
    mutate(is_event = case_when(
      grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ TRUE,
      TRUE ~ CNSR == 0
    )) %>%
    mutate(n_events = case_when(
      grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ as.integer(.data$AVAL),
      TRUE ~ as.integer(is_event)
    )) %>%
    teal.data::col_relabel(is_event = "Is an Event") %>%
    teal.data::col_relabel(n_events = "Number of Events")
  .ADAETTE_AE <- filter(ADAETTE, grepl("TOT", .data$PARAMCD, fixed = TRUE)) %>% select(-"AVAL")
  .ADAETTE_OTH <- filter(ADAETTE, !(grepl("TOT", .data$PARAMCD, fixed = TRUE)))

  .ADAETTE_TTE <- ADAETTE %>%
    filter(PARAMCD == "AEREPTTE") %>%
    select(USUBJID, ARM, ARMCD, AVAL)

  .ADAETTE_AE <- full_join(.ADAETTE_AE, .ADAETTE_TTE, by = c("USUBJID", "ARM", "ARMCD"))
  ADAETTE <- rbind(.ADAETTE_AE, .ADAETTE_OTH)

  ADEX <- random.cdisc.data::cadex
  .ADEX_labels <- teal.data::col_labels(ADEX, fill = FALSE)
  # Below steps are done to simulate data with TDURD parameter as it is not in the ADEX data from `random.cdisc.data` package
  set.seed(1, kind = "Mersenne-Twister")
  ADEX <- ADEX %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    mutate(
      PARAMCD = "TDURD",
      PARAM = "Overall duration (days)",
      AVAL = sample(
        x = seq(1, 200),
        size = n(),
        replace = TRUE
      ),
      AVALU = "Days",
      PARCAT1 = "OVERALL"
    ) %>%
    bind_rows(ADEX)
  ADEX <- ADEX %>%
    filter(PARCAT1 == "OVERALL" &
      PARAMCD %in% c("TDOSE", "TNDOSE", "TDURD"))
  teal.data::col_labels(ADEX) <- .ADEX_labels

  ADLB <- random.cdisc.data::cadlb

  ADEG <- random.cdisc.data::cadeg

  # For real data, ADVS needs some preprocessing like group different ANRIND and BNRIND into abnormal
  ADVS <- random.cdisc.data::cadvs %>%
    mutate(ONTRTFL = ifelse(AVISIT %in% c("SCREENING", "BASELINE"), "", "Y")) %>%
    teal.data::col_relabel(ONTRTFL = "On Treatment Record Flag") %>%
    mutate(ANRIND = as.character(ANRIND), BNRIND = as.character(BNRIND)) %>%
    mutate(
      ANRIND = case_when(
        ANRIND == "HIGH HIGH" ~ "HIGH",
        ANRIND == "LOW LOW" ~ "LOW",
        TRUE ~ ANRIND
      ),
      BNRIND = case_when(
        BNRIND == "HIGH HIGH" ~ "HIGH",
        BNRIND == "LOW LOW" ~ "LOW",
        TRUE ~ BNRIND
      )
    )

  ADCM <- random.cdisc.data::cadcm %>% mutate(CMSEQ = as.integer(CMSEQ))

  # Add study-specific pre-processing: convert arm, param and visit variables to factors
  # Sample code:
  # ADSL$ACTARM <- factor(ADSL$ACTARM)
  # ADAE$AETOXGR <- factor(ADAE$AETOXGR)
  # ADLB <- ADLB %>%
  #   tern::df_explicit_na(omit_columns = setdiff(names(ADLB), c("PARAM", "PARAMCD", "AVISIT") ))
  # ADEX <- ADEX %>%
  #   tern::df_explicit_na(omit_columns = setdiff(names(ADEX), c("PARAM", "PARAMCD", "PARCAT2") ))

  # define study-specific analysis subgroups and baskets from ADAE
  .add_event_flags <- function(dat) {
    dat %>%
      dplyr::mutate(
        TMPFL_SER = AESER == "Y",
        TMPFL_REL = AEREL == "Y",
        TMPFL_GR5 = AETOXGR == "5",
        TMP_SMQ01 = !is.na(SMQ01NAM),
        TMP_SMQ02 = !is.na(SMQ02NAM),
        TMP_CQ01 = !is.na(CQ01NAM)
      ) %>%
      teal.data::col_relabel(
        TMPFL_SER = "Serious AE",
        TMPFL_REL = "Related AE",
        TMPFL_GR5 = "Grade 5 AE",
        TMP_SMQ01 = aesi_label(dat$SMQ01NAM, dat$SMQ01SC),
        TMP_SMQ02 = aesi_label(dat$SMQ02NAM, dat$SMQ02SC),
        TMP_CQ01 = aesi_label(dat$CQ01NAM)
      )
  }

  ADAE <- ADAE %>%
    .add_event_flags()
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE", "ADAETTE", "ADEX", "ADLB", "ADEG", "ADVS", "ADCM")]

## App configuration ----
ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]
ADAETTE <- data[["ADAETTE"]]
ADEX <- data[["ADEX"]]
ADLB <- data[["ADLB"]]
ADEG <- data[["ADEG"]]
ADVS <- data[["ADVS"]]
ADCM <- data[["ADCM"]]

arm_vars <- c("ACTARMCD", "ACTARM")

## Create variable type lists
date_vars_adsl <-
  names(ADSL)[vapply(ADSL, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  }, logical(1))]
demog_vars_adsl <-
  names(ADSL)[!(names(ADSL) %in% c("USUBJID", "STUDYID", date_vars_adsl))]

ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
# flag variables for AE baskets; set to NULL if not applicable to study
aesi_vars <-
  names(ADAE)[startsWith(names(ADAE), "TMP_SMQ") |
    startsWith(names(ADAE), "TMP_CQ")]

## App header and footer ----
nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"
app_source <- "https://github.com/insightsengineering/teal.gallery/tree/main/safety"
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
app <- teal::init(
  data = data,
  # Set initial filter state as safety-evaluable population
  filter = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
    teal_slice(dataname = "ADSL", varname = "SEX"),
    teal_slice(dataname = "ADSL", varname = "AGE"),
    teal_slice(dataname = "ADLB", varname = "AVAL"),
    # default filter
    teal_slice(dataname = "ADEX", varname = "AVAL"),
    # default filter
    teal_slice(dataname = "ADEG", varname = "AVAL")
  ),
  modules = modules(
    tm_front_page(
      label = "App Info",
      header_text = c("Info about input data source" = "This app uses CDISC ADaM datasets randomly generated by `random.cdisc.data` R packages"),
      tables = list(`NEST packages used in this demo app` = data.frame(
        Packages = c(
          "teal.modules.general",
          "teal.modules.clinical",
          "random.cdisc.data"
        )
      ))
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = variables(arm_vars, "ACTARM"),
      summarize_vars = variables(demog_vars_adsl, c("SEX", "AGE", "RACE"))
    ),
    modules(
      label = "Adverse Events",
      tm_t_events_summary(
        label = "AE Summary",
        dataname = "ADAE",
        arm_var = variables(arm_vars, "ACTARM"),
        flag_var_anl = variables(ae_anl_vars, ae_anl_vars, multiple = TRUE),
        flag_var_aesi = variables(aesi_vars, aesi_vars, multiple = TRUE),
        add_total = TRUE
      ),
      tm_t_events(
        label = "AE by Term",
        dataname = "ADAE",
        arm_var = variables(arm_vars, "ACTARM"),
        llt = variables(c("AETERM", "AEDECOD"), "AEDECOD"),
        hlt = variables(c("AEBODSYS", "AESOC"), "AEBODSYS"),
        add_total = TRUE,
        event_type = "adverse event"
      ),
      tm_t_events_by_grade(
        label = "AE Table by Grade",
        dataname = "ADAE",
        arm_var = variables(arm_vars, "ACTARM"),
        llt = variables("AEDECOD", "AEDECOD"),
        hlt = variables(c("AEBODSYS", "AESOC"), "AEBODSYS"),
        grade = variables("AETOXGR", "AETOXGR"),
        add_total = TRUE
      ),
      tm_t_events_patyear(
        label = "AE Rates Adjusted for Patient-Years at Risk",
        dataname = "ADAETTE",
        arm_var = variables(arm_vars, "ACTARM"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "AETTE1", multiple = FALSE),
          check_dataset = FALSE
        ),
        events_var = variables("n_events", "n_events")
      ),
      tm_t_smq(
        label = "Adverse Events by SMQ Table",
        dataname = "ADAE",
        arm_var = variables(c(arm_vars, "SEX"), "ACTARM"),
        add_total = FALSE,
        baskets = variables(
          function(x) grepl("^(SMQ|CQ).*NAM$", names(parent.frame()$X)[[parent.frame()$i]], value = TRUE),
          dplyr::everything(),
          multiple = TRUE
        ),
        scopes = variables(
          function(x) grep("^SMQ.*SC$", names(parent.frame()$X)[[parent.frame()$i]], value = TRUE),
          dplyr::everything(),
          multiple = TRUE,
          fixed = TRUE
        ),
        llt = variables("AEDECOD", "AEDECOD")
      )
    ),
    modules(
      label = "Lab Tables",
      tm_t_summary_by(
        label = "Labs Summary",
        dataname = "ADLB",
        arm_var = variables(arm_vars, "ACTARM"),
        by_vars = variables(c("PARAM", "AVISIT"), c("PARAM", "AVISIT"), multiple = TRUE, fixed = TRUE),
        summarize_vars = variables(c("AVAL", "CHG"), "AVAL"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "ALT", multiple = TRUE),
          check_dataset = FALSE
        )
      ),
      tm_t_shift_by_grade(
        label = "Grade Laboratory Abnormality Table",
        dataname = "ADLB",
        arm_var = variables(c("ACTARMCD", "ACTARM"), "ACTARM"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "ALT", multiple = TRUE),
          check_dataset = FALSE
        ),
        worst_flag_var = variables(c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"), "WGRLOVFL"),
        worst_flag_indicator = teal.picks::values("Y", "Y", fixed = TRUE, multiple = FALSE),
        anl_toxgrade_var = variables("ATOXGR", "ATOXGR"),
        base_toxgrade_var = variables("BTOXGR", "BTOXGR"),
        add_total = FALSE
      ),
      tm_t_abnormality_by_worst_grade(
        label = "Laboratory test results with highest grade post-baseline",
        dataname = "ADLB",
        arm_var = variables(c("ARM", "ARMCD"), "ARM"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = c("ALT", "CRP", "IGA")),
          check_dataset = FALSE
        ),
        add_total = FALSE
      )
    ),
    modules(
      label = "Exposure",
      tm_t_summary_by(
        label = "Exposure Summary",
        dataname = "ADEX",
        arm_var = variables(arm_vars, "ACTARM"),
        by_vars = variables(c("PARCAT2", "PARAM"), c("PARCAT2", "PARAM"), fixed = TRUE),
        summarize_vars = variables("AVAL", "AVAL", fixed = TRUE),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "TDOSE", multiple = TRUE),
          check_dataset = FALSE
        ),
        denominator = values(c("n", "N", "omit"), "n")
      ),
      tm_t_exposure(
        label = "Duration of Exposure Table",
        dataname = "ADEX",
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "TDURD", multiple = FALSE),
          check_dataset = FALSE
        ),
        col_by_var = variables(c(arm_vars, "SEX"), "SEX"),
        row_by_var = variables(c("RACE", "REGION1", "STRATA1", "SEX"), "RACE"),
        parcat = picks(
          variables("PARCAT2", "PARCAT2"),
          values(selected = "Drug A"),
          check_dataset = FALSE
        ),
        add_total = FALSE
      )
    ),
    tm_t_abnormality(
      label = "Vital Signs Abnormality",
      dataname = "ADVS",
      arm_var = variables(arm_vars, "ACTARM"),
      id_var = variables("USUBJID", "USUBJID", fixed = TRUE),
      by_vars = variables(c("PARAM", "AVISIT"), "PARAM", multiple = TRUE),
      grade = variables("ANRIND", "ANRIND", fixed = TRUE),
      abnormal = list(low = "LOW", high = "HIGH")
    ),
    tm_t_mult_events(
      label = "Concomitant Medication",
      dataname = "ADCM",
      arm_var = variables(arm_vars, "ACTARM"),
      seq_var = variables("CMSEQ", "CMSEQ", fixed = TRUE),
      hlt = variables("ATC2", "ATC2", fixed = TRUE),
      llt = variables("CMDECOD", "CMDECOD", fixed = TRUE),
      add_total = TRUE,
      event_type = "treatment"
    ),
    tm_t_shift_by_arm(
      label = "ECG Shift Table by Arm",
      dataname = "ADEG",
      arm_var = variables(arm_vars, "ACTARM"),
      paramcd = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "HR", multiple = FALSE),
        check_dataset = FALSE
      ),
      visit_var = picks(
        variables("AVISIT", "AVISIT"),
        values(selected = "POST-BASELINE MINIMUM"),
        check_dataset = FALSE
      ),
      aval_var = variables("ANRIND", "ANRIND", fixed = TRUE),
      baseline_var = variables("BNRIND", "BNRIND", fixed = TRUE)
    ),
    tm_g_lineplot(
      label = "Line Plot",
      dataname = "ADLB",
      group_var = variables(arm_vars, "ACTARM"),
      x = variables("AVISIT", "AVISIT", fixed = TRUE),
      y = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      y_unit = variables("AVALU", "AVALU", fixed = TRUE),
      paramcd = picks(
        variables("PARAMCD", "PARAMCD", fixed = TRUE),
        values(selected = "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      plot_height = c(1000L, 200L, 4000L)
    )
  )
) |>
  modify_title(
    title = "Safety Analysis Teal Demo App",
    favicon = nest_logo
  ) |>
  modify_header(header) |>
  modify_footer(footer)

shinyApp(app$ui, app$server)
