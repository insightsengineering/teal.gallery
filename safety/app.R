options(shiny.useragg = FALSE)

library(dplyr)
library(scda)
library(scda.2022)
library(teal.modules.general)
library(teal.modules.clinical)
library(nestcolor)
# optional libraries
library(sparkline)

# code>
## Generate Data
ADSL <- synthetic_cdisc_data("latest")$adsl

## Create variable type lists
date_vars_adsl <-
  names(ADSL)[vapply(ADSL, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  }, logical(1))]
char_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.character)))

ADAE <- synthetic_cdisc_data("latest")$adae
ADAETTE <- synthetic_cdisc_data("latest")$adaette
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
ADAETTE_AE <-
  filter(ADAETTE, grepl("TOT", .data$PARAMCD, fixed = TRUE)) %>% select(-"AVAL")
ADAETTE_OTH <-
  filter(ADAETTE, !(grepl("TOT", .data$PARAMCD, fixed = TRUE)))

ADAETTE_TTE <- ADAETTE %>%
  filter(PARAMCD == "AEREPTTE") %>%
  select(USUBJID, ARM, ARMCD, AVAL)

ADAETTE_AE <-
  full_join(ADAETTE_AE, ADAETTE_TTE, by = c("USUBJID", "ARM", "ARMCD"))
ADAETTE <- rbind(ADAETTE_AE, ADAETTE_OTH)

ADEX <- synthetic_cdisc_data("latest")$adex
ADEX_labels <- teal.data::col_labels(ADEX, fill = FALSE)
# Below steps are done to simulate data with TDURD parameter as it is not in the ADEX data from scda package
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
teal.data::col_labels(ADEX) <- ADEX_labels

ADLB <- synthetic_cdisc_data("latest")$adlb

ADEG <- synthetic_cdisc_data("latest")$adeg

# For real data, ADVS needs some preprocessing like group different ANRIND and BNRIND into abnormal
ADVS <- synthetic_cdisc_data("latest")$advs %>%
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

ADCM <-
  synthetic_cdisc_data("latest")$adcm %>% mutate(CMSEQ = as.integer(CMSEQ))

# Add study-specific pre-processing: convert arm, param and visit variables to factors
# Sample code:
# ADSL$ACTARM <- factor(ADSL$ACTARM)
# ADAE$AETOXGR <- factor(ADAE$AETOXGR)
# ADLB <- ADLB %>%
#   tern::df_explicit_na(omit_columns = setdiff(names(ADLB), c("PARAM", "PARAMCD", "AVISIT") ))
# ADEX <- ADEX %>%
#   tern::df_explicit_na(omit_columns = setdiff(names(ADEX), c("PARAM", "PARAMCD", "PARCAT2") ))

# define study-specific analysis subgroups and baskets from ADAE
add_event_flags <- function(dat) {
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
  add_event_flags()
# <code

## Reusable Configuration For Modules
arm_vars <- c("ACTARMCD", "ACTARM")

demog_vars_adsl <-
  names(ADSL)[!(names(ADSL) %in% c("USUBJID", "STUDYID", date_vars_adsl))]

cs_arm_var <-
  choices_selected(
    choices = variable_choices(ADSL, subset = arm_vars),
    selected = "ACTARM"
  )

ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
# flag variables for AE baskets; set to NULL if not applicable to study
aesi_vars <-
  names(ADAE)[startsWith(names(ADAE), "TMP_SMQ") |
    startsWith(names(ADAE), "TMP_CQ")]

## Define code needed to produce the required data sets
ADAE_code <- paste(
  'ADAE <- synthetic_cdisc_data("latest")$adae',
  "add_event_flags <- function(dat) {",
  "  dat %>%",
  "    dplyr::mutate(",
  '      TMPFL_SER = AESER == "Y",',
  '      TMPFL_REL = AEREL == "Y",',
  '      TMPFL_GR5 = AETOXGR == "5",',
  "      TMP_SMQ01 = !is.na(SMQ01NAM),",
  "      TMP_SMQ02 = !is.na(SMQ02NAM),",
  "      TMP_CQ01 = !is.na(CQ01NAM)",
  "    ) %>%",
  "    teal.data::col_relabel(",
  '      TMPFL_SER = "Serious AE",',
  '      TMPFL_REL = "Related AE",',
  '      TMPFL_GR5 = "Grade 5 AE",',
  "      TMP_SMQ01 = aesi_label(dat$SMQ01NAM, dat$SMQ01SC),",
  "      TMP_SMQ02 = aesi_label(dat$SMQ02NAM, dat$SMQ02SC),",
  "      TMP_CQ01 = aesi_label(dat$CQ01NAM)",
  "    )",
  "}",
  "ADAE <- ADAE %>%",
  "  add_event_flags()",
  sep = "\n",
  collapse = "\n"
)

ADAETTE_code <-
  paste(
    'ADAETTE <- synthetic_cdisc_data("latest")$adaette',
    "ADAETTE <- ADAETTE %>%",
    "  mutate(is_event = case_when(",
    '    grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ TRUE,',
    "    TRUE ~ CNSR == 0",
    "  )) %>%",
    "  mutate(n_events = case_when(",
    '    grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ as.integer(.data$AVAL),',
    "    TRUE ~ as.integer(is_event)",
    "  )) %>%",
    '  teal.data::col_relabel(is_event = "Is an Event") %>%',
    '  teal.data::col_relabel(n_events = "Number of Events")',
    "ADAETTE_AE <-",
    '  filter(ADAETTE, grepl("TOT", .data$PARAMCD, fixed = TRUE)) %>% select(-"AVAL")',
    "ADAETTE_OTH <-",
    '  filter(ADAETTE, !(grepl("TOT", .data$PARAMCD, fixed = TRUE)))',
    "",
    "ADAETTE_TTE <- ADAETTE %>%",
    '  filter(PARAMCD == "AEREPTTE") %>%',
    "  select(USUBJID, ARM, ARMCD, AVAL)",
    "",
    "ADAETTE_AE <-",
    '  full_join(ADAETTE_AE, ADAETTE_TTE, by = c("USUBJID", "ARM", "ARMCD"))',
    "ADAETTE <- rbind(ADAETTE_AE, ADAETTE_OTH)",
    sep = "\n",
    collapse = "\n"
  )

ADEX_code <- paste(
  'ADEX <- synthetic_cdisc_data("latest")$adex',
  "ADEX_labels <- teal.data::col_labels(ADEX, fill = FALSE)",
  'set.seed(1, kind = "Mersenne-Twister")',
  "ADEX <- ADEX %>%",
  "  distinct(USUBJID, .keep_all = TRUE) %>%",
  "  mutate(",
  '    PARAMCD = "TDURD",',
  '    PARAM = "Overall duration (days)",',
  "    AVAL = sample(",
  "      x = seq(1, 200),",
  "      size = n(),",
  "      replace = TRUE",
  "    ),",
  '    AVALU = "Days",',
  '    PARCAT1 = "OVERALL"',
  "  ) %>%",
  "  bind_rows(ADEX)",
  "ADEX <- ADEX %>%",
  '  filter(PARCAT1 == "OVERALL" &',
  '    PARAMCD %in% c("TDOSE", "TNDOSE", "TDURD"))',
  "teal.data::col_labels(ADEX) <- ADEX_labels",
  sep = "\n",
  collapse = "\n"
)

ADVS_code <-
  paste(
    'ADVS <- synthetic_cdisc_data("latest")$advs %>%',
    '  mutate(ONTRTFL = ifelse(AVISIT %in% c("SCREENING", "BASELINE"), "", "Y")) %>%',
    '  teal.data::col_relabel(ONTRTFL = "On Treatment Record Flag") %>%',
    "  mutate(ANRIND = as.character(ANRIND), BNRIND = as.character(BNRIND)) %>%",
    "  mutate(",
    "    ANRIND = case_when(",
    '      ANRIND == "HIGH HIGH" ~ "HIGH",',
    '      ANRIND == "LOW LOW" ~ "LOW",',
    "      TRUE ~ ANRIND",
    "    ),",
    "    BNRIND = case_when(",
    '      BNRIND == "HIGH HIGH" ~ "HIGH",',
    '      BNRIND == "LOW LOW" ~ "LOW",',
    "      TRUE ~ BNRIND",
    "    )",
    "  )",
    sep = "\n",
    collapse = "\n"
  )

## Setup App
app <- teal::init(
  data = cdisc_data(
    cdisc_dataset(
      "ADSL",
      ADSL,
      code = 'ADSL <- synthetic_cdisc_data("latest")$adsl',
      vars = list(char_vars_adsl = char_vars_adsl)
    ),
    cdisc_dataset("ADAE", ADAE, code = ADAE_code),
    cdisc_dataset("ADAETTE", ADAETTE, code = ADAETTE_code),
    cdisc_dataset("ADEX", ADEX, code = ADEX_code),
    cdisc_dataset("ADLB", ADLB, code = "ADLB <- synthetic_cdisc_data(\"latest\")$adlb"),
    cdisc_dataset("ADEG", ADEG, code = "ADEG <- synthetic_cdisc_data(\"latest\")$adeg"),
    cdisc_dataset("ADVS", ADVS, code = ADVS_code),
    cdisc_dataset("ADCM", ADCM, code = 'ADCM <- synthetic_cdisc_data("latest")$adcm %>% mutate(CMSEQ = as.integer(CMSEQ))'),
    # code = get_code("app.R")),
    check = TRUE
  ),
  modules = modules(
    tm_front_page(
      label = "App Info",
      header_text = c("Info about input data source" = "This app uses CDISC ADaM datasets randomly generated by `scda` & `scda.2022` R packages"),
      tables = list(`NEST packages used` = data.frame(
        Packages = c(
          "teal.modules.general",
          "teal.modules.clinical",
          "scda",
          "scda.2022"
        )
      ))
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = cs_arm_var,
      summarize_vars = choices_selected(
        choices = variable_choices(ADSL, demog_vars_adsl),
        selected = c("SEX", "AGE", "RACE")
      )
    ),
    modules(
      label = "Adverse Events",
      tm_t_events_summary(
        label = "AE Summary",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        flag_var_anl = choices_selected(
          choices = variable_choices("ADAE", ae_anl_vars),
          selected = ae_anl_vars,
          keep_order = TRUE
        ),
        flag_var_aesi = choices_selected(
          choices = variable_choices("ADAE", aesi_vars),
          selected = aesi_vars,
          keep_order = TRUE
        ),
        add_total = TRUE
      ),
      tm_t_events(
        label = "AE by Term",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        llt = choices_selected(
          choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
          selected = c("AEDECOD")
        ),
        hlt = choices_selected(
          choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
          selected = "AEBODSYS"
        ),
        add_total = TRUE,
        event_type = "adverse event"
      ),
      tm_t_events_by_grade(
        label = "AE Table by Grade",
        dataname = "ADAE",
        arm_var = cs_arm_var,
        llt = choices_selected(
          choices = variable_choices(ADAE, c("AEDECOD")),
          selected = c("AEDECOD")
        ),
        hlt = choices_selected(
          choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
          selected = "AEBODSYS"
        ),
        grade = choices_selected(
          choices = variable_choices(ADAE, c("AETOXGR")),
          selected = "AETOXGR"
        ),
        add_total = TRUE
      ),
      tm_t_events_patyear(
        label = "AE Rates Adjusted for Patient-Years at Risk",
        dataname = "ADAETTE",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADAETTE, "PARAMCD", "PARAM"),
          selected = "AETTE1"
        ),
        events_var = choices_selected(
          choices = variable_choices(ADAETTE, "n_events"),
          selected = "n_events",
          fixed = TRUE
        )
      ),
      tm_t_smq(
        label = "Adverse Events by SMQ Table",
        dataname = "ADAE",
        arm_var = choices_selected(
          choices = variable_choices(ADSL, subset = c(arm_vars, "SEX")),
          selected = "ACTARM"
        ),
        add_total = FALSE,
        baskets = choices_selected(
          choices = variable_choices(ADAE, subset = grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)),
          selected = grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
        ),
        scopes = choices_selected(
          choices = variable_choices(ADAE, subset = grep("^SMQ.*SC$", names(ADAE), value = TRUE)),
          selected = grep("^SMQ.*SC$", names(ADAE), value = TRUE),
          fixed = TRUE
        ),
        llt = choices_selected(
          choices = variable_choices(ADAE, subset = c("AEDECOD")),
          selected = "AEDECOD"
        )
      )
    ),
    modules(
      label = "Lab Tables",
      tm_t_summary_by(
        label = "Labs Summary",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        by_vars = choices_selected(
          choices = variable_choices(ADLB, c("PARAM", "AVISIT")),
          selected = c("PARAM", "AVISIT"),
          fixed = TRUE
        ),
        summarize_vars = choices_selected(
          choices = variable_choices(ADLB, c("AVAL", "CHG")),
          selected = c("AVAL")
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        )
      ),
      tm_t_shift_by_grade(
        label = "Grade Laboratory Abnormality Table",
        dataname = "ADLB",
        arm_var = cs_arm_var,
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = "ALT"
        ),
        worst_flag_var = choices_selected(
          choices = variable_choices(ADLB, subset = c(
            "WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"
          )),
          selected = c("WGRLOVFL")
        ),
        worst_flag_indicator = choices_selected(
          value_choices(ADLB, "WGRLOVFL"),
          selected = "Y",
          fixed = TRUE
        ),
        anl_toxgrade_var = choices_selected(
          choices = variable_choices(ADLB, subset = c("ATOXGR")),
          selected = c("ATOXGR"),
          fixed = TRUE
        ),
        base_toxgrade_var = choices_selected(
          choices = variable_choices(ADLB, subset = c("BTOXGR")),
          selected = c("BTOXGR"),
          fixed = TRUE
        ),
        add_total = FALSE
      ),
      tm_t_abnormality_by_worst_grade(
        label = "Laboratory test results with highest grade post-baseline",
        dataname = "ADLB",
        arm_var = choices_selected(
          choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
          selected = "ARM"
        ),
        paramcd = choices_selected(
          choices = value_choices(ADLB, "PARAMCD", "PARAM"),
          selected = c("ALT", "CRP", "IGA")
        ),
        add_total = FALSE
      )
    ),
    modules(
      label = "Exposure",
      tm_t_summary_by(
        label = "Exposure Summary",
        dataname = "ADEX",
        arm_var = cs_arm_var,
        by_vars = choices_selected(
          choices = variable_choices(ADEX, c("PARCAT2", "PARAM")),
          selected = c("PARCAT2", "PARAM"),
          fixed = TRUE
        ),
        summarize_vars = choices_selected(
          choices = variable_choices(ADEX, "AVAL"),
          selected = c("AVAL"),
          fixed = TRUE
        ),
        paramcd = choices_selected(
          choices = value_choices(ADEX, "PARAMCD", "PARAM"),
          selected = "TDOSE"
        ),
        denominator = choices_selected(
          choices = c("n", "N", "omit"),
          selected = "n"
        )
      ),
      tm_t_exposure(
        label = "Duration of Exposure Table",
        dataname = "ADEX",
        paramcd = choices_selected(
          choices = value_choices(ADEX, "PARAMCD", "PARAM"),
          selected = "TDURD",
          fixed = TRUE
        ),
        col_by_var = choices_selected(
          choices = variable_choices(ADEX, subset = c(arm_vars, "SEX")),
          selected = "SEX"
        ),
        row_by_var = choices_selected(
          choices = variable_choices(
            ADEX,
            subset = c("RACE", "REGION1", "STRATA1", "SEX")
          ),
          selected = "RACE"
        ),
        parcat = choices_selected(
          choices = value_choices(ADEX, "PARCAT2"),
          selected = "Drug A"
        ),
        add_total = FALSE
      )
    ),
    tm_t_abnormality(
      label = "Vital Signs Abnormality",
      dataname = "ADVS",
      arm_var = cs_arm_var,
      id_var = choices_selected(
        choices = variable_choices(ADSL, subset = "USUBJID"),
        selected = "USUBJID",
        fixed = TRUE
      ),
      by_vars = choices_selected(
        choices = variable_choices(ADVS, subset = c("PARAM", "AVISIT")),
        selected = c("PARAM"),
        keep_order = TRUE
      ),
      grade = choices_selected(
        choices = variable_choices(ADVS, subset = "ANRIND"),
        selected = "ANRIND",
        fixed = TRUE
      ),
      abnormal = list(low = "LOW", high = "HIGH")
    ),
    tm_t_mult_events(
      label = "Concomitant Medication",
      dataname = "ADCM",
      arm_var = cs_arm_var,
      seq_var = choices_selected("CMSEQ", selected = "CMSEQ", fixed = TRUE),
      hlt = choices_selected(
        choices = variable_choices(ADCM, c(
          "ATC1", "ATC2", "ATC3", "ATC4"
        )),
        selected = "ATC2"
      ),
      llt = choices_selected(
        choices = variable_choices(ADCM, c("CMDECOD")),
        selected = "CMDECOD",
        fixed = TRUE
      ),
      add_total = TRUE,
      event_type = "treatment"
    ),
    tm_t_shift_by_arm(
      label = "ECG Shift Table by Arm",
      dataname = "ADEG",
      arm_var = cs_arm_var,
      paramcd = choices_selected(value_choices(ADEG, "PARAMCD"),
        selected = "HR"
      ),
      visit_var = choices_selected(value_choices(ADEG, "AVISIT"),
        selected = "POST-BASELINE MINIMUM"
      ),
      aval_var = choices_selected(
        variable_choices(ADEG, subset = "ANRIND"),
        selected = "ANRIND",
        fixed = TRUE
      ),
      base_var = choices_selected(
        variable_choices(ADEG, subset = "BNRIND"),
        selected = "BNRIND",
        fixed = TRUE
      )
    ),
    tm_g_lineplot(
      label = "Line Plot",
      dataname = "ADLB",
      strata = cs_arm_var,
      x = choices_selected(variable_choices(ADLB, "AVISIT"), "AVISIT", fixed = TRUE),
      y = choices_selected(variable_choices(ADLB, c(
        "AVAL", "BASE", "CHG", "PCHG"
      )), "AVAL"),
      y_unit = choices_selected(variable_choices(ADLB, "AVALU"), "AVALU", fixed = TRUE),
      paramcd = choices_selected(variable_choices(ADLB, "PARAMCD"), "PARAMCD", fixed = TRUE),
      param = choices_selected(value_choices(ADLB, "PARAMCD", "PARAM"), "ALT"),
      plot_height = c(1000L, 200L, 4000L)
    )
  ),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
    tags$span(
      style = "font-size: 30px;",
      "Example teal app focusing on safety analysis of clinical trial data with teal.modules.clinical"
    ),
    tags$span(
      style = "display: flex; align-items: center;",
      tags$img(src = "nest.png", alt = "NEST logo", height = "45px"),
      tags$span(style = "font-size: 24px;", "NEST @ Roche")
    )
  ),
  # Set initial filter state as safety-evaluable population
  filter = teal_slices(
    teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
    teal_slice(dataname = "ADLB", varname = "AVAL"),
    # default filter
    teal_slice(dataname = "ADEX", varname = "AVAL"),
    # default filter
    teal_slice(dataname = "ADEG", varname = "AVAL")
  ),
  footer = tags$p(
    actionLink("showAboutModal", "About,"),
    tags$a(
      href = "https://github.com/insightsengineering/teal.gallery/tree/main/safety",
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
