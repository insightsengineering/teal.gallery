options(shiny.sanitize.errors = FALSE)

# attach required packages
library(DescTools)
library(magrittr)
library(dplyr)
library(scda)
library(scda.2022)
library(stringr)
library(teal.goshawk)
library(teal.modules.clinical)
library(teal.modules.general)
# optional libraries
library(sparkline)

options(shiny.useragg = FALSE)

# study information: used to label app and in "Example Info Page" module
ATYPE <- "Exploratory" # define here and in the "# code >" section below
TA <- "I2ON"
MOLECULE <- "Demo Molecule"
INDICATION <- "Demo Indication"
STUDY <- "Demo Study"
lead_spa_name <- "John Doe"
lead_spa_uid <- "John_Doe"
app_owner_name <- "John Doe"
app_owner_uid <- "John_Doe"

# code>

# expected data are ADSL and ADLB
# other data can be included but needs to be added to ADSL or ADLB as appropriate
# depending on if the data are SUBJECT LEVEL ANALYSIS DATASET or BASIC DATA STRUCTURE
# if for example outcome variables are added to ADSL then do also add them as columns to ADLB
# this is needed to be able to use plot splitting by outcome functionality
ADSL <- synthetic_cdisc_data("latest")$adsl
ADLB <- synthetic_cdisc_data("latest")$adlb

################################################################################
# BEGIN: SPA Input Required To Modify Values Below To Reflect Study Specifics
################################################################################

# assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
shape_manual <- c(
  "N" = 1,
  "Y" = 2,
  "NA" = 0
)

# study specific biomarkers of interest to exclude from performing log2 calculation done below
# for example if biomarker is already log2 transformed then exclude
# value of AVAL will then be assigned to XXXXXL2 variables. e.g. AVALL2
# e.g. if x assay is already log2 transformed then assigns AVAL to AVALL2
exclude_l2 <- c("")

# for example if study specific biomarker represents a CHG value then exclude
# value NA is assigned to XXXXXL2. e.g. AVALL2
# e.g. if x assay are CHG values then assigns NA to AVALL2
exclude_chg <- c("")

# the app expects ARM to be the main treatment variable
# to use other treatment variables it is easiest to rename to ARM and process as ARM

# convenience function: simply reduces typing effort especially if there are a lot of treatment arms
# run once to create treatment mapping code for assigning arm_mapping variable below
# this function writes the code framework to the console
# copy code lines from console and provide right hand side values as desired
# maptrt(df_arm = ADSL$TRT01P, code = "M")

# ARM mapping: used to map dose values as ARM values or to abbreviate long treatment values for improved display
# "original ARM value" = "displayed treatment value".
# the customized values may be the same value as original if this functionality isn't needed
# but the arm_mapping still needs to be defined
arm_mapping <- list(
  "A: Drug X" = "Drug X 100mg",
  "C: Combination" = "Combination 100mg",
  "B: Placebo" = "Placebo"
)

# for consistency: assign treatment colors to match those used in other outputs. left hand side values need to
# match values on right hand side of arm_mapping done above
color_manual <- c(
  "Drug X 100mg" = "#1e90ff",
  "Combination 100mg" = "#bb9990",
  "Placebo" = "#ffa07a"
)

# convenience function: run once to create treatment ordering code for TRTORD variable below.
# this function writes the code framework to the console
# copy code lines from console and provide right hand side values as desired.
# maptrt(df_arm = ADSL$TRT01P, code = "O")

# convenience operator: create an operator that keeps the variable attributes
# variable labels often get clobbered and we would like to keep them)
`%keep_label%` <- function(lhv, rhv) {
  attributes(lhv) <- attributes(rhv)
  lhv
}

# convenience operator: create an operator that adds a label to newly created or re-processed variables
`%make_label%` <- function(lhv, label) {
  attr(lhv, "label") <- label
  lhv
}

# post process the ADSL data to subset records per specification
ADSL <- ADSL %>%
  filter(ITTFL == "Y") %>%
  # use TRT01P values to order treatment in visualization legend and accompanying summary tables
  mutate(
    TRTORD = case_when(
      TRT01P == "A: Drug X" ~ 1,
      TRT01P == "C: Combination" ~ 2,
      TRT01P == "B: Placebo" ~ 3,
      TRUE ~ as.numeric(NA)
    ),
    TRTORD = TRTORD %make_label% "Treatment Order",
    TRT01P = as.character(arm_mapping[match(TRT01P, names(arm_mapping))]),
    TRT01P = factor(ARM) %>% reorder(TRTORD),
    TRT01P = TRT01P %make_label% "Planned Treatment for Period 01"
  )

# capture state of variable labels to apply back onto variables after data filtering
adsl_labels <- teal.data::col_labels(ADSL)
date_vars_adsl <- names(ADSL)[vapply(ADSL, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
char_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.character)))

# set all character class variables to class factor required by NEST functions
ADSL <- ADSL %>%
  mutate_at(char_vars_adsl, factor)

# add labels back onto variables
var_labels(ADSL) <- c(adsl_labels)

# post process the ADLB data to subset records per specification
# goshawk expects the following variables: AVISITCD, AVALU, BASE2, CHG2, LBSTRESC, LOQFL, PCHG2
# AVISITCD is assigned abbreviated values of AVISIT. this allows for more efficient use of plot real estate for the plot itself
#   if this is not helpful then simply AVISITCD <- AVISIT
# AVISITCDN is assigned intuitive numeric values from AVISITCD. if this is not helpful then simply AVISITCDN <- AVISITN
# BASE2 represents screening visit value. if this is not relevant for your study then BASE2 <- NA
# CHG2 represents change from screening visit value. if this is not relevant for your study then CHG2 <- NA
# PCHG2 represents % change from screening visit value. if this is not relevant for your study then PCHG2 <- NA
# if you would like to include screening visit related "change from ..." analysis
# variables in the analysis variable menu then do the following:
#   create BASE2 and assign it AVAL values at screening visit
#   create CHG2 and assign it change from screening values
#   create PCHG2 and assign it % change from screening values
# LBSTRESC is carried forward from SDTM and used for deriving an AVAL value and assigning the LOQFL value
#  - expectation is that LBSTRESC LOQ values are "< x" or "> y" format.
#  - value assigned to AVAL is x/2 and y respectively. LOQFL is assigned "Y"
#  - other LBSTRESC value formats will need to be pre-processed so that AVAL and LOQFL are assigned as desired
# LOQFL represents limit of quantitation flag. if this is not relevant for your study then LOQFL <- NA
# goshawk expectation is to only keep "SCREENING", "BASELINE", "DAY xx", "WEEK yy", "FOLLOW zz" type AVISIT values
# other values can be adjusted for as well of course but this is study specific pre-processing that needs to be done

# adjust select based on your study specifics

set.seed(1, kind = "Mersenne-Twister") # Reproducible code due to `sample` calls

ADLB_SUBSET <- ADLB %>%
  filter(!is.na(AVAL)) %>%
  filter(ITTFL == "Y" & toupper(AVISIT) %like any% c("SCREEN%", "BASE%", "%WEEK%", "%FOLLOW%")) %>%
  select(c(
    "STUDYID", "USUBJID",
    "ITTFL",
    "ARM", "ARMCD", "ACTARM", "ACTARMCD", "TRT01P", "TRT01A",
    "AVISIT", "AVISITN", "ADY",
    "PARAM", "PARAMCD",
    "AVAL", "AVALU", "BASE", "CHG", "PCHG",
    "ANRLO", "ANRHI",
    "LBSTRESC",
    "SEX", "RACE",
    "LOQFL"
  )) %>%
  mutate(
    # adjust substr to work with study specific values
    AVISITCD = case_when(
      toupper(AVISIT) == "SCREENING" ~ "SCR",
      toupper(AVISIT) == "BASELINE" ~ "BL",
      grepl("WEEK", toupper(AVISIT)) ~ paste("W", trimws(substr(AVISIT, start = 6, stop = stringr::str_locate(AVISIT, "DAY") - 1))),
      grepl("FOLLOW", toupper(AVISIT)) ~ "FU",
      TRUE ~ as.character(NA)
    ),
    AVISITCDN = case_when(
      AVISITCD == "SCR" ~ -2,
      AVISITCD == "BL" ~ 0,
      grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]+", "", AVISITCD)) * 7,
      AVISITCD == "FU" ~ 100,
      TRUE ~ as.numeric(NA)
    ),
    TRTORD = case_when(
      TRT01P == "A: Drug X" ~ 1,
      TRT01P == "C: Combination" ~ 2,
      TRT01P == "B: Placebo" ~ 3,
      TRUE ~ as.numeric(NA)
    ),
    LOQFL = if_else(as.character(LOQFL) == "Y", as.character(LOQFL), "N"), # need explicit "N" value for LOQFL
    BASE2 = NA,
    CHG2 = NA,
    PCHG2 = NA
  ) %>%
  rowwise() %>%
  group_by(PARAMCD) %>%
  # only used for producing LOQ values in synthetic data not for study use
  mutate(LBSTRESC = ifelse(
    USUBJID %in% sample(USUBJID, 1, replace = TRUE),
    paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
  )) %>%
  mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
    paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
  )) %>%
  ungroup()

attr(ADLB_SUBSET[["LBSTRESC"]], "label") <- "Character Result/Finding in Std Format"
attr(ADLB_SUBSET[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
attr(ADLB_SUBSET[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"

# combined treatment and lineplot statistics overlay color
color_comb <- "#39ff14"

# line and spaghetti plot x-axis tic and label controls. identify key visits to display at ticks
# numeric comes from the AVISITCDN variable which controls chronological order
x_tick_num <- c(-2, 0, 7, 14, 21, 28, 35)
# label values are assigned here and can be customized or AVISITCD values
x_tick_label <- c("Screening", "Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5")

################################################################################
# END: SPA Input Required
################################################################################

################################################################################
# BEGIN: Generic Data Post Processing
################################################################################

# identify the minimum non-zero value for AVAL for each PARAMCD.
# non-zero minimum value used for log2 transformed analysis values
PARAM_MINS <- ADLB_SUBSET %>%
  select(USUBJID, PARAMCD, AVAL) %>%
  group_by(PARAMCD) %>%
  summarise(AVAL_MIN = min(AVAL, na.rm = TRUE), .groups = "drop") %>%
  mutate(PARAMCD = PARAMCD %make_label% "Parameter Code")

# post process the data to create several new variables and adjust existing record specific values per specification
# - adjust existing BASELINE record values where values are missing
ADLB_SUPED1 <- ADLB_SUBSET %>%
  mutate(BASE2 = ifelse(toupper(AVISIT) == "SCREENING" & is.na(BASE2), AVAL, BASE2) %keep_label% BASE2) %>%
  mutate(CHG2 = ifelse(toupper(AVISIT) == "SCREENING" & is.na(CHG2), 0, CHG2) %keep_label% CHG2) %>%
  mutate(PCHG2 = ifelse(toupper(AVISIT) == "SCREENING" & is.na(PCHG2), 0, PCHG2) %keep_label% PCHG2) %>%
  mutate(BASE = ifelse(toupper(AVISIT) == "BASELINE" & is.na(BASE), AVAL, BASE) %keep_label% BASE) %>%
  mutate(CHG = ifelse(toupper(AVISIT) == "BASELINE" & is.na(CHG), 0, CHG) %keep_label% CHG) %>%
  mutate(PCHG = ifelse(toupper(AVISIT) == "BASELINE" & is.na(PCHG), 0, PCHG) %keep_label% PCHG) %>%
  mutate(TRTORD = TRTORD %make_label% "Treatment Order")

# Inconsequential Warning issued: Warning: "PARAMCD" has different attributes on LHS and RHS of join.
# merge minimum AVAL value onto the ADLB data to calculate the log2 variables. preserve the variable order
ADLB_SUPED2 <- inner_join(PARAM_MINS, ADLB_SUPED1, by = "PARAMCD")[, union(names(ADLB_SUPED1), names(PARAM_MINS))] %>%
  # visit values
  # excludes biomarkers where log2 is not appropriate: for example assay value already log2
  mutate(AVALL2 = ifelse(PARAMCD %in% exclude_l2, AVAL,
    # excludes biomarkers where log2 is not appropriate: for example CHG type assay
    ifelse(PARAMCD %in% exclude_chg, NA,
      ifelse(AVAL == 0 & AVAL_MIN > 0, log2(AVAL_MIN / 2),
        # would be taking log2 of 0 or negative value so set to NA
        ifelse(AVAL == 0 & AVAL_MIN <= 0, NA,
          ifelse(AVAL > 0, log2(AVAL), NA)
        )
      )
    )
  ) %make_label% "Log2 of AVAL") %>%
  # baseline values
  mutate(BASEL2 = ifelse(PARAMCD %in% exclude_l2, BASE,
    ifelse(PARAMCD %in% exclude_chg, NA,
      ifelse(BASE == 0 & AVAL_MIN > 0, log2(AVAL_MIN / 2),
        ifelse(BASE == 0 & AVAL_MIN <= 0, NA,
          ifelse(BASE > 0, log2(BASE), NA)
        )
      )
    )
  ) %make_label% "Log2 of BASE") %>%
  # screening
  mutate(BASE2L2 = ifelse(PARAMCD %in% exclude_l2, BASE2,
    ifelse(PARAMCD %in% exclude_chg, NA,
      ifelse(BASE2 == 0 & AVAL_MIN > 0, log2(AVAL_MIN / 2),
        ifelse(BASE2 == 0 & AVAL_MIN <= 0, NA,
          ifelse(BASE2 > 0, log2(BASE2), NA)
        )
      )
    )
  ) %make_label% "Log2 of BASE2") %>%
  mutate(AVAL_MIN = AVAL_MIN %make_label% "Minimum AVAL Within PARAMCD")

# create final data set used by goshawk
# all data set passed into a goshawk app must have all of the columns
# `AVISITCD`, `BASE`, `BASE2`, `AVALU`, `LBSTRESC`, `LOQFL`, `CHG2`, and `PCHG2`.
ADLB <- ADLB_SUPED2 %>%
  mutate(
    TRT01P = as.character(arm_mapping[match(TRT01P, names(arm_mapping))]),
    TRT01P = factor(TRT01P) %>% reorder(TRTORD) %make_label% "Planned Treatment for Period 01",
    TRT01A = as.character(arm_mapping[match(TRT01A, names(arm_mapping))]),
    TRT01A = factor(TRT01A) %>% reorder(TRTORD) %make_label% "Actual Treatment for Period 01",
    LOQFL = LOQFL %make_label% "Limit of Quantification",
    AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN) %make_label% "Analysis Visit Window Code",
    AVISITCDN = AVISITCDN %make_label% "Analysis Visit Window Code (N)",
    BASE2 = BASE2 %make_label% "Screening Value",
    CHG2 = CHG2 %make_label% "Absolute Change from Screening",
    PCHG2 = PCHG2 %make_label% "Percent Change from Screening"
  )

# add LLOQ and ULOQ variables to support adding horizontal/vertical range lines
# LBSTRESC is carried forward from SDTM and used for deriving an AVAL value and assigning the LOQFL value
#  - expectation is that LBSTRESC LOQ values are "< x" or "> y" format.
#  - value assigned to AVAL is x/2 and y respectively. LOQFL is assigned "Y"
#  - other LBSTRESC value formats will need to be pre-pocessed so that AVAL and LOQFL are assigned as desired
# LBSTRESC is required in order to use the h_identify_loq_values function. if this is not available then LBSTRESC <- AVAL
ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")

################################################################################
# END: Generic Data Post Processing
################################################################################

# <code

# USER INTERFACE CONFIGURATIONS: match with available analysis variables to display them in pull down menus
arm_vars <- c("TRT01A", "TRT01P", "SEX", "RACE")

demog_vars_asl <- function(data) {
  date_vars_asl <- names(data)[vapply(data, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
  names(data)[!(names(data) %in% c("USUBJID", "STUDYID", date_vars_asl))]
}

anl_vars1 <- c("AVAL", "BASE", "CHG", "PCHG", "AVALL2")
anl_vars2 <- c("AVAL", "CHG", "PCHG", "AVALL2")
box_xaxis_vars <- c("TRT01A", "TRT01P", "AVISITCD", "STUDYID")
dm_vars <- c("SEX", "AGE", "RACE")

# response variable menu controls. match with available response variables in ADLB
box_facet_vars <- c("TRT01A", "TRT01P", "AVISITCD")
line_splits <- c("SEX")
cs_params <- choices_selected(choices = value_choices("ADLB", "PARAMCD", "PARAM"), selected = "ALT")

cs_arm_vars <- choices_selected(
  choices = variable_choices("ADSL", subset = arm_vars),
  selected = "TRT01A"
)

cs_anl_vars1_1 <- choices_selected(
  choices = variable_choices("ADLB", subset = anl_vars1),
  selected = "BASE"
)

cs_anl_vars1_2 <- choices_selected(
  choices = variable_choices("ADLB", subset = anl_vars1),
  selected = "AVAL"
)

cs_anl_vars2 <- choices_selected(
  choices = variable_choices("ADLB", subset = anl_vars2),
  selected = "AVAL"
)

cs_dm_vars <- choices_selected(
  choices = variable_choices("ADSL", demog_vars_asl),
  selected = dm_vars
)

cs_box_facet_vars <- choices_selected(
  choices = variable_choices("ADLB", subset = box_facet_vars),
  selected = "AVISITCD"
)

cs_box_xaxis_vars <- choices_selected(
  choices = variable_choices("ADLB", subset = box_xaxis_vars),
  selected = "TRT01P"
)

cs_line_splits <- choices_selected(
  choices = variable_choices("ADSL", subset = line_splits),
  selected = NULL
)

# create ADSL metadata for Example Info Page tab
adsl_source <- "Randomly generated CDISC data"
adsl_time <- "Current"

sdsl_label <- c("Subject Level Analysis Data Set")

# get number of subjects
sdsl_nsubjs <- ADSL %>%
  pull(USUBJID) %>%
  unique()

# create ADLB metadata for Example Info Page tab
adlb_source <- "Randomly generated CDISC data"
adlb_time <- "Current"

sdlb_label <- c("Laboratory/Biomarker Analysis Data Set")

# get number of subjects
sdlb_nsubjs <- ADLB %>%
  pull(USUBJID) %>%
  unique()

# create biomarkers of interest dictionary table to display to user
paramDict <- unique(ADLB[c("PARAM", "PARAMCD")])
paramDict <- paramDict[order(paramDict$PARAM), ]
param_list <- paramDict$PARAM
paramcd_list <- paramDict$PARAMCD

# create list of biomarkers excluded from log2 transformation
paramexcldDict <- paramDict %>%
  filter(PARAMCD %in% c(exclude_l2, exclude_chg))
paramexcld_list <- paramexcldDict$PARAM
paramcdexcld_list <- paramexcldDict$PARAMCD

app <- teal::init(
  data = cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    cdisc_dataset("ADLB", ADLB),
    code = get_code("app.R",
      exclude_comments = TRUE,
      read_sources = TRUE
    ),
    check = FALSE
  ),
  filter = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SEX"),
    teal_slice(dataname = "ADSL", varname = "AGE")
  ),
  modules = modules(
    tm_front_page(
      label = "App Info",
      header_text = c("Info about input data source" = "This app uses CDISC ADaM datasets randomly generated by `scda` & `scda.2022` R packages"),
      tables = list(`NEST packages used in this demo app` = data.frame(
        Packages = c("teal.modules.clinical", "teal.modules.general", "teal.goshawk", "goshawk", "scda", "scda.2022")
      ))
    ),
    module(
      "User Guide",
      server = function(input, output, session, datasets) {},
      ui = function(id, ...) {
        div(
          h5(
            strong("Analysis Variable Legend"),
            actionLink("showAnlVarLegendModal", label = NULL, icon = icon("fas fa-question-circle"), title = "Show help")
          ),
          h5(
            strong("Key Variables:")
          ),
          p(tags$ul(
            h6(
              "The 'Description of Planned Arm' (ARM/TRT01P) variable is used to reflect ITT treatment for these longitudinal
              visualizations. Alternative treatment variables and populations can be used as appropriate."
            ),
            h6(
              "The 'Parameter Code' (PARAMCD) variable is used to select biomarker/lab of interest.
              The 'Example Info Page' tab provides the 'Parameter Description' (PARAM). The biomarker/lab selection pull down
              menu items are a concatenation of PARAMCD with PARAM for ease of identification."
            ),
            h6(
              "Pull down menus containing many items include search functionality to ease finding menu items."
            ),
            h6(
              "The 'Analysis Visit' (AVISIT) variable is used to display visit as an abbreviated analysis visit value."
            )
          )),
          h5(
            strong("Data Constraint Filter:")
          ),
          p(tags$ul(
            h6(
              "Selecting the Screening constraint will remove subjects who do not satisfy the filter range based on
            their screening value for the given assay."
            ),
            h6(
              "Selecting the Baseline constraint will remove subjects who do not satisfy the filter range based on
            their baseline value for the given assay."
            ),
            h6(
              "Let's say subject #58 has a baseline value of 5 for assay x and the range of assay x across all
            subjects is 1 to 10. If the the baseline constraint is selected and the value range slider is changed
            to a range of 7 to 10 then subjects who do not meet that condition are removed from the longitudinal
            visualizations. Since subject #58 baseline value is 5 for assay x they are one of the subjects who is
            removed."
            )
          )),
          h5(
            strong("Data Point Brushing:")
          ),
          p(tags$ul(
            h6(
              "Selecting specific data points to reveal Subject ID and other data are available for Box, Correlation and
              Spaghetti Plot visualizations."
            )
          )),
          h5(
            strong("Log2 Variables:")
          ),
          p(tags$ul(
            h6(
              "Some biomarker/lab values are already log2 transformed or represent a change value. These are excluded from
              log2 transform applied to biomarkers/labs at large.",
              do.call(
                # call shiny tagList
                shiny::tagList,
                # with a list of shiny::p elements (one for each element) by mapping a
                # '<p> builder' function over a list of values
                Map(function(paramexcld_list, paramcdexcld_list) {
                  shiny::p(shiny::tags$ul(h6(paste0(paramexcld_list, " ,"), paramcdexcld_list)))
                }, paramcdexcld_list = paramcdexcld_list, paramexcld_list = paramexcld_list)
              ),
              br(),
              p("Biomarkers/labs having a value of 0 are log2 transformed by taking log2 of the minimum non-zero value for
                that biomarker/lab, divided by 2.")
            )
          )),
          h5(
            strong("Visualization Tab: Right Hand Data Filter Panel:")
          ),
          p(tags$ul(
            h6(
              "These filters are not hierarchical so should not be used to filter on analysis variables. To filter on
                analysis variables please use the filtering controls available in the left hand panel.",
              style = ("color: #ff5700;")
            ),
            h6(
              "Use right hand data panel filters to filter on categorical variables."
            ),
            tags$ul(
              h6("AVISIT to exclude/include specific visits in visualizations."),
              h6("LOQFL to exclude/include LOQ flagged values."),
              h6("SEX to exclude/include specific gender."),
              h6("Etc.")
            )
          )),
          h5(
            strong("Visualization Specifics:")
          ),
          p(tags$ul(
            h6(
              strong("Box Plot:")
            )
          )),
          p(tags$ul(tags$ul(
            h6("Selecting STUDYID as X-Axis variable will produce the visualization with all subjects combined and identify the study on the X-Axis.")
          ))),
          p(tags$ul(
            h6(
              strong("Correlation Plot:")
            )
          )),
          p(tags$ul(tags$ul(
            h6("The data constraint that can be placed on Screening or Baseline records is associated with the analysis
               variable and Biomarker selected for the X-Axis only."),
          ))),
          p(tags$ul(tags$ul(
            h6("The Limit of Quantification (LOQFL) flag is set if either of the biomarker/lab values is identified as
             LOQ. The brushing table column header reflects this as LOQFL_COMB."),
          ))),
          p(tags$ul(tags$ul(
            h6("The 'Regression Line' option should only be used in conjunction with the 'Treatment Facetting' option.
                 Otherwise the per treatment regression formula and coefficient annotations will overlay.")
          ))),
          p(tags$ul(
            h6(
              strong("Line Plot:")
            )
          )),
          p(tags$ul(tags$ul(
            h6("If an error is displayed related to plot height it's best to first alter the relative plot height in the left panel using the slider. For additional plot height control, use the icons in the upper right corner of the visualization."),
          ))),
          h5(
            strong("Copying Content:")
          ),
          p(tags$ul(
            h6(
              "To copy data from the descriptive summary table, highlight, copy and paste special into Excel using
              'Unicode Text'."
            )
          )),
          p(tags$ul(
            h6(
              "To copy a visualization, right click
              options."
            )
          ))
        )
      },
      datanames = NULL
    ),
    module(
      "Example Info Page",
      server = function(input, output, session, datasets) {},
      ui = function(id, ...) {
        div(
          h5(strong("Molecule:"), MOLECULE),
          h5(strong("Indication:"), INDICATION),
          h5(strong("Study:"), STUDY),
          h5(strong("Analysis Type:"), ATYPE),
          tags$ul(
            h6(
              p(strong("Subject Level Data Set:"), adsl_source),
              p(strong("Data Set Label:"), sdsl_label),
              p(strong("Data Set Owner:"), a(lead_spa_name,
                href = paste0("https://en.wikipedia.org/wiki/", lead_spa_uid),
                title = "Click here for detailed user contact",
                target = "blank"
              )),
              p(strong("Data Set Creation DateTime:"), adsl_time),
              p(strong("Number of  Subjects:"), format(length(sdsl_nsubjs), big.mark = ",")),
              p(strong("Number of Variables:"), format(length(names(ADSL)), big.mark = ",")),
              p(strong("Number of Records:"), format(nrow(ADSL), big.mark = ",")),
              br(),
              p(strong("Biomarker Data Set (Identified as ADLB in app):"), adlb_source),
              p(strong("Data Set Label:"), sdlb_label),
              p(strong("Data Set Owner:"), a(lead_spa_name,
                href = paste0("https://en.wikipedia.org/wiki/", lead_spa_uid),
                title = "Click here for detailed user contact",
                target = "blank"
              )),
              p(strong("Data Set Creation DateTime:"), adlb_time),
              p(strong("Number of  Subjects:"), format(length(sdlb_nsubjs), big.mark = ",")),
              p(strong("Number of Variables Kept for Visualizations:"), format(length(names(ADLB)), big.mark = ",")),
              p(strong("Number of Records for Biomarkers of Interest:"), format(nrow(ADLB), big.mark = ",")),
              p(strong("Number of Biomarkers of Interest:"), format(length(table(ADLB$PARAMCD)), big.mark = ",")),
              br(),
              p(strong("Biomarkers of interest (label, code):")),
              do.call(
                # call shiny tagList
                shiny::tagList,
                # with a list of shiny::p elements (one for each letter) by mapping a
                # '<p> builder' function over a list of values
                Map(function(param_list, paramcd_list) {
                  shiny::p(shiny::tags$ul(h6(paste0(param_list, " ,"), paramcd_list)))
                }, paramcd_list = paramcd_list, param_list = param_list)
              )
            )
          )
        )
      },
      datanames = NULL
    ),
    tm_variable_browser(label = "Variable Browser"),
    tm_data_table(label = "Data Table"),
    tm_t_summary(
      label = "Demographics",
      dataname = "ADSL",
      arm_var = cs_arm_vars,
      summarize_vars = cs_dm_vars
    ),
    modules(
      label = "Visualizations",
      tm_g_gh_boxplot(
        label = "Box Plot",
        dataname = "ADLB",
        param_var = "PARAMCD",
        param = cs_params,
        facet_var = cs_box_facet_vars,
        xaxis_var = cs_box_xaxis_vars,
        yaxis_var = cs_anl_vars2,
        plot_height = c(500, 200, 2000),
        trt_group = cs_arm_vars,
        color_manual = color_manual,
        shape_manual = shape_manual,
        rotate_xlab = TRUE,
        hline_arb = c(10, 30),
        hline_arb_color = c("grey", "red"),
        hline_arb_label = c("default_hori_A", "default_hori_B"),
        hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
        hline_vars_colors = c("pink", "brown", "purple", "gray")
      ),
      tm_g_gh_correlationplot(
        label = "Correlation Plot",
        dataname = "ADLB",
        param_var = "PARAMCD",
        xaxis_param = cs_params,
        xaxis_var = cs_anl_vars1_1,
        yaxis_param = cs_params,
        yaxis_var = cs_anl_vars1_2,
        trt_group = cs_arm_vars,
        color_manual = color_manual,
        shape_manual = shape_manual,
        plot_height = c(500, 200, 2000),
        facet_ncol = 2,
        trt_facet = FALSE,
        reg_line = FALSE,
        rotate_xlab = TRUE,
        font_size = c(12, 8, 20),
        dot_size = c(1, 1, 12),
        reg_text_size = c(3, 3, 10),
        hline_arb = c(10, 30),
        hline_arb_label = "arb hori label",
        hline_arb_color = c("red", "blue"),
        hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
        hline_vars_colors = c("pink", "brown", "purple", "gray"),
        hline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
        vline_arb = c(50, 70),
        vline_arb_label = "arb vert A",
        vline_arb_color = c("green", "orange"),
        vline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
        vline_vars_colors = c("yellow", "orange", "brown", "gold"),
        vline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label")
      ),
      tm_g_gh_density_distribution_plot(
        label = "Density Distribution Plot",
        dataname = "ADLB",
        param_var = "PARAMCD",
        param = cs_params,
        xaxis_var = cs_anl_vars2,
        trt_group = cs_arm_vars,
        color_manual = color_manual,
        color_comb = color_comb,
        rotate_xlab = TRUE,
        plot_height = c(500, 200, 2000),
        font_size = c(12, 8, 20),
        line_size = c(1, 1, 12),
        hline_arb = c(.02, .05),
        hline_arb_color = c("red", "black"),
        hline_arb_label = c("Horizontal Line A", "Horizontal Line B")
      ),
      tm_g_gh_lineplot(
        label = "Line Plot",
        dataname = "ADLB",
        param_var = "PARAMCD",
        param = cs_params,
        shape_choices = cs_line_splits,
        xaxis_var = choices_selected(variable_choices("ADLB", "AVISITCDN"), "AVISITCDN"),
        yaxis_var = cs_anl_vars2,
        trt_group = cs_arm_vars,
        color_manual = color_manual,
        rotate_xlab = TRUE,
        plot_height = c(600, 200, 2000),
        xtick = x_tick_num,
        xlabel = x_tick_label
      ),
      tm_g_gh_spaghettiplot(
        label = "Spaghetti Plot",
        dataname = "ADLB",
        idvar = "USUBJID",
        param_var = "PARAMCD",
        param = cs_params,
        xaxis_var = choices_selected(variable_choices("ADLB", "AVISITCDN"), "AVISITCDN"),
        yaxis_var = cs_anl_vars2,
        trt_group = cs_arm_vars,
        man_color = color_manual,
        color_comb = color_comb,
        rotate_xlab = TRUE,
        plot_height = c(500, 200, 2000),
        xtick = x_tick_num,
        xlabel = x_tick_label,
        hline_arb = c(10, 30),
        hline_arb_color = c("grey", "red"),
        hline_arb_label = c("default A", "default B"),
        hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
        hline_vars_colors = c("pink", "brown", "purple", "gray")
      )
    )
  ),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
    tags$span(
      style = "font-size: 30px;",
      "Example teal app focusing on analysis of longitudinal clinical trial data with teal.goshawk"
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
      href = "https://github.com/insightsengineering/teal.gallery/tree/main/longitudinal",
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

# Add context sensitive help code
body(app$server)[[length(body(app$server)) + 1]] <- quote(
  observeEvent(input$showAnlVarLegendModal, {
    showModal(modalDialog(
      title = "Analysis Variable Pull Down Menu",
      tags$p(
        "These variables will appear in the Visualizations 'Analysis Variable' pull down menu. This legend
            provides the variable labels to help clarify the short analysis variable names displayed in the pull down
          menu."
      ),
      p("BASE2 = Screening Visit Value"),
      p("BASE2L2 = Log2(BASE2)"),
      p("CHG2 = Change from Screening"),
      p("PCHG2 =  % Change from Screening"),
      p("BASE = Baseline Visit Value"),
      p("BASEL2 = Log2(BASE)"),
      p("CHG = Change from Baseline"),
      p("PCHG =  % Change from Baseline"),
      p("AVAL = Visit Values"),
      p("AVALL2 = Log2(AVAL)"),
      easyClose = TRUE
    ))
  })
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
