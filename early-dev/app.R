## Setup App
app <- teal::init(
  modules = modules(
    module(
      label = "App Information",
      server = srv_front_page,
      ui = ui_front_page,
      filters = "all"
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
  ),
  header = div(
    class = "",
    style = "margin-bottom: 2px;",
    tags$h1("Demo ED Onco teal app with random ADaM data", tags$span("ED SPA", class = "pull-right"))
  ),
  footer = tags$p(class = "text-muted", "Source: teal.gallery package")
)

## Start Teal Shiny App ----
shinyApp(app$ui, app$server)
