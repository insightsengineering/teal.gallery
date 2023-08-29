library(teal.modules.hermes)
library(teal.modules.general)
library(scda.2022)
library(nestcolor)

options(shiny.useragg = FALSE)

# code>
mae <- hermes::multi_assay_experiment
mae_data <- dataset("MAE", mae)

adtte <- scda::synthetic_cdisc_data("rcd_2022_06_27")$adtte %>%
  dplyr::mutate(is_event = CNSR == 0)

data <- teal_data(
  dataset(
    "ADTTE",
    adtte,
    code = 'adtte <- scda::synthetic_cdisc_data("rcd_2022_06_27")$adtte %>%
      dplyr::mutate(is_event = CNSR == 0)'
  ),
  dataset("MAE", mae)
)

app <- init(
  data = data,
  modules = modules(
    tm_front_page(
      label = "App Info",
      header_text = c("Study Information" = "Random data are used that has been created with the 'scda' and 'hermes' R packages."),
    ),
    tm_g_quality(
      label = "Quality Control",
      mae_name = "MAE"
    ),
    tm_g_pca(
      label = "PCA plot",
      mae_name = "MAE"
    ),
    teal.modules.hermes::tm_g_scatterplot(
      label = "Scatterplot",
      mae_name = "MAE"
    ),
    tm_g_boxplot(
      label = "Boxplot",
      mae_name = "MAE"
    ),
    tm_g_barplot(
      label = "Barplot",
      mae_name = "MAE"
    ),
    tm_g_volcanoplot(
      label = "Volcanoplot",
      mae_name = "MAE"
    ),
    tm_g_forest_tte(
      label = "Forestplot",
      adtte_name = "ADTTE",
      mae_name = "MAE"
    ),
    tm_g_km(
      label = "Kaplan-Meier",
      adtte_name = "ADTTE",
      mae_name = "MAE"
    )
  ),
  header = tags$span(
    style = "display: flex; align-items: center; justify-content: space-between;",
    tags$h2("Example teal app focusing on analysis of RNA-seq data with teal.modules.hermes"),
    tags$span(
      style = "display: flex; align-items: center;",
      tags$img(src = "nest.png", alt = "NEST logo", height = "60px"),
      tags$h3("NEST @ Roche")
    )
  ),
  footer = tags$p(class = "text-muted", "Source: teal.gallery package")
)
## Not run:
shinyApp(app$ui, app$server)
