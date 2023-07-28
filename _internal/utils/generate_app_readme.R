# Run this file to generate the readme inside each app directory
library(yaml)

app_names <- unlist(lapply(yaml::read_yaml("_internal/quarto/demo-apps.yml"), function(x) x$apps))

for (app_name in app_names) {
  rmarkdown::render(
    "_internal/utils/app_readme_template.Rmd",
    output_dir = app_name,
    output_file = "README.md",
    knit_root_dir = paste0("../../", app_name),
    params = list(app_name = app_name)
  )
}
