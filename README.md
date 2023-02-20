# teal.gallery

<img align="right" width="100" height="100" src="https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/thumbs/teal.png">

A gallery of sample apps based on the [Teal framework](https://github.com/insightsengineering/teal).

## Usage

### Installation

After you've cloned this repository locally, simply run the following commands in an R session with your current working directory set to the base of the cloned repository.

Feel free to check out any branch after you've cloned this repository. Dependencies will be automatically installed in the steps provided below.

```R
# Set your Github and Gitlab PATs
# You might have already set these. If not, do so here.
Sys.setenv("GITHUB_PAT" = "<token>")
Sys.setenv("GITLAB_PAT" = "<token>")

# Install staged.dependencies
remotes::install_github(
  "openpharma/staged.dependencies",
  upgrade = "never"
)

# Set token mappings
options(
  staged.dependencies.token_mapping = c(
    "https://github.com" = "GITHUB_PAT",
    "https://gitlab.com" = "GITLAB_PAT"
  )
)

# Install deps and this project
staged.dependencies::install_deps(
  staged.dependencies::dependency_table()
)
```

### Listing Apps

You can list all available apps by running:

```R
teal.gallery::list_apps()
```

### Running an app

Launch an app by running:

```R
# Say you want to run the `basic-teal` app
teal.gallery::launch_app("basic-teal")
```

### Deployments

To deploy a specific app to [shinyapps.io](https://shinyapps.io) to an internally hosted [Posit Connect](https://posit.co/products/enterprise/connect/) server, run:

```R
# Assuming you want to deploy the basic-teal app
teal.gallery:::deploy_app(
  app_name = "basic-teal",
  app_title = "Basic Teal App - TEST",
  vanity_url = "/NEST/basic-teal-test",
  api_key = "SHINYAPPS_OR_POSIT_CONNECT_API_TOKEN",
  api_secret = "SHINYAPPS_OR_POSIT_CONNECT_API_SECRET" # N/A for Posit Connect
)
```

To deploy all apps from this package to [shinyapps.io](https://shinyapps.io) to an internally hosted [Posit Connect](https://posit.co/products/enterprise/connect/), run:

```R
teal.gallery:::deploy_all_apps(
    api_key = "SHINYAPPS_OR_POSIT_CONNECT_API_TOKEN"
    api_secret = "SHINYAPPS_OR_POSIT_CONNECT_API_SECRET" # N/A for Posit Connect
)
```

## Development

All `teal` sample apps are wrapped into this package for the sake of portability. All development standards and practices that we currently use for R package development also apply to this repository.

### Adding a sample app to `teal.gallery`

Adding a sample app involves the following steps:

1. Copy each sample app into a file named `app.R` into a sensible folder name inside the [`inst/apps`](inst/apps) directory
1. Update the first test in the package to list all the app names (i.e. populate the `expected_apps` vector with the new app name)
1. Add into the package *Suggests* in the [`DESCRIPTION`](DESCRIPTION) file any dependencies that were explicitly used in `app.R` for the sample app. To do so, invoke `renv::dependencies("path_to_file")`) to see a list of dependencies.
1. Update the upstream dependencies of the `staged_dependencies.yaml` file of this package and the downstream dependencies of the packages that were added in the previous step.

### Links to teal apps on shinyapps.io

#### Main (development)

- [RNA-seq](https://genentech.shinyapps.io/NEST_RNA-seq_main/)
- [basic-teal](https://genentech.shinyapps.io/NEST_basic-teal_main/)
- [efficacy](https://genentech.shinyapps.io/NEST_efficacy_main/)
- [exploratory](https://genentech.shinyapps.io/NEST_exploratory_main/)
- [longitudinal](https://genentech.shinyapps.io/NEST_longitudinal_main/)
- [early-dev](https://genentech.shinyapps.io/NEST_early-dev_main/)
- [patient-profile](https://genentech.shinyapps.io/NEST_patient-profile_main/)
- [python](https://genentech.shinyapps.io/NEST_python_main/)
- [safety](https://genentech.shinyapps.io/NEST_safety_main/)
