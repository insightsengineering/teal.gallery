# teal.gallery

<img align="right" width="100" height="100" src="https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/thumbs/teal.png">

A gallery of sample apps based on the [`teal` framework](https://github.com/insightsengineering/teal).

## Links to apps on `shinyapps.io`

### Development version

Preview the [Teal Gallery apps](https://insightsengineering.github.io/teal.gallery/main/articles/demo.html)

- [RNA-seq](https://genentech.shinyapps.io/NEST_RNA-seq_main/)
- [basic-teal](https://genentech.shinyapps.io/NEST_basic-teal_main/)
- [efficacy](https://genentech.shinyapps.io/NEST_efficacy_main/)
- [exploratory](https://genentech.shinyapps.io/NEST_exploratory_main/)
- [longitudinal](https://genentech.shinyapps.io/NEST_longitudinal_main/)
- [early-dev](https://genentech.shinyapps.io/NEST_early-dev_main/)
- [patient-profile](https://genentech.shinyapps.io/NEST_patient-profile_main/)
- [python](https://genentech.shinyapps.io/NEST_python_main/)
- [safety](https://genentech.shinyapps.io/NEST_safety_main/)

## Usage

### Installation (optional)

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```R
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.gallery")
```

Note that this does not install the R package dependencies of individual teal gallery apps. Individual app dependencies are managed by [renv](https://rstudio.github.io/renv/) within their [own app directory](https://github.com/insightsengineering/teal.gallery/tree/main/inst/apps).

### Listing Apps

You can list all available apps by running:

```R
teal.gallery::list_apps()
```

### Running an app locally

#### Method 1: Using `shiny::runGitHub` (no need to install `teal.gallery`)

Note that in order for this method to work, you would have to make sure that all the required packages for the app are installed.

Launch an app by running the shiny::runGitHub by replacing the `subdir = "inst/apps/<APP_NAME>"`

```R
# Say you want to run the `basic-teal` app
shiny::runGitHub(
  username = "insightsengineering",
  repo = "teal.gallery",
  subdir = "inst/apps/basic-teal"
)
```

#### Method 2: Using the `teal.gallery` package

You can run the sample app using the `teal.gallery::launch_app` function. This automatically restores the packages by calling `renv::restore()`

```R
teal.gallery::launch_app("basic-teal")
```

#### Method 3: By downloading the contents of the `teal.gallery` GitHub repository

It is recommended that you clone the teal.gallery repository using git. Alternatively, you can download the contents as zip and unzip them.

```sh
git clone https://github.com/insightsengineering/teal.gallery.git
```

Now you can open any RStudio project for your preferred sample app inside the `inst/apps` directory and run it just like how you would run any normal shiny app.

If promted by renv to run `renv::restore()` run it to ensure that the R package dependencies are installed.

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
