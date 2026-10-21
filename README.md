# teal.gallery

<a href="https://github.com/insightsengineering/teal" target="_blank">
  <img align="right" width=auto height="100" src="https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.png" style="margin-left:10px;">
</a>
<a href="https://github.com/insightsengineering/teal.gallery" target="_blank">
  <img align="right" width=auto height="100" src="https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/teal.gallery.png">
</a>

A gallery of sample apps based on the [`teal`](https://github.com/insightsengineering/teal) framework.

## Links to apps on `shinyapps.io`

The Stable version of the apps use the latest released packages while the dev version of the apps use the development packages which are installed from the `main` branch of the respective package repository. The specific versions used can be seen in the `Session Info` of the deployed app.

| Description                                                              | Stable version                                        | Dev version                                                                           |
| ----------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| Minimal `teal` app with a simple custom module on the `iris` dataset.                                                               | [basic-teal](https://rinpharma.shinyapps.io/NEST_basic-teal_stable/)                     | [basic-teal](https://rinpharma.shinyapps.io/NEST_basic-teal_dev/)                     |
| Embeds `teal` as modules (`ui_teal()`/`srv_teal()`) inside a regular `shiny` app.                                                   | [teal-as-shiny-module](https://rinpharma.shinyapps.io/NEST_teal-as-shiny-module_stable/) | [teal-as-shiny-module](https://rinpharma.shinyapps.io/NEST_teal-as-shiny-module_dev/) |
| Loads data on demand with `teal_data_module()`, letting the user pick the datasets before the app starts.                           | [delayed-data](https://rinpharma.shinyapps.io/NEST_delayed-data_stable/)                 | [delayed-data](https://rinpharma.shinyapps.io/NEST_delayed-data_dev/)                 |
| Modifies module data with custom `teal_transform_module()` transformators.                                                          | [custom-transform](https://rinpharma.shinyapps.io/NEST_custom-transform_stable/)         | [custom-transform](https://rinpharma.shinyapps.io/NEST_custom-transform_dev/)         |
| General exploratory analysis of ADaM data with `teal.modules.general` (distributions, outliers, associations, regression, ...).     | [exploratory](https://rinpharma.shinyapps.io/NEST_exploratory_stable/)           | [exploratory](https://rinpharma.shinyapps.io/NEST_exploratory_dev/)                   |
| Safety analysis with `teal.modules.clinical` (adverse events, laboratory abnormalities, exposure, shift tables, ...).               | [safety](https://rinpharma.shinyapps.io/NEST_safety_stable/)                     | [safety](https://rinpharma.shinyapps.io/NEST_safety_dev/)                             |
| Efficacy analysis with `teal.modules.clinical` (forest plots, Kaplan-Meier, time-to-event, Cox and logistic regression, MMRM, ...). | [efficacy](https://rinpharma.shinyapps.io/NEST_efficacy_stable/)                 | [efficacy](https://rinpharma.shinyapps.io/NEST_efficacy_dev/)                         |
| Individual patient profiles (basic info, medical history, medications, vitals, adverse events, laboratory, timeline).               | [patient-profile](https://rinpharma.shinyapps.io/NEST_patient-profile_stable/)   | [patient-profile](https://rinpharma.shinyapps.io/NEST_patient-profile_dev/)           |
| Early development analysis with `teal.osprey` (adverse event overviews, butterfly, spider, swimlane and waterfall plots, ...).      | [early-dev](https://rinpharma.shinyapps.io/NEST_early-dev_stable/)               | [early-dev](https://rinpharma.shinyapps.io/NEST_early-dev_dev/)                       |
| Longitudinal biomarker and laboratory analysis with `teal.goshawk` (box, correlation, density, line and spaghetti plots).           | [longitudinal](https://rinpharma.shinyapps.io/NEST_longitudinal_stable/)         | [longitudinal](https://rinpharma.shinyapps.io/NEST_longitudinal_dev/)                 |
| RNA-seq analysis with `teal.modules.hermes` (quality control, PCA, volcano, Kaplan-Meier, ...).                                     | [RNA-seq](https://rinpharma.shinyapps.io/NEST_RNA-seq_stable/)                   | [RNA-seq](https://rinpharma.shinyapps.io/NEST_RNA-seq_dev/)                           |
| Builds a dataset with Python code via `reticulate` and explores it with `teal.modules.general`.                                     | [python](https://rinpharma.shinyapps.io/NEST_python_stable/)                     | [python](https://rinpharma.shinyapps.io/NEST_python_dev/)                             |

## Running the apps

You can run any of these apps by just executing these two lines of code in your R console.

By sourcing the [sourceme.R](https://github.com/insightsengineering/teal.gallery/blob/main/utils/sourceme.R) file you make sure that you have access to the `restore_and_run()`

Note: Make sure to install [renv](https://rstudio.github.io/renv/index.html) before you do this.
Also, it is recommended that you create and use a Github PAT. Once you have the PAT, run the following:

```R
Sys.setenv(GITHUB_PAT = "your_access_token_here")
```

Running the `restore_and_run("APP_NAME")` will run the `APP_NAME` by restoring the packages using [renv](https://rstudio.github.io/renv/)

```R
source("https://raw.githubusercontent.com/insightsengineering/teal.gallery/main/_internal/utils/sourceme.R")

# Run the basic-teal app
restore_and_run("basic-teal")
```

## Development

All `teal` sample apps are wrapped into this repository into it's own sub-directory.
All development standards and practices that we currently use for teal app development also apply to this repository.

### Auto-deployment

All the sample apps are automatically deployed every day using the CI in two channels:

- `stable` channel: The code for the Teal apps is taken from the `main` branch of `teal.gallery`, and the NEST packages are installed from the `release tag` branch of GitHub. This is done using the `deploy_stable.yaml`.
- `dev` channel: The code for the Teal apps is taken from the `dev` branch of `teal.gallery` and the NEST packages are installed from the last `main` of GitHub. This is done using the `deploy_dev.yaml`.

The `deploy.yml` workflow automatically updates `renv.lock` files by:

1. Restoring packages from the current lockfile
2. Updating packages to their latest versions
3. Detecting packages in the framework (teal, osprey, hermes, etc.)
4. Applying the appropriate reference strategy (dev/Github vs. stable/CRAN channel)
5. Perform end-to-end tests
6. Committing changes back to the repository

_IMPORTANT_: Although we can now test the unreleased features of the NEST packages in deployments, currently, the divergent `dev` and `main` branches must be managed and merged manually as needed to ensure that the apps work fine in both deployment channels, i.e., making sure to merge the app changes from `dev` to `main` by creating a PR.

### Adding a sample app to `teal.gallery`

Adding a sample app involves the following steps:

#### Hard requirements

1. Copy each sample app into a file named `app.R` into a sensible folder name inside it's own directory. The directory has to be named with the `APP_NAME`.
2. Update the `_internal/quarto/demo-apps.yml` with a new `app/title`. This should be the `APP_NAME`. Also, edit the `.github/workflows/deploy.yml` with the path to the new app so it can be deployed.
3. Run the `_internal/utils/generate_app_readme.R` to generate the readme for the app inside it's own directory.

#### Optional/Soft requirements

1. You can snapshot the teal app dependencies using `{renv}` but make sure to snapshot using GitHub references to the teal packages.
2. Create a GIF recording ([LICEcap](https://www.cockos.com/licecap/) is a good tool for this). Make sure that the dimensions of the GIF is 970x555 px and the size is about 1 MB. Place the GIF inside the `_internal/quarto/assets/img` directory. Also, make sure that the name of the GIF is `APP_NAME.gif`. Also, make sure to place a static image with the name dimention called `APP_NAME.png` that will be displayed in the demo page when the card is not hovered.
3. Add front-end tests with the help of cypress. Copy the contents of the `js` directory within some other app's directory inside your app directory to get the node dependencies. Place the cypress tests inside the `tests/cypress` inside your app's directory. Please refer to an existing app's tests so that the `.github/deploy.yaml` will automaticall run the cypress tests.

### Dependencies

#### Overview

Each app maintains its dependencies through a `renv.lock` file stored in the app's directory.
This ensures reproducible package installations across development and deployment environments.

**Dependency reference strategies:**
- `<organization>/<repository>` — installs from the development branch (used in `dev` channel)
- CRAN / `<organization>/<repository>@*release` — installs the latest released version (used in `stable` channel)

#### Manual Dependency Updates

To manually update dependencies for an app, use the same R version and environment as the CI workflow.
The recommended approach is to use the Docker image from [`insightsengineering/ci-images`](https://github.com/insightsengineering/ci-images/pkgs/container/rstudio).

**Script to update GitHub-sourced NEST packages:**

```r
# update_renv
deploy_channel <- "stable" # "dev"

yml <- yaml::read_yaml("../.github/workflows/deploy.yml")

step_names <- c(
  "Install required R packages",
  "Install R packages using renv and update the renv snapshot",
  "Commit and push changes 📌"
)
steps <- yml$jobs$deploy$steps[which(sapply(yml$jobs$deploy$steps, function(x) x$name) %in% step_names)]
lapply(steps, function(x) {
  x$run |>
    stringr::str_replace_all("\\$\\{\\{ env.DEPLOY_CHANNEL \\}\\}", deploy_channel) |>
    stringr::str_replace_all("\\$\\{\\{ matrix.directory \\}\\}", getwd()) |>
    str2expression() |>
    eval()
})
```

#### Local Development with containers

Run apps locally using the same environment as CI with Podman/Docker Compose.

**Setup:**

1. Create a `.env` file in the project root:
   ```
   GITHUB_PAT=your_access_token_here
   ```

2. Use the compose file configuration below _(use the appropriate version and user password)_:

```yaml
services:
  rstudio:
    image: ghcr.io/insightsengineering/rstudio:<version>
    ports:
      - "8787:8787"
    volumes:
      # Project directories
      - ./RNA-seq:/workspace/RNA-seq
      - ./basic-teal:/workspace/basic-teal
      - ./custom-transform:/workspace/custom-transform
      - ./delayed-data:/workspace/delayed-data
      - ./early-dev:/workspace/early-dev
      - ./efficacy:/workspace/efficacy
      - ./exploratory:/workspace/exploratory
      - ./longitudinal:/workspace/longitudinal
      - ./patient-profile:/workspace/patient-profile
      - ./python:/workspace/python
      - ./safety:/workspace/safety
      - ./teal-as-shiny-module:/workspace/teal-as-shiny-module
    environment:
      - PASSWORD=<password>
      - GITHUB_PAT=${GITHUB_PAT}
    working_dir: /workspace
```

3. Start the container

4. Access RStudio at `http://localhost:8787` (username: `rstudio`, password: `<password>`)