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

| Stable version                                                                 | Dev version                                                                   |
| ------------------------------------------------------------------------------ | ----------------------------------------------------------------------------- |
| [basic-teal](https://genentech.shinyapps.io/NEST_basic-teal_stable/)           | [basic-teal](https://genentech.shinyapps.io/NEST_basic-teal_dev/)             |
| [delayed-data](https://genentech.shinyapps.io/NEST_delayed-data)               | [delayed-data](https://genentech.shinyapps.io/NEST_delayed-data_dev/)         |
| [custom-transform](https://genentech.shinyapps.io/NEST_custom-transform)       | [custom-transform](https://genentech.shinyapps.io/NEST_custom-transform_dev/) |
| [exploratory](https://genentech.shinyapps.io/NEST_exploratory_stable/)         | [exploratory](https://genentech.shinyapps.io/NEST_exploratory_dev/)           |
| [safety](https://genentech.shinyapps.io/NEST_safety_stable/)                   | [safety](https://genentech.shinyapps.io/NEST_safety_dev/)                     |
| [efficacy](https://genentech.shinyapps.io/NEST_efficacy_stable/)               | [efficacy](https://genentech.shinyapps.io/NEST_efficacy_dev/)                 |
| [patient-profile](https://genentech.shinyapps.io/NEST_patient-profile_stable/) | [patient-profile](https://genentech.shinyapps.io/NEST_patient-profile_dev/)   |
| [early-dev](https://genentech.shinyapps.io/NEST_early-dev_stable/)             | [early-dev](https://genentech.shinyapps.io/NEST_early-dev_dev/)               |
| [longitudinal](https://genentech.shinyapps.io/NEST_longitudinal_stable/)       | [longitudinal](https://genentech.shinyapps.io/NEST_longitudinal_dev/)         |
| [RNA-seq](https://genentech.shinyapps.io/NEST_RNA-seq_stable/)                 | [RNA-seq](https://genentech.shinyapps.io/NEST_RNA-seq_dev/)                   |
| [python](https://genentech.shinyapps.io/NEST_python_stable/)                   | [python](https://genentech.shinyapps.io/NEST_python_dev/)                     |

## Running the apps

You can run any of these apps by just executing these two lines of code in your R console.

By sourcing the [sourceme.R](https://github.com/insightsengineering/teal.gallery/blob/main/utils/sourceme.R) file you make sure that you have access to the `restore_and_run()`

Note: Make sure to install [renv](https://rstudio.github.io/renv/index.html) before you do this. Also, it is recommended that you create and use a Github PAT. Once you have the PAT, run the following:

```R
Sys.setenv(GITHUB_PAT = "your_access_token_here")
```

Running the `restore_and_run("APP_NAME")` will run the `APP_NAME` by restoring the packages using [renv](https://rstudio.github.io/renv/)

```R
source("https://raw.github.com/insightsengineering/teal.gallery/main/_internal/utils/sourceme.R")

# Run the basic-teal app
restore_and_run("basic-teal")
```

## Development

All `teal` sample apps are wrapped into this repository into it's own sub-directory. All development standards and practices that we currently use for teal app development also apply to this repository.

### Auto-deployment

All the sample apps are automatically deployed every day using the CI in two channels:

- `stable` channel: The code for the Teal apps is taken from the `main` branch of `teal.gallery`, and the NEST packages are installed from the `main` branch of GitHub. This is done using the `deploy_stable.yaml`.
- `dev` channel: The code for the Teal apps is taken from the `dev` branch of `teal.gallery` and the NEST packages are installed from the last `release tag` of GitHub. This is done using the `deploy_dev.yaml`.

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
