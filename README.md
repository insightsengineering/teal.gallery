# teal.gallery

<img align="right" width="100" height="100" src="https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/thumbs/teal.png">

A gallery of sample apps based on the [`teal` framework](https://github.com/insightsengineering/teal).

## Links to apps on `shinyapps.io`

### Development version

- [basic-teal](https://genentech.shinyapps.io/NEST_basic-teal_stable/)
- [exploratory](https://genentech.shinyapps.io/NEST_exploratory_stable/)
- [safety](https://genentech.shinyapps.io/NEST_safety_stable/)
- [efficacy](https://genentech.shinyapps.io/NEST_efficacy_stable/)
- [patient-profile](https://genentech.shinyapps.io/NEST_patient-profile_stable/)
- [early-dev](https://genentech.shinyapps.io/NEST_early-dev_stable/)
- [longitudinal](https://genentech.shinyapps.io/NEST_longitudinal_stable/)
- [RNA-seq](https://genentech.shinyapps.io/NEST_RNA-seq_stable/)
- [python](https://genentech.shinyapps.io/NEST_python_stable/)

## Running the apps

You can run any of these apps by just executing these two lines of code in your R console.

By sourcing the [\sourceme.R](https://github.com/insightsengineering/teal.gallery/blob/main/utils/sourceme.R) file you make sure that you have access to the `restore_and_run()`

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

All `teal` sample apps are wrapped into this package for the sake of portability. All development standards and practices that we currently use for teal app development also apply to this repository.

### Adding a sample app to `teal.gallery`

Adding a sample app involves the following steps:

#### Hard requirements

1. Copy each sample app into a file named `app.R` into a sensible folder name inside it's own directory. The directory has to be named with the `APP_NAME`.
2. Update the `_internal/quarto/demo-apps.yml` with a new `app/title`. This should be the `APP_NAME`.
3. Run the `_internal/utils/generate_app_readme.R` to generate the readme for the app inside it's own directory.

#### Optional/Soft requirements

1. Create a GIF recording ([KAP](https://getkap.co/) is a good tool for this). Make sure that the dimensions of the GIF is 970x555 px and the size is about 1 MB. (It can be done by recording using KAP in 1470x840 px and rendering 5fps and downsizing 33%). Place the GIF inside the `_internal/quarto/assets/img` direcxtory. Also, make sure that the name of the GIF is `APP_NAME.gif`
2. Add front-end tests with the help of cypress. Copy the contents of the `js` directory within some other app's directory inside your app directory to get the node dependencies. Place the cypress tests inside the `tests/cypress` inside your app's directory. Please refer to an existing app's tests so that the `.github/deploy.yaml` will automaticall run the cypress tests.
