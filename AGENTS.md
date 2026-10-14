# teal.gallery Development Guide

`teal.gallery` is a set of `teal` applications that provide examples of ready to use apps exploring clinical trial data.
It requires extensive knowledge of the teal framework from the app developer perspective and github workflows.

The automated workflows allow for the apps' dependencies to be updated and deployed.
It deploys a stable and development apps to a connect server that contain essentially the same application, but with different dependencies.

The application code is written in a simple form to allow users to understand how an app is developed and use it as a base for their own work.

### Direct dependencies

- **teal** - Provides the framework and helpers to operate with teal
  modules.
- **teal.code** - Bare code generation and evaluation ensuring
  reproducibility.
- **teal.data** - Data management and relationships between datasets via
  join_keys. (contains sample data for ADaM datasets and default keys to
  merge ADaM datasets).
- **teal.picks** - Helpers to select, transform and manipulate them.

## Deployment practices

The app deployment should be automated and allow for the update of dependencies in the teal framework to be kept up to date.
It also updates other dependencies to the latest version from CRAN.

The teal packages used differ from `stable` and `dev` deployment channels:

- `stable`: Using latest releases from CRAN and in cases that the package is not released on CRAN, use latest release channel
  - in GitHub it should use `<organization>/<repository>@*release` tag to install from latest GitHub release
- `dev`: Using latest development versions from `teal` framework and relevant packages (such as `rtables`, `mmrm`, `tern`, ...)
  - The relevant packages should be added especially when there is active development that is closely associated with `teal`

When the update process or any of the end-to-end test fail, it should not re-deploy the app.

The CI/CD should be able to run in each Pull Request on GitHub to test every step with the exceptions of deployment and commiting.
If the pull request targets `main` it should run as if it was the `stable` channel.
When targeting `dev` branch it should run as the `dev` channel.

## Testing

There should be end-to-end tests testing the different application.
It should cover:

- application starts without errors.
- application has X number of tabs.
- application can change to all modules without errors.

## Quality Assurance

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`, `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed for `teal_data`/`qenv` data objects with `eval_code()` and `within()`

## Code Review Process

- **Pull Request Reviews**: All changes require human review and approval
- **Automated Checks**: CI must pass before merging

