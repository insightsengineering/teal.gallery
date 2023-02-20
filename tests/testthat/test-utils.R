testthat::test_that("package environment is setup as expected", {
  expected_apps <- c(
    "basic-teal",
    "efficacy",
    "exploratory",
    "longitudinal",
    "patient-profile",
    "early-dev",
    "python",
    "RNA-seq",
    "safety"
  )
  testthat::expect_equal(sort(list_apps()), sort(expected_apps))
})

# ---- get_app_code ----
testthat::test_that("get_app_code throws error if app_name is not string", {
  testthat::expect_error(get_app_code(3), "Assertion on 'app_name' failed")
  testthat::expect_error(get_app_code(c("a", "b")), "Assertion on 'app_name' failed")
})

testthat::test_that("get_app_code throws error if no app exists", {
  testthat::expect_error(get_app_code("not_an_app"), "Cannot find app 'not_an_app'")
})

testthat::test_that("get_app_code throws error if app.R file does not exist (or cannot be read)", {
  temp_location <- tempfile()
  dir.create(temp_location)
  on.exit(unlink(temp_location))

  mockery::stub(get_app_code, "get_app_dir", function(app_name) temp_location)
  testthat::expect_error(get_app_code("app"), "app.R for application app does not exist in location")

  writeBin(1:10, con = file.path(temp_location, "app.R"))
  testthat::expect_error(get_app_code("app"), "Unable to read file")
})


# ---- launch_app----
testthat::test_that("launch_app throws error if app_name is not a string", {
  testthat::expect_error(launch_app(3), "Assertion on 'app_name' failed")
  testthat::expect_error(launch_app(c("a", "b")), "Assertion on 'app_name' failed")
})

testthat::test_that("launch_app throws error if app directory does not exist", {
  testthat::expect_error(launch_app("not_an_app"), "Cannot find app 'not_an_app'")
})

testthat::test_that("launch_app throws error if app dependencies do not exist", {
  mockery::stub(
    launch_app, "get_app_dependencies",
    function(app_name) c("teal.gallery", "not_a_real_package")
  )

  testthat::expect_error(
    launch_app("basic-teal"),
    paste(
      "Unable to run app as package not_a_real_package is not installed.",
      "Please ensure the following packages are installed: teal.gallery, not_a_real_package"
    )
  )
})


testthat::test_that("launch_app launches the app if dependencies exist", {
  # basic-teal app includes only teal which is an import of teal.gallery
  mockery::stub(launch_app, "shiny::runApp", function(...) print("launching application"))

  testthat::expect_equal(
    testthat::capture_output(launch_app("basic-teal")),
    "Finding R package dependencies ... Done!\n[1] \"launching application\""
  )
})
