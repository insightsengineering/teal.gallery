source("renv/activate.R")

for (package in renv::dependencies()$Package) {
    if (!requireNamespace(package)) {
        renv::install(package, prompt = FALSE)
    }
}
