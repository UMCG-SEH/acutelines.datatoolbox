# Development instructions

Install R `devtools`, `roxygen2`,`pkgdown` and `cffr`:

    install.packages(c("devtools", "roxygen2", "pkgdown", "cffr"))

Run local developed changes:

    devtools::load_all() # or devtools::install() for installing the package

Parse documentation:

    devtools::document()
    pkgdown::build_site()

Increment version:
    usethis::use_version()
    pkgdown::build_site()

Update citation file:

    cffr::cff_write(keys = list("doi" = "10.5281/zenodo.14824082"),  r_citation = TRUE)

# References
1. https://pkgdown.r-lib.org/