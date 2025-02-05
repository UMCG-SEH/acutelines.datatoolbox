# Development instructions

Install R `devtools`, `roxygen2` and `pkgdown`:

    install.packages(c("devtools", "roxygen2", "pkgdown"))

Run local developed changes:

    devtools::load_all() # or devtools::install() for installing the package

Parse documentation:

    devtools::document()
    pkgdown::build_site()

# References
1. https://pkgdown.r-lib.org/