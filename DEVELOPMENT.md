# Development instructions

Install R `devtools`, `roxygen2` and `altdoc`. Install Quatro.
Run:

    devtools::document()
    render_docs()

When it gives errors, use:

    render_docs(verbose=TRUE)