# Acutelines Datatoolbox R Package

<!-- badges: start -->
[![GitHub R package version](https://img.shields.io/github/r-package/v/UMCG-SEH/acutelines.datatoolbox?logo=R)](https://github.com/UMCG-SEH/acutelines.datatoolbox/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Full documenation is available at [umcg-seh.github.io/acutelines.datatoolbox](https://umcg-seh.github.io/acutelines.datatoolbox/). Visit [GitHub](https://github.com/UMCG-SEH/acutelines.datatoolbox) for source code.

## Introduction
This package can be used to work with data extracted from the [Acutelines](https://umcgresearch.org/w/acutelines) data- and biobank. It provides tools to parse data extracts and perform calculations in a uniform matter.

Acutelines is a multi-disciplinary prospective hospital-based cohort study examining 24/7 the complete acute patient journey admitted to the ED of the University Medical Centre Groningen (UMCG), a tertiary care teaching hospital in the Netherlands. It employs a broad range of investigative procedures in assessing the pre-hospital, in-hospital, and long-term health factors that affect outcome in patients with acute conditions. The cohort population is broadly representative of the people living in the Northern Netherlands with acute medical conditions. Detailed information about the cohort and participant selection can be found elsewhere [1] [2]. Participants were asked for written informed consent, when applicable by proxy. The Acutelines cohort study is approved by the medical ethics committee of the UMCG, the Netherlands and registered under trial registration number NCT04615065 at ClinicalTrials.gov [3]. 

1. [www.acutelines.nl](https://www.acutelines.nl)
2. ter Avest E, van Munster BC, van Wijk RJ*, et al* Cohort profile of Acutelines: a large data/biobank of acute and emergency medicine. _BMJ Open_ 2021;**11:**e047349. doi: [10.1136/bmjopen-2020-047349](https://doi.org/10.1136/bmjopen-2020-047349)
3. https://clinicaltrials.gov/study/NCT04615065

## Installation
To install a package from Github, first install and load the `devtools` packages:

``` R
install.packages('devtools')
library(devtools)
```

Install the `acutelines.datatoolbox` package from GitHub:

``` R
install_github("UMCG-SEH/acutelines.datatoolbox")
```

Load the package:

``` R
library(acutelines.datatoolbox)
```

### Install specific version
If you want to install a specific version of the package for legacy purposes, mention the version number with `@` in the `install_github` command:

``` R
install_github("UMCG-SEH/acutelines.datatoolbox@v0.0.0.6")
```

After that load and use the package as you normally do.

## Getting started
After installation, you can process the data depending on your data extract. Refer to the Articles section in the [documentation](https://umcg-seh.github.io/acutelines.datatoolbox/) for a general introduction.

Start by importing your dataset from CSV, for example:

``` R
df <- read.csv("path/to/data/file.csv",
               header = TRUE, # Import CSV header (first line) as column names
               sep = ";", # Set CSV columns separator
               dec = ".", # Set CSV decimal separator for numbers
               fileEncoding = "UTF-8-BOM", # Set file encoding format, change or ommit according to file type
               na.strings=c("NA","NaN", " ", "\\N", "NULL")) # Set which values are considered NA
```

Then run the desired functions from the Acutelines Datatoolbox, eg: `sofa_magic_wrapper()`.