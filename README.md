# Acutelines Datatoolbox R Package
![GitHub R package version](https://img.shields.io/github/r-package/v/UMCG-SEH/acutelines.datatoolbox?logo=R)
![GitHub License](https://img.shields.io/github/license/umcg-seh/acutelines.datatoolbox)

Full documenation is available at [umcg-seh.github.io/acutelines.datatoolbox](https://umcg-seh.github.io/acutelines.datatoolbox/#/). Visit [GitHub](https://github.com/UMCG-SEH/acutelines.datatoolbox) for source code.

## Introduction
This package can be used to work with data extracted from the [Acutelines](https://umcgresearch.org/w/acutelines) data- and biobank. It provides tools to parse data extracts and perform calculations in a uniform matter.

Acutelines is a multi-disciplinary prospective hospital-based cohort study examining 24/7 the complete acute patient journey admitted to the ED of the University Medical Centre Groningen (UMCG), a tertiary care teaching hospital in the Netherlands. It employs a broad range of investigative procedures in assessing the pre-hospital, in-hospital, and long-term health factors that affect outcome in patients with acute conditions. The cohort population is broadly representative of the people living in the Northern Netherlands with acute medical conditions. Detailed information about the cohort and participant selection can be found elsewhere [1] [2]. Participants were asked for written informed consent, when applicable by proxy. The Acutelines cohort study is approved by the medical ethics committee of the UMCG, the Netherlands and registered under trial registration number NCT04615065 at ClinicalTrials.gov [2]. 

1. [www.acutelines.nl](https://www.acutelines.nl)
2. ter Avest E, van Munster BC, van Wijk RJ*, et al* Cohort profile of Acutelines: a large data/biobank of acute and emergency medicine. *BMJ Open* 2021;**11:**e047349. doi: [10.1136/bmjopen-2020-047349](https://doi.org/10.1136/bmjopen-2020-047349)

## Installation
Install the package from GitHub, you need devtools for this.

    library(devtools)
    install_github("UMCG-SEH/acutelines.datatoolbox")

Load the package:

    library(acutelines.datatoolbox)