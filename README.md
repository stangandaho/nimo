  [![R-CMD-check](https://github.com/stangandaho/nimo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stangandaho/nimo/actions/workflows/R-CMD-check.yaml)
  [![Code coverage](https://codecov.io/gh/stangandaho/nimo/graph/badge.svg?token=ehmZyEafyI)](https://codecov.io/gh/stangandaho/nimo)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10495310.svg)](https://doi.org/10.5281/zenodo.10495310)


# **`{nimo}`** in brief
**`nimo`** provides a user-friendly graphical interface that enables users to define complete or partial modeling procedures, including variables, records, algorithms, ensemble methods, and algorithm tuning.  This comprehensive package empowers users to explore, customize, and model their data effectively, with the added benefit of accessing real-world biodiversity data. They can intuitively control the input parameters, explore different modeling options, and observe the corresponding outputs in real-time. This visual and interactive experience enhances the usability and accessibility allowing researchers to focus on their modeling tasks without worrying about complex programming or technical details. It allows users to easily query occurrence data through Global Biodiversity Information Facility [(GBIF)](https://www.gbif.org/occurrence/search) and incorporate it into their modeling workflow. Based on [**`flexsdm`**](https://github.com/sjevelazco/flexsdm/) features and powered by [**`shiny`**](https://github.com/rstudio/shiny), **`nimo`** offers users the flexibility to manipulate and parameterize models. 

# Install
**`nimo`** is not yet on CRAN. The development version can be installed from [github](https://github.com/stangandaho/nimo). 


``` r
# Install remotes if it is not already installed

if (!require("remotes", character.only = TRUE)) {
  install.packages("remotes")
}

# For Windows and Mac OS operating systems
remotes::install_github("stangandaho/nimo")

# For Linux operating system
remotes::install_github("stangandaho/nimo@HEAD")
```

# Get started
Once **nimo** is installed, load the package and call `nimo()` function.
```r
library(nimo)
nimo()
```
The **nimo** [website](https://nimo.re-agro.org) provides to users the resources to get started. 
All necessaries concept and part of **nimo** are explained with screenshot and example. 

# To do 

## General
* [x] Stylize User Interface
* [x] Package documentation
* [ ] Add tooltip to Pre-modeling, modeling and Post modeling input (globally not started)

## Access data through GBIF

| Task                                   | Progress (1-5) | Status         |
|----------------------------------------|----------------|--------------- |
| Design UI                             | 5              | Completed  ✅ |
| Query data using vector files          | 5              | Completed  ✅ |
| Query data drawing an area             | 5              | Completed  ✅ |
| Get and save data citation             | 5              | Completed  ✅ |
| Documentation                          | 4              | Completed  ✅  |

## Pre-modeling

| Task                                   | Progress (1-5) | Status                                                                                |
|----------------------------------------|----------------|---------------------------------------------------------------------------------------|
| Pre-modeling user interface            | 5              | Completed  ✅ |
| Implement [(functions)](https://sjevelazco.github.io/flexsdm/articles/v01_pre_modeling.html) to prepare modeling input| 5 | Completed  ✅ |
| Documentation                          | 5              | Completed  ✅     |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |

## Modeling

| Task                                   | Progress (1-5) | Status                                                                                         |
|----------------------------------------|----------------|----------------------------------------------------------------------------------------------- |
| Pre-modeling user interface            | 5              | Completed  ✅ |
| Construct and validate models with default hyper-parameter [(model fitting)](https://sjevelazco.github.io/flexsdm/articles/v02_modeling.html)| 5 | Completed  ✅ |
|  Model tuning by searching for the best combination of hyper-parameter| 5 | Completed ✅|
| Documentation                          | 5              | Completed  ✅     |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |

## Post-modeling

| Task                                   | Progress (1-5) | Status                                                                                         |
|----------------------------------------|----------------|----------------------------------------------------------------------------------------------- |
| Post-modeling user interface            | 5              | Completed  ✅ |
| Functionalities for [(model prediction, evaluation and correction)](https://sjevelazco.github.io/flexsdm/articles/v03_post_modeling.html)| 5 | Completed  ✅ |
| Documentation                          | 5              | Completed  ✅     |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |
