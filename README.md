
# `{nimo}` in brief
The `{nimo}` is R package seamlessly integrates with the Global Biodiversity Information Facility [(GBIF)](https://www.gbif.org/occurrence/search) occurence data. It allows users to easily query occurrence data for biodiversity research and incorporate it into their modeling analyses. Based on [`{flexsdm}`](https://github.com/sjevelazco/flexsdm/) features and powered by [`{shiny}`](https://github.com/rstudio/shiny), `{nimo}` offers users the flexibility to manipulate and parameterize models according to their unique research needs. It provides a user-friendly graphical interface that enables users to define complete or partial modeling procedures, including variables, records, algorithms, ensemble methods, and algorithm tuning.  This comprehensive package empowers researchers to explore, customize, and model their data effectively, with the added benefit of accessing real-world biodiversity data. They can intuitively control the input parameters, explore different modeling options, and observe the corresponding outputs in real-time. This visual and interactive experience enhances the usability and accessibility allowing researchers to focus on their modeling tasks without worrying about complex programming or technical details.  

# Install
`{nimo}` is not yet on CRAN. The development version can be installed from [github](https://github.com/stangandaho/nimo). 

:warning: 
  NOTE: The version 1.4-22 of **terra** package is causing errors when trying to install **flexsdm** and so **nimo**. Please, first install a version ≥ 1.5-12 of **terra** package available on CRAN or development version of [terra](https://github.com/rspatial/terra) and then **nimo**.

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
* [ ] Package documentation (globally in progress)
* [ ] Add tooltip to Pre-modeling, modeling and Post modeling input (globally mot started)

## GBIF API

| Task                                   | Progress (1-5) | Status         |
|----------------------------------------|----------------|--------------- |
| Designe UI                             | 5              | Completed  ✅ |
| Query data using vector files          | 5              | Completed  ✅ |
| Query data drawing an area             | 5              | Completed  ✅ |
| Get and save data citation             | 5              | Completed  ✅ |
| Documentation                          | 4              | In progess    |

## Pre-modeling

| Task                                   | Progress (1-5) | Status                                                                                |
|----------------------------------------|----------------|---------------------------------------------------------------------------------------|
| Pre-modeling user interface            | 5              | Completed  ✅ |
| Implement [(functions)](https://sjevelazco.github.io/flexsdm/articles/v01_pre_modeling.html) to prepare modeling input| 5 | Completed  ✅ |
| Documentation                          | 4              | In progess    |
| Functionality to request R script of pre-modeling process| 3              | Planning   |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |

## Modeling

| Task                                   | Progress (1-5) | Status                                                                                         |
|----------------------------------------|----------------|----------------------------------------------------------------------------------------------- |
| Pre-modeling user interface            | 5              | Completed  ✅ |
| Construct and validate models with default hyper-parameter [(model fitting)](https://sjevelazco.github.io/flexsdm/articles/v02_modeling.html)| 5 | Completed  ✅ |
|  Model tuning by searching for the best combination of hyper-parameter| 4 | In progess|
| Documentation                          | 4              | In progess    |
| Functionality to request R script of pre-modeling process| 2              | Not started   |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |

## Post-modeling

| Task                                   | Progress (1-5) | Status                                                                                         |
|----------------------------------------|----------------|----------------------------------------------------------------------------------------------- |
| Post-modeling user interface            | 5              | Completed  ✅ |
| Functionalities for [(model prediction, evaluation and correction)](https://sjevelazco.github.io/flexsdm/articles/v03_post_modeling.html)| 5 | Completed  ✅ |
| Documentation                          | 4              | In progess    |
| Functionality to request R script of pre-modeling process| 2              | Not started   |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |
