[![DOI](https://img.shields.io/badge/DOI-10.1002%2F2688--8319.12385-004c03)](https://doi.org/10.1002/2688-8319.12385)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html#license-text)
[![R-CMD-check](https://github.com/stangandaho/nimo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stangandaho/nimo/actions/workflows/R-CMD-check.yaml)
[![Code coverage](https://codecov.io/gh/stangandaho/nimo/graph/badge.svg?token=ehmZyEafyI)](https://app.codecov.io/gh/stangandaho/nimo)

# nimo <img src="man/figures/logo.png" align="right" height="90" alt="NIMO LOGO" />

**`nimo`** provides a user-friendly graphical interface that enables users to define 
complete or partial modeling procedures, including variables, records, algorithms, 
ensemble methods, and algorithm tuning.  This comprehensive package empowers users 
to explore, customize, and model their data effectively, with the added benefit of 
accessing real-world biodiversity data. They can intuitively control the input 
parameters, explore different modeling options, and observe the corresponding 
outputs in real-time. This visual and interactive experience enhances the usability 
and accessibility allowing researchers to focus on their modeling tasks without 
worrying about complex programming or technical details. It allows users to easily 
query occurrence data through Global Biodiversity Information Facility (GBIF) and incorporate it into their 
modeling workflow. Based on [**`flexsdm`**](https://github.com/sjevelazco/flexsdm/) 
features and powered by [**`shiny`**](https://github.com/rstudio/shiny), 
**`nimo`** offers users the flexibility to manipulate and parameterize models. 

# Install
**`nimo`** is not yet on CRAN. The development version can be installed from [github](https://github.com/stangandaho/nimo). 


``` r
# Install pak if it is not already installed

if (!require("pak", character.only = TRUE)) {
  install.packages("pak")
}

# Install all required packages at once
pak::pkg_install(c(
  "sjevelazco/flexsdm",
  "stangandaho/nimo"
))
```

# Get started
Once **nimo** is installed, load the package and call `nimo()` function.
```r
library(nimo)
nimo()
```
The **nimo** [website](https://nimo.naben.org) provides to users the resources to get started. 
All necessaries concept and part of **nimo** are explained with screenshot and example. 

# What you can do in the app

| Step | Features |
|------|----------|
| **GBIF** | Query occurrence records by taxon, country, period, drawn area or vector file; save records and dataset citations |
| **Pre-modeling** | Import records, calibration area, predictors, collinearity reduction (Pearson, VIF, PCA, factor analysis) with the option to use principal components as predictors, environmental outliers, occurrence filtering (geographical or environmental, with automatic selection among several filtering values), spatial constraint layers, data partition, background and pseudo-absence points, extraction |
| **Modeling** | GAM, GLM, GBM, Maxent, neural networks, random forest, SVM, Gaussian process, Bioclim and Domain; tuning; ensembles of small models; ensembles |
| **Post-modeling** | Variable importance, response curves and two-predictor response surfaces, predictions (with Bioclim limiting-factor and envelope maps), extrapolation (in geographical and environmental space) and truncation, bootstrap uncertainty, overprediction correction |
| **Script** | The R code of every step run in the app, ready to download |

# Reproducible script
Every step run in the app is recorded as R code with the exact options used. Open
**Script** in the sidebar to see the code as the analysis goes, and download it at
any time. Running the script in R reproduces the results of the app (the random seed
set in the configuration page is used for every random step). A short extract of a
downloaded script:

```r
library(nimo)
library(flexsdm)
library(terra)

seed <- 123

# 1. Occurrence data ----
occ_raw <- read.csv(file = "D:/data/Gyps_africanus.csv", header = TRUE)

# 3. Predictors ----
env_files <- list.files("D:/data/env_layers", pattern = "\\.tiff?$", ignore.case = TRUE, full.names = TRUE)
env <- terra::rast(env_files)

# 9. Model fitting ----
## Bioclim Models
set.seed(seed)
m_bioc <- nimo::nm_fit_bioclim(
  data = sdm_data,
  response = "pr_ab",
  predictors = c("dnw", "dsm"),
  partition = ".part",
  thr = "max_sens_spec",
  tails = "both",
  aggregation = "min"
)
```

# Using nimo in R
The functions used by the app can also be used directly in R scripts.

| Function | Purpose |
|----------|---------|
| `nm_gbif_occ()`, `nm_gbif_suggestion()` | Query GBIF occurrence records and taxon names |
| `nm_extract()`, `nm_match_raster()`, `nm_find_hcv()` | Extract predictor values, match rasters, find highly correlated variables |
| `nm_fit_bioclim()`, `predict()` | Fit and predict Bioclim models (tails and aggregation options) |
| `nm_bioclim_limiting()`, `nm_bioclim_envelope()`, `nm_bioclim_plot()` | Limiting-factor map, core / marginal / outside envelope classes, envelope plot |
| `nm_predict()` | Spatial predictions of individual models, ensembles and ensembles of small models |
| `nm_varimp()`, `nm_varimp_plot()` | Permutation variable importance for any model |
| `nm_response()`, `nm_response_plot()` | Marginal response curves and partial dependence |
| `nm_response2d()`, `nm_response2d_plot()` | Response surface of two predictors |
| `nm_uncertainty()` | Bootstrap uncertainty map |
| `nm_eval()`, `nm_boyce()` | Performance metrics |

```r
library(nimo)

df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
vars <- c("dnw", "dsm", "lst", "ndvi")

m_bioc <- nm_fit_bioclim(df, "pr_ab", vars, partition = ".part", thr = "max_sens_spec")
m_glm <- flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part", thr = "max_sens_spec")

vi <- nm_varimp(list(bioclim = m_bioc, glm = m_glm), data = df)
nm_varimp_plot(vi)

rc <- nm_response(list(bioclim = m_bioc, glm = m_glm), data = df)
nm_response_plot(rc, data = df)
```

# To do 

## General
* [x] Stylize User Interface
* [x] Package documentation
* [x] Reproducible script of the session
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
| Environmental outliers, filtering selection, spatial constraints, PCA predictors | 5 | Completed  ✅ |
| Documentation                          | 5              | Completed  ✅     |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |

## Modeling

| Task                                   | Progress (1-5) | Status                                                                                         |
|----------------------------------------|----------------|----------------------------------------------------------------------------------------------- |
| Pre-modeling user interface            | 5              | Completed  ✅ |
| Construct and validate models with default hyper-parameter [(model fitting)](https://sjevelazco.github.io/flexsdm/articles/v02_modeling.html)| 5 | Completed  ✅ |
|  Model tuning by searching for the best combination of hyper-parameter| 5 | Completed ✅|
| Bioclim (native implementation) and Domain models | 5 | Completed  ✅ |
| Documentation                          | 5              | Completed  ✅     |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |

## Post-modeling

| Task                                   | Progress (1-5) | Status                                                                                         |
|----------------------------------------|----------------|----------------------------------------------------------------------------------------------- |
| Post-modeling user interface            | 5              | Completed  ✅ |
| Functionalities for [(model prediction, evaluation and correction)](https://sjevelazco.github.io/flexsdm/articles/v03_post_modeling.html)| 5 | Completed  ✅ |
| Variable importance, response curves and surfaces | 5 | Completed  ✅ |
| Extrapolation in environmental space, truncation, uncertainty | 5 | Completed  ✅ |
| Documentation                          | 5              | Completed  ✅     |
| Add tooltip to briefly explain inputs by mouse hovering | 2               | Not started   |


# Citation
Gandaho, S. M., Sogbohossou, E. A., & Thompson, L. J. (2024). NIMO: A graphical
user interface-based R package for species distribution modelling. Ecological
Solutions and Evidence, 5, e12385. <https://doi.org/10.1002/2688-8319.12385>
