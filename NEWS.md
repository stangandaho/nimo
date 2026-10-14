# nimo 1.0.0

## Reproducible script

* Every step run in the app is recorded as R code. The *Script* page shows the
  script and downloads it at any time; running it reproduces the app results.
* Random steps use the seed set in the configuration, so results can be
  repeated.
* GBIF queries are part of the recorded script.

## Bioclim

* Bioclim is fully integrated in nimo and no longer depends on another package.
* Users can choose which tails of the predictors limit suitability and how
  predictors are combined.
* After fitting, an envelope plot shows the conditions of the presences.
* Predictions include a map of the most limiting predictor and a map of the
  core, marginal and outside areas of the envelope.

## Modeling and interpretation

* Domain models can be fitted.
* Variable importance shows how much each model relies on each predictor.
* Response curves and response surfaces show how suitability changes along one
  or two predictors.

## Pre-modeling

* Environmental outliers can be detected and removed.
* Filtered occurrences can be used in the analysis, and several filtering values
  can be compared to select the best dataset.
* Principal components can replace the predictors.
* Spatial constraint layers can be added to the predictors to reduce
  overprediction.

## Post-modeling

* Uncertainty maps show where predictions change with the records used.
* The *Extrapolation & uncertainty* page adds the environmental space plot and
  the truncation of predictions in extrapolated areas.

## Bug fixes

* Data extracted in the app can be used for modeling without loading a file.
* "Start modeling" from GBIF passes the data to the workflow.
* The "Calibrate area" option and the prediction area are applied.
* Ensembles no longer require Ensembles of Small Models.
* The Gaussian process button of Ensembles of Small Models no longer fits a GAM.
* Extrapolation only uses the predictors of the modeling data.
* The occurrence filtering plot is displayed again.
* Binary prediction maps are named.
* The predictor folder only lists GeoTIFF files.
