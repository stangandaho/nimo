# nimo 0.3.0

## Reproducible script

* Every step run in the app is recorded as R code. The *Script* page shows the
  script and downloads it at any time; running it reproduces the app results.
* Random steps use the seed set in the configuration.
* `nm_gbif_occ()` queries GBIF occurrences.

## Bioclim

* `nm_fit_bioclim()` is implemented natively, without `dismo`.
* Limiting tails and aggregation of variables can be chosen.
* `predict()` method, `nm_bioclim_limiting()`, `nm_bioclim_envelope()` and
  `nm_bioclim_plot()`, also available in the app.

## Modeling and interpretation

* Domain models can be fitted in the app.
* Permutation variable importance for every model: `nm_varimp()`,
  `nm_varimp_plot()`.
* Response curves and response surfaces for every model, marginal or partial
  dependence: `nm_response()`, `nm_response2d()` and their plot functions.

## Pre-modeling

* Environmental outliers can be detected and removed.
* Filtered occurrences can be used in the analysis, and several filtering values
  can be compared to select the best dataset.
* Principal components can replace the predictors.
* Spatial constraint layers can be added to the predictors.

## Post-modeling

* `nm_uncertainty()` maps prediction uncertainty from bootstrap refits.
* The *Extrapolation & uncertainty* page adds the environmental space plot,
  prediction truncation and the uncertainty map.

## Bug fixes

* Data extracted in the app can be used for modeling without loading a file.
* "Start modeling" from GBIF passes the data to the workflow.
* The "Calibrate area" option and the prediction area are applied.
* Ensembles no longer require ESM models.
* The ESM Gaussian process button no longer fits a GAM.
* Extrapolation only uses the predictors of the modeling data.
* The occurrence filtering plot is displayed again.
* Binary layers from `nm_predict()` are named.
* The predictor folder only lists GeoTIFF files.
