library(flexsdm)

# esm_gam()
# esm_gau()
# esm_gbm()
# esm_glm()
# esm_max()
# esm_net()
# esm_svm()
sdm_summarize()

require(dplyr)
require(terra)

data("spp")
somevar <- system.file("external/somevar.tif", package = "flexsdm")
somevar <- terra::rast(somevar)

# Extract data
some_sp <- spp %>%
  filter(species == "sp3")

some_sp <-
  sdm_extract(
    data = some_sp,
    x = "x",
    y = "y",
    env_layer = somevar
  )

# Partition
some_sp <- part_random(
  data = some_sp,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 3, replicates = 5)
)


## %######################################################%##
#                                                          #
####          Create different type of models           ####
#                                                          #
## %######################################################%##
# Fit some models
mglm <- fit_glm(
  data = some_sp,
  response = "pr_ab",
  predictors = c("CFP_1", "CFP_2", "CFP_3", "CFP_4"),
  partition = ".part",
  poly = 2
)
mraf <- fit_raf(
  data = some_sp,
  response = "pr_ab",
  predictors = c("CFP_1", "CFP_2", "CFP_3", "CFP_4"),
  partition = ".part",
)
mgbm <- fit_gbm(
  data = some_sp,
  response = "pr_ab",
  predictors = c("CFP_1", "CFP_2", "CFP_3", "CFP_4"),
  partition = ".part"
)

# Fit an ensemble model
mensemble <- fit_ensemble(
  models = list(mglm, mraf, mgbm),
  ens_method = "meansup",
  thr = NULL,
  thr_model = "max_sens_spec",
  metric = "TSS"
)

# Fit a model with the Ensembles of Small Models approach
# Without threshold specification and with kfold
msmall <- esm_gam(
  data = some_sp,
  response = "pr_ab",
  predictors = c("CFP_1", "CFP_2", "CFP_3", "CFP_4"),
  partition = ".part",
  thr = NULL
)
names(msmall)

## %######################################################%##
#                                                          #
####      Predict different kind of models       ####
#                                                          #
## %######################################################%##

# sdm_predict can be used for predict one or more models fitted with fit_ or tune_ functions

# a single model
ind_p <- sdm_predict(
  models = mglm,
  pred = somevar,
  thr = "max_fpb",
  con_thr = FALSE,
  predict_area = NULL
)
print(ind_p)
# a list of models
list_p <- sdm_predict(
  models = list(mglm, mraf, mgbm),
  pred = somevar,
  thr = "max_fpb",
  con_thr = FALSE,
  predict_area = NULL
)

# Predict an ensemble model
# (only is possilbe use one fit_ensemble)
ensemble_p <- sdm_predict(
  models = mensemble,
  pred = somevar,
  thr = "max_fpb",
  con_thr = FALSE,
  predict_area = NULL
)

# Predict an ensemble of small models
# (only is possible to use one ensemble of small models)
small_p <- sdm_predict(
  models = msmall,
  pred = somevar,
  thr = "max_fpb",
  con_thr = FALSE,
  predict_area = NULL
)

## End(Not run)

l <- list()
l$Stan <- mglm; l$MAH <- mraf; l$BIL <- mgbm

ess <- list()
ess[[2]] <- l[[2]]
names(ess) <- c( "model", "predictors", "performance", "data_ens")

  
concat_model <- function(list, index = c()){
  empty_list <- list()
  for (i in index) {
    empty_list[[i]] <- list[[i]]
  }
  return(empty_list[i][[1]])
}

predic_modlist <- lapply(c(1,3), function(x) concat_model(l, x))

predicti <- sdm_predict(
  models = predic_modlist,
  pred = somevar,
  thr = "max_jaccard",
  con_thr = FALSE,
  predict_area = NULL
)

#length(predic_modlist)

#par(mfcol= c(1, 2))
predic_raster_list <- list()
for (m in 1:length(predicti)) {
  predict_raster <- predicti[[m]]
  model_name <- names(which(algorithm == names(predict_raster)[1]))
  names(predict_raster) <- c(model_name, paste(names(predict_raster)[1], "- max_jaccard"))
  for (r in 1:terra::nlyr(predict_raster)) {
    predic_raster_list[[ names(predict_raster[[r]]) ]] <- predict_raster[[r]]
    print(length(predic_raster_list))
  }
  #terra::plot(predicti[[m]], main = names(predict_raster))
}

par(mfrow = c(2, 2))
for (ras in 1:length(predic_raster_list)) {
  terra::plot(predic_raster_list[[ras]], main = names(predic_raster_list[[ras]]))
}