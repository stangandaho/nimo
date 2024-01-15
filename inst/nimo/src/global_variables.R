# Algotithm
algorithm <- c("Generalized Additive Models" = "gam", "Maximum Entropy" = "max",
               "Neural Networks" = "net", "Generalized Linear Models" = "glm",
               "Gaussian Process" = "gau", "Generalized Boosted Regression" = "gbm",
               "Random Forest" = "raf", "Support Vector Machine" = "svm")
# Ensemble method
ensemble <- c("Average" = "mean", "Super average" = "meansup", "Weighted average" = "meanw",
              "Median" = "median", "Based on Threshold" = "meanthr")
# Calibration area method
calib_area_method <- c("Buffer" = "buffer", "Minimum convex polygon" = "mcp",
                       "Buffered minimum convex polygon" = "bmcp", "Mask" = "mask")
# Occurrence filtering method
occ_filt_method <- c("Moran" = "moran", "Cellsize" = "cellsize", "Defined" = "defined")
bg_metho <- c("Random" = "random", "Thickening" = "thickening")
pseudo_abs_method <- c("Random" = "random", "Env constrained" = "env_const",
                       "Geo contrained" = "geo_const", "Env & Geo contrained" = "geo_env_const",
                       "Env clustering" = "geo_env_km_const")
## Add ressource
addResourcePath("nimo", paste0(system.file("nimo", package = "nimo"), "/www"))#addResourcePath("nimo", "./inst/nimo/www")

# Button style
bttn_primary_style <-  paste0("background-color:", "#065325;", "color:#ffffff;")
bttn_second_style <- paste0("background-color:#065325;", "color:#ffffff;", "hover:red")
bttn_third_style <- paste0("background-color:#B7C1C6")
bttn_warn <- paste0("background-color:#f0b0a9", "border-color:#f0b0a9")
loader_color <- "#042d0f"; loader_type  <- 7
