#' Bioclim model
#'
#' The BIOCLIM algorithm
#'
#' @param data data.frame object with response (0,1) and predictors values.
#' @param response character. Column name with species absence-presence data (0,1).
#' @param predictors character. Vector with the column names of quantitative predictor variables.
#' @param partition character. Column name with training and validation partition
#' groups (like part_* family output from `flexsdm`).
#' @param thr character. Threshold used to get binary suitability values (i.e. 0,1).
#' See a function of model fitting family in `flexsdm` package (e.g [fit_gam()])
#'
#' @return
#'
#' A list object with:
#' \itemize{
#' \item model: A "Bioclim" class object from dismo package. This object can be used for predicting.
#' \item predictors: A tibble with quantitative (c column names) variables use for modeling.
#' \item performance: Performance metric (see \code{\link{nm_eval}}).
#' Threshold dependent metrics are calculated based on the threshold specified in the argument.
#' \item data_ens: Predicted suitability for each test partition.
#' }
#'
#' @examples
#'
#' wbv_path <- paste0(system.file("extdata", package = "nimo"), "/Gyps_africanus_KNP.csv")
#' wbv_df <- read.csv(wbv_path)
#' mod = nm_fit_bioclim(data = wbv_df,
#'                     response = "pr_ab",
#'                     predictors = c("dnw", "dsm", "lst", "ndvi"),
#'                     partition = ".part",
#'                     thr = "max_sens_spec")
#'
#' @export

nm_fit_bioclim <- function(data,
                           response,
                           predictors,
                           partition,
                           thr = NULL) {
  . <- model <- TPR <- IMAE <- rnames <- thr_value <- n_presences <- n_absences <- NULL
  variables <- dplyr::bind_rows(c(c = predictors))

  # Test response variable
  r_test <- (data %>% dplyr::pull(response) %>% unique() %>% na.omit())
  if ((!all(r_test %in% c(0, 1)))) {
    stop("values of response variable do not match with 0 and 1")
  }

  data <- data.frame(data)

  data <- data %>%
    dplyr::select(dplyr::all_of(response), dplyr::all_of(predictors), dplyr::starts_with(partition))

  # Remove NAs
  complete_vec <- stats::complete.cases(data[, c(response, unlist(variables))])
  if (sum(!complete_vec) > 0) {
    message(sum(!complete_vec), " rows were excluded from database because NAs were found")
    data <- data %>% dplyr::filter(complete_vec)
  }
  rm(complete_vec)


  # Fit models
  np <- ncol(data %>% dplyr::select(dplyr::starts_with(partition)))
  p_names <- names(data %>% dplyr::select(dplyr::starts_with(partition)))
  eval_partial_list <- list()
  pred_test_ens <- data %>%
    dplyr::select(dplyr::starts_with(partition)) %>%
    apply(., 2, unique) %>%
    data.frame() %>%
    as.list() %>%
    lapply(., function(x) {
      x <- stats::na.exclude(x)
      x[!(x %in% c("train-test", "test"))] %>% as.list()
    })

  #---#
  pre_tr_te <- function(data, p_names, h) {
    train <- list()
    test <- list()

    if (any(c("train", "train-test", "test") %in% unique(data[, p_names[h]]))) {
      np2 <- 1

      filt <- grepl("train", data[, p_names[h]])
      train[[1]] <- data[filt, ] %>%
        dplyr::select(-p_names[!p_names == p_names[h]])

      filt <- grepl("test", data[, p_names[h]])
      test[[1]] <- data[filt, ] %>%
        dplyr::select(-p_names[!p_names == p_names[h]])
    } else {
      np2 <- max(data[p_names[h]])

      for (i in 1:np2) {
        train[[i]] <- data[data[p_names[h]] != i, ] %>%
          dplyr::select(-p_names[!p_names == p_names[h]])

        test[[i]] <- data[data[p_names[h]] == i, ] %>%
          dplyr::select(-p_names[!p_names == p_names[h]])
      }
    }
    return(list(train = train, test = test, np2 = np2))
  }
  #--#

  for (h in 1:np) {
    message("Replica number: ", h, "/", np)

    out <- pre_tr_te(data, p_names, h)
    train <- out$train
    test <- out$test
    np2 <- out$np2
    rm(out)

    train <- lapply(train, function(x) x[x[, response] == 1, ])

    eval_partial <- as.list(rep(NA, np2))
    pred_test <- list()
    mod <- list()


    for (i in 1:np2) {
      message("Partition number: ", i, "/", np2)
      tryCatch({
        mod[[i]] <- dismo::bioclim( train[[i]][predictors] %>% as.matrix() )

        pred_test <- data.frame(
          pr_ab = test[[i]][, response],
          pred = dismo::predict(
            object = mod[[i]],
            x = test[[i]]
          )
        )

        pred_test_ens[[h]][[i]] <- pred_test %>%
          dplyr::mutate(rnames = rownames(test[[i]]))

        # Validation of model
        eval <- sdm_eval(
          p = pred_test$pred[pred_test$pr_ab == 1],
          a = pred_test$pred[pred_test$pr_ab == 0],
          thr = thr
        )

        eval_partial[[i]] <- dplyr::tibble(model = "bioc", eval)
        names(eval_partial) <- i
      })
    }

    # Create final database with parameter performance
    eval_partial <-
      eval_partial[sapply(eval_partial, function(x) !is.null(dim(x)))] %>%
      dplyr::bind_rows(., .id = "partition")
    eval_partial_list[[h]] <- eval_partial
  }

  eval_partial <- eval_partial_list %>%
    dplyr::bind_rows(., .id = "replica")

  eval_final <- eval_partial %>%
    dplyr::group_by(model, threshold) %>%
    dplyr::summarise(dplyr::across(
      TPR:IMAE,
      list(mean = mean, sd = stats::sd)
    ), .groups = "drop")

  # Bind data for ensemble
  for (e in 1:length(pred_test_ens)) {
    fitl <- sapply(pred_test_ens[[e]], function(x) !is.null(nrow(x)))
    pred_test_ens[[e]] <- pred_test_ens[[e]][fitl]
  }

  pred_test_ens <-
    lapply(pred_test_ens, function(x) {
      bind_rows(x, .id = "part")
    }) %>%
    bind_rows(., .id = "replicates") %>%
    dplyr::tibble() %>%
    dplyr::relocate(rnames)

  # Fit final models
  data_2 <- data[data[, response]==1, ]


  mod <- dismo::bioclim(data_2[predictors] %>% as.matrix)

  pred_test <- data.frame(
    "pr_ab" = data.frame(data)[, response],
    "pred" = dismo::predict(mod, data[, predictors])
  )

  threshold <- sdm_eval(
    p = pred_test$pred[pred_test$pr_ab == 1],
    a = pred_test$pred[pred_test$pr_ab == 0],
    thr = thr
  )

  result <- list(
    model = mod,
    predictors = variables,
    performance = dplyr::left_join(eval_final, threshold[1:4], by = "threshold") %>%
      dplyr::relocate(model, threshold, thr_value, n_presences, n_absences),
    data_ens = pred_test_ens
  )
  return(result)
}
