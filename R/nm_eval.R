#' Calculate model performance metrics
#'
#'This function calculates threshold dependent and independent model performance metrics.
#'The function is imported and completed from [sdm_eval()].
#'
#' @param p numeric. Predicted suitability for presences
#' @param a numeric. Predicted suitability for absences
#' @param bg numeric. Predicted suitability for background points, used for BOYCE metric
#' @param thr character. Threshold criterion used to get binary suitability values (i.e. 0,1). See *Details* section.
#'
#' @return
#' A tibble with next columns
#' \itemize{
#' \item threshold: threshold names
#' \item thr_value: threshold values
#' \item n_presences: number of presences
#' \item n_absences: number of absences
#' \item performance metrics: from Prevalence to IMAE:
#' }
#' @details
#' The additionally metrics completed are: prevalence, False Positive Rate, False Nagative Rate,
#' Correct Classification Rate, Miss Classification Rate, Kappa. See [sdm_eval()].
#'
#' It is possible to use more than one threshold type.
#' A vector must be provided for this argument. The following threshold criteria are available:
#' \itemize{
#'   \item lpt: The highest threshold at which there is no omission.
#'   \item equal_sens_spec: Threshold at which the Sensitivity and Specificity are equal.
#'   \item max_sens_spec: Threshold at which the sum of the Sensitivity and Specificity
#'   is the highest (aka threshold that maximizes the TSS).
#'   \item max_jaccard: The threshold at which the Jaccard index is the highest.
#'   \item max_sorensen: The threshold at which the Sorensen index is the highest.
#'   \item max_fpb: The threshold at which FPB (F-measure on presence-background data) is the highest.
#'   \item sensitivity: Threshold based on a specified Sensitivity value.
#'   Usage thr = c('sensitivity', sens='0.6') or thr = c('sensitivity'). 'sens' refers
#'   to Sensitivity value. If a sensitivity value is not specified, the
#'    default value is 0.9
#'   }
#' If more than one threshold type is used, concatenate threshold types,
#' e.g., thr=c('lpt', 'max_sens_spec', 'max_jaccard'), or thr=c('lpt', 'max_sens_spec',
#' 'sensitivity', sens='0.8'), or thr=c('lpt', 'max_sens_spec', 'sensitivity').
#' Function will use all thresholds if no threshold type is specified
#'
#' @examples
#' \dontrun{
#' require(dplyr)
#'
#' set.seed(0)
#' p <- rnorm(66, mean = 0.6, sd = 0.4) %>% abs()
#' p[p > 1] <- 1
#' p[p < 0] <- 0
#'
#' set.seed(0)
#' a <- rnorm(45, mean = 0.6, sd = 0.3) %>% abs()
#' a[a > 1] <- 1
#' a[a < 0] <- 0
#'
#' e <- nm_eval(p, a)
#' }
#'
#' @export
nm_eval <- function (p, a, bg = NULL, thr = NULL) {

  TPR <- TNR <- JACCARD <- SORENSEN <- threshold <- FPB <- TSS <- NULL
  if (any(!(thr[is.na(suppressWarnings(as.numeric(thr)))]) %in%
          c("lpt", "max_sens_spec", "equal_sens_spec", "sensitivity",
            "max_jaccard", "max_sorensen", "max_fpb"))) {
    stop("'thr' Argument is not valid!")
  }
  if (is.null(thr)) {
    thr <- c("lpt", "max_sens_spec", "max_kappa", "equal_sens_spec",
             "sensitivity", "max_jaccard", "max_sorensen", "max_fpb")
  }
  if (any(thr %in% "sensitivity") && !any(names(thr) %in% "sens")) {
    thr <- c(thr, sens = 0.9)
  }
  np <- length(p); na <- length(a); N <- na + np
  if (na == 0 | np == 0) {
    stop("Presence and absence must be greater than zero")
  }
  if (length(p) > 1000) {
    tr <- as.vector(stats::quantile(p, 0:1000/1000))
  }
  else {
    tr <- p
  }
  if (length(a) > 1000) {
    tr <- c(tr, as.vector(stats::quantile(a, 0:1000/1000)))
  }
  else {
    tr <- c(tr, a)
  }
  tr <- sort(unique(round(tr, 8)))
  res <- matrix(ncol = 4, nrow = length(tr))
  colnames(res) <- c("tp", "fp", "fn", "tn")
  for (i in 1:length(tr)) {
    res[i, 1] <- length(p[p >= tr[i]]) ## a  true positives = tp
    res[i, 2] <- length(a[a >= tr[i]]) ## b  false positives = fp
    res[i, 3] <- length(p[p < tr[i]])  ## c  false negatives = fn
    res[i, 4] <- length(a[a < tr[i]])  ## d  true negatives = tn
  }

  ## Add metrics
  res <- data.frame(res)
  ODP = (res$fp + res$tn) / N # Overall Diagnostic Power
  PPP = res$tp/(res$tp + res$fp)
  NPP = res$tn/(res$fn + res$tn)
  # Kappa metric
  prA = (res$tp + res$tn)/N
  prY = (res$tp + res$fp)/N * (res$tp + res$fn)/N
  prN = (res$fn+ res$tn)/N * (res$fp + res$tn)/N
  prE = prY + prN

  performance <- dplyr::tibble(threshold = tr,
                               n_presences = np,
                               n_absences = na,
                               prevalence = (res$tp + res$fn) / N, # Prevalence
                               TPR = res$tp/(res$tp + res$fn),
                               TNR = res$tn/(res$tn + res$fp),
                               FPR = res$fp / (res$fp + res$tn), # False Positive Rate <-
                               FNR = res$fn/(res$tp + res$fn), # False Negative Rate <-
                               CCR = (res$tp + res$tn) / N, # Correct Classification Rate <-
                               MCR = (res$fp +  res$fn)/N, # Miss Classification Rate <-
                               SORENSEN = 2 * res$tp/(res$fn + (2 * res$tp) + res$fp),
                               JACCARD = res$tp/(res$fn + res$tp + res$fp),
                               KAPPA = (prA - prE) / (1-prE), # Kappa
                               FPB = 2 * JACCARD,
                               OR = (1 - TPR),
                               TSS = (TPR + TNR) - 1)
  R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
  performance <- performance %>% dplyr::mutate(AUC = R/(as.numeric(na) * as.numeric(np)))
  if (is.null(bg)) {
    performance <- performance %>% dplyr::mutate(BOYCE = nm_boyce(presence_predicted = p, contrast = c(p, a)))
  }
  else {
    performance <- performance %>% dplyr::mutate(BOYCE = nm_boyce(presence_predicted = p, bg = bg))
  }
  real <- c(rep(1, length(p)), rep(0, length(a)))
  pred <- c(p, a)
  performance <- performance %>% dplyr::mutate(IMAE = 1 - (sum(abs(real - pred))/length(pred)))

  thresholds <- list()
  thresholds$max_sorensen <- max(performance %>% dplyr::filter(SORENSEN == max(SORENSEN)) %>% dplyr::pull(threshold))
  thresholds$max_jaccard <- max(performance %>% dplyr::filter(JACCARD == max(JACCARD)) %>% dplyr::pull(threshold))
  thresholds$max_fpb <- max(performance %>% dplyr::filter(FPB == max(FPB)) %>% dplyr::pull(threshold))
  thresholds$max_sens_spec <- max(performance %>% dplyr::filter(TSS == max(TSS)) %>% dplyr::pull(threshold))
  thresholds$equal_sens_spec <- performance$threshold[which(abs(performance$TPR - performance$TNR) == min(abs(performance$TPR - performance$TNR)))] %>% max()
  suppressWarnings(thresholds$lpt <- max(performance$threshold[performance$TPR == 1]))
  if (any(thr == "sensitivity")) {
    thresholds$sensitivity <- performance$threshold[which.min(performance$TPR > as.numeric(thr["sens"]))]
  }
  thresholds <- dplyr::bind_cols(thresholds)
  thr_table <- dplyr::tibble(threshold = names(thresholds),
                             thr_value = unlist(thresholds))
  thr_table <- dplyr::left_join(thr_table, performance, by = c(thr_value = "threshold"))
  result <- thr_table
  if (!is.null(thr)) {
    result <- result %>% dplyr::filter(threshold %in% thr)
  }
  performance <<- performance
  return(result)
}
