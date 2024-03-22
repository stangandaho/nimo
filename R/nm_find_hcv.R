
#' Find highly correlated variables
#' @description
#' This function examines a correlation matrix and generates a vector of
#' integers representing columns to be eliminated for minimizing pairwise
#' correlations. It is similar to [findCorrelation()] in `caret` package.
#'
#' @param x A correlation matrix
#' @param cutoff A numeric value for the pair-wise absolute correlation cutoff
#' @param verbose A boolean for printing the details
#'
#' @return
#' A vector of of column names denoting the columns to remove. If no correlations meet the criteria,
#' integer(0) is returned.
#'
#' @examples
#' wbv_path <- paste0(system.file("extdata", package = "nimo"), "/Gyps_africanus_KNP.csv")
#' wbv_df <- read.csv(wbv_path)
#' mat_dt <- wbv_df[, c("dsm", "lst", "ndvi", "dnw")]
#' corr <- nm_find_hcv(x = cor(mat_dt), cutoff = 0.4)
#' corr
#'
#' @export

nm_find_hcv <- function (x, cutoff = 0.9, verbose = FALSE){

  findCorrelation_fast <- function (x, cutoff = 0.9, verbose = FALSE){
    if (any(!complete.cases(x)))
      stop("The correlation matrix has some missing values.")
    averageCorr <- colMeans(abs(x))
    averageCorr <- as.numeric(as.factor(averageCorr))
    x[lower.tri(x, diag = TRUE)] <- NA
    combsAboveCutoff <- which(abs(x) > cutoff)
    colsToCheck <- ceiling(combsAboveCutoff/nrow(x))
    rowsToCheck <- combsAboveCutoff%%nrow(x)
    colsToDiscard <- averageCorr[colsToCheck] > averageCorr[rowsToCheck]
    rowsToDiscard <- !colsToDiscard
    if (verbose) {
      colsFlagged <- pmin(ifelse(colsToDiscard, colsToCheck,
                                 NA), ifelse(rowsToDiscard, rowsToCheck, NA), na.rm = TRUE)
      values <- round(x[combsAboveCutoff], 3)
      cat("\n", paste("Combination row", rowsToCheck, "and column",
                      colsToCheck, "is above the cut-off, value =", values,
                      "\n \t Flagging column", colsFlagged, "\n"))
    }
    deletecol <- c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
    deletecol <- unique(deletecol)
    deletecol
  }

  if (is.null(colnames(x))){stop("'x' must have column names")}

  out <- findCorrelation_fast(x = x, cutoff = cutoff, verbose = verbose)
  out <- colnames(x)[out]
  return(out)
}
