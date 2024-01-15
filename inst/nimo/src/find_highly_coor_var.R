# Function to Determine highly correlated variables - caret R package
findCorrelation_fast <- function (x, cutoff = 0.9, verbose = FALSE)
        {
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

find_coor <- function (x, cutoff = 0.9, verbose = FALSE, names = FALSE, exact = ncol(x) < 100)
        {
          if (names & is.null(colnames(x)))
            stop("'x' must have column names when `names = TRUE`")
          out <- findCorrelation_fast(x = x, cutoff = cutoff, verbose = verbose)
          if (names)
            out <- colnames(x)[out]
          out
        }
