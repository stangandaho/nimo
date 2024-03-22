#' Continuous Boyce Index (CBI) with weighting
#'
#' This function calculates the continuous Boyce index (CBI), a measure of model accuracy for presence-only test data. This version uses multiple, overlapping windows, in contrast to \code{link{contBoyce2x}}, which covers each point by at most two windows.
#' @param presence_predicted Numeric vector. Predicted values at presence sites.
#' @param contrast Numeric vector. Predicted values at background sites.
#' @param num_bins Positive integer. Number of (overlapping) bins into which to divide predictions.
#' @param bin_width Positive numeric value < 1. Size of a bin. Each bin will be \code{bin_width * (max - min)}. If \code{autoWindow} is \code{FALSE} (the default) then \code{min} is 0 and \code{max} is 1. If \code{autoWindow} is \code{TRUE} then \code{min} and \code{max} are the maximum and minimum value of all predictions in the background and presence sets (i.e., not necessarily 0 and 1).
#' @param presence_weight Numeric vector same length as \code{presence_predicted}. Relative weights of presence sites. The default is to assign each presence a weight of 1.
#' @param contrast_weight Numeric vector same length as \code{contrast}. Relative weights of background sites. The default is to assign each presence a weight of 1.
#' @param auto_window Logical. If \code{FALSE} calculate bin boundaries starting at 0 and ending at 1 + epsilon (where epsilon is a very small number to assure inclusion of cases that equal 1 exactly). If \code{TRUE} (default) then calculate bin boundaries starting at minimum predicted value and ending at maximum predicted value.
#' @param method Character. Type of correlation to calculate. The default is \code{'spearman'}, the Spearman rank correlation coefficient used by Boyce et al. (2002) and Hirzel et al. (2006), which is the "traditional" CBI. In contrast, \code{'pearson'} or \code{'kendall'} can be used instead.  See \code{\link[stats]{cor}} for more details.
#' @param drop_zeros Logical. If \code{TRUE} then drop all bins in which the frequency of presences is 0.
#' @param graph Logical. If \code{TRUE} then plot P vs E and P/E versus bin.
#' @param na.rm Logical. If \code{TRUE} then remove any presences and associated weights and background predictions and associated weights with \code{NA}s.
#' @param bg Same as \code{contrast}. Included for backwards compatibility. Ignored if \code{contrast} is not \code{NULL}.
#' @param bg_weight Same as \code{contrast_weight}. Included for backwards compatibility. Ignored if \code{contrast_weight} is not \code{NULL}.
#' @param ... Other arguments (not used).
#'
#' @importFrom enmSdm contBoyce
#' @return Numeric value.
#'
#' @details CBI is the Spearman rank correlation coefficient between the proportion of sites in each prediction class and the expected proportion of predictions in each prediction class based on the proportion of the landscape that is in that class.  The index ranges from -1 to 1. Values >0 indicate the model's output is positively correlated with the true probability of presence.  Values <0 indicate it is negatively correlated with the true probability of presence.
#'
#' @references Boyce, M.S., Vernier, P.R., Nielsen, S.E., and Schmiegelow, F.K.A.  2002.  Evaluating resource selection functions.  \emph{Ecological Modeling} 157:281-300. \doi{https://doi.org/10.1016/S0304-3800(02)00200-4}
#' @references Hirzel, A.H., Le Lay, G., Helfer, V., Randon, C., and Guisan, A.  2006.  Evaluating the ability of habitat suitability models to predict species presences.  \emph{Ecological Modeling} 199:142-152. \doi{10.1016/j.ecolmodel.2006.05.017}
#'
#' @seealso \code{\link[stats]{cor}}, \code{link[enmSdm]{contBoyce2x}}
#'
#' @examples
#'
#' set.seed(123)
#' presence_predicted <- sqrt(runif(100))
#' contrast <- runif(1000)
#' nm_boyce(presence_predicted, contrast)
#' contBoyce2x(presence_predicted, contrast)
#' presence_weight <- c(rep(1, 10), rep(0.5, 90))
#' nm_boyce(presence_predicted, contrast, presence_weight=presence_weight)
#'

nm_boyce <- function(
    presence_predicted,
    contrast,
    numBins = 101,
    bin_width = 0.1,
    presence_weight = rep(1, length(presence_predicted)),
    contrast_weight = rep(1, length(contrast)),
    auto_window = TRUE,
    method = 'spearman',
    drop_zeros = TRUE,
    graph = FALSE,
    na.rm = FALSE,
    bg = NULL,
    bg_weight = NULL,
    ...
) {

  if (missing(contrast) & !is.null(bg)) contrast <- bg
  if (missing(contrast_weight) & !is.null(bg_weight)) contrast_weight <- bg_weight

  # if all NAs
  if (all(is.na(presence_predicted)) | all(is.na(contrast)) | all(is.na(presence_weight)) | all(is.na(contrast_weight))) return(NA)

  # catch errors
  if (bin_width > 1 | bin_width <= 0) stop('Argument "bin_width" must be between 0 and 1.')
  if (length(presence_weight) != length(presence_predicted)) stop('You must have the same number of presence predictions and presence weights ("presence_predicted" and "presence_weight").')
  if (length(contrast_weight) != length(contrast)) stop('You must have the same number of absence/background predictions and absence/background weights ("contrast" and "contrast_weight").')

  # right hand side of each class (assumes max value is >0)
  lowest <- if (auto_window) { min(c(presence_predicted, contrast), na.rm=na.rm) } else { 0 }
  highest <- if (auto_window) { max(c(presence_predicted, contrast), na.rm=na.rm) + .Machine$double.eps } else { 1 + .Machine$double.eps }

  windowWidth <- bin_width * (highest - lowest)

  lows <- seq(lowest, highest - windowWidth, length.out=numBins)
  highs <- seq(lowest + windowWidth + .Machine$double.eps, highest, length.out=numBins)

  ##########
  ## MAIN ##
  ##########

  ## initiate variables to store predicted/expected (P/E) values
  freqPres <- freqContrast <- rep(NA, length(numBins))

  ### tally proportion of test presences/background sites in each class
  for (countClass in 1:numBins) {

    # number of presence predictions in this class
    presInBin <- presence_predicted >= lows[countClass] & presence_predicted < highs[countClass]
    presInBin <- presInBin * presence_weight
    freqPres[countClass] <- sum(presInBin, na.rm=na.rm)

    # number of background predictions in this class
    bgInBin <- contrast >= lows[countClass] & contrast < highs[countClass]
    bgInBin <- bgInBin * contrast_weight
    freqContrast[countClass] <- sum(bgInBin, na.rm=na.rm)

  } # next predicted value class

  # mean bin prediction
  meanPred <- rowMeans(cbind(lows, highs))

  # add small number to each bin that has 0 background frequency but does have a presence frequency > 0
  if (any(freqPres > 0 & freqContrast == 0)) {
    smallValue <- min(0.5 * c(presence_weight[presence_weight > 0], contrast_weight[contrast_weight > 0]))
    freqContrast[freqPres > 0 & freqContrast == 0] <- smallValue
  }

  # remove classes with 0 presence frequency
  if (drop_zeros && 0 %in% freqPres) {
    zeros <- which(freqPres == 0)
    meanPred[zeros] <- NA
    freqPres[zeros] <- NA
    freqContrast[zeros] <- NA
  }

  # remove classes with 0 background frequency
  if (any(0 %in% freqContrast)) {
    zeros <- which(freqContrast == 0)
    meanPred[zeros] <- NA
    freqPres[zeros] <- NA
    freqContrast[zeros] <- NA
  }

  P <- freqPres / sum(presence_weight, na.rm=TRUE)
  E <- freqContrast / sum(contrast_weight, na.rm=TRUE)
  PE <- P / E

  # plot
  if (graph) {
    graphics::par(mfrow=c(1, 2))
    lims <- c(0, max(P, E, na.rm=TRUE))
    plot(E, P, col='white', xlab='Expected', ylab='Predicted', main='P/E\nNumbered from lowest to highest class', xlim=lims, ylim=lims)
    graphics::text(E, P, labels=1:numBins, col=1:20)
    plot(meanPred, PE, type='l', xlab='Mean Prediction in Bin', ylab='P/E Ratio', main='CBI\nNumbered from lowest to highest class')
    graphics::text(meanPred, PE, labels=1:numBins, col='blue')
  }

  # remove NAs
  meanPred <- stats::na.omit(meanPred)
  PE <- stats::na.omit(PE)

  # calculate continuous Boyce index (cbi)
  cbi <- stats::cor(x=meanPred, y=PE, method=method)
  return(cbi)
}
