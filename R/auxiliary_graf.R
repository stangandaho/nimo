## %######################################################%##
#                                                          #
####                     GRaF codes                     ####
#                                                          #
## %######################################################%##
# This codes belogn to GRaF package available at https://github.com/goldingn/GRaF

## citation
# Golding, N., & Purse, B. V. (2016). Fast and flexible Bayesian species distribution modelling
# using Gaussian processes. Methods in Ecology and Evolution, 7, 598–608.
# https://doi.org/10.1111/2041-210X.12523

# cov.SE
cov.SE <- function(x1, x2 = NULL, e1 = NULL, e2 = NULL, l) {
  n1 <- nrow(x1)
  n2 <- ifelse(is.null(x2), n1, nrow(x2))
  n3 <- ncol(x1)

  # distance matrices
  if (is.null(x2)) {
    e2 <- e1
    # if no second matrix do with distance matrices for speed up
    dists <- lapply(1:n3, function(i, x) dist(x[, i])^2, x1)
  } else {
    dists <- list()
    for (i in 1:n3) {
      dists[[i]] <- x1[, i]^2 %*% t(rep(1, n2)) +
        rep(1, n1) %*% t(x2[, i]^2) - 2 * x1[, i] %*% t(x2[, i])
    }
  }

  # with error matrices
  if (!is.null(e1)) {
    E1 <- list()
    ones <- t(rep(1, n2))
    for (i in 1:n3) {
      E1[[i]] <- e1[, i] %*% ones
    }

    if (!is.null(e2)) {
      E2 <- list()
      ones <- t(rep(1, n1))
      for (i in 1:n3) {
        E2[[i]] <- t(e2[, i] %*% ones)
      }
    } else {
      E2 <- as.list(rep(0, n3))
    }

    # run through each covariate

    sumdiffs <- 0
    denom <- 1
    lower <- lower.tri(E1[[1]])
    for (i in 1:n3) {
      err <- E1[[i]] + E2[[i]]
      if (is.null(x2)) {
        err <- err[lower] # save only lower portion for speed up
      }
      sumdiffs <- sumdiffs + dists[[i]] / (err + l[i])
      denom <- denom * (1 + err / l[i])
    }
    # inverse kronecker delta
    ikds <- as.numeric(sumdiffs > 0)
    diag(ikds <- 1)
    denom <- sqrt(denom) * ikds
    K <- exp(-0.5 * sumdiffs) / denom
  } else {
    # without error matrices
    sumdiffs <- 0
    for (i in 1:n3) {
      sumdiffs <- sumdiffs + dists[[i]] / l[i]
    }
    K <- exp(-0.5 * sumdiffs) # to matrix?
  }

  if (class(sumdiffs)[1] == "dist") {
    K <- as.matrix(K)
    diag(K) <- 1
  }
  K
}

# pred
pred <-
  function(predx, fit, mn, std = TRUE, maxn = 250, same = FALSE) {
    predx <- as.matrix(predx)
    n <- nrow(predx)
    if (n > maxn & !same) {
      inds <- split(1:n, ceiling((1:n) / maxn))
      fun <- function(ind, X, fit, std, maxn) {
        pred(X[ind, , drop = FALSE], fit, mn[ind], std, maxn, same)
      }
      prediction <- lapply(inds, fun, predx, fit, std, maxn)
      prediction <- do.call("rbind", prediction)
    } else {
      if (same) {
        # if predicting back to input data re-use covariance matrix
        Kx <- fit$K
        prediction <- fit$MAP
      } else {
        Kx <- cov.SE(x1 = fit$x, x2 = predx, e1 = fit$e, e2 = NULL, l = fit$ls)
        mpred <- stats::qnorm(mn)
        prediction <- crossprod(Kx, fit$a) + mpred
      }

      if (std) {
        v <- backsolve(fit$L, sqrt(as.vector(fit$W)) * Kx, transpose = T)
        # using correlation matrix, so diag(kxx) is all 1s, no need to compute kxx
        predvar <- 1 - crossprod(v)
        prediction <- cbind(prediction, sqrt(diag(predvar)))
        colnames(prediction) <- c("MAP", "std")
      }
    }
    prediction
  }

# predict.graf
predict.graf <-
  function(object, newdata = NULL, type = c("response", "latent"),
           CI = 0.95, maxn = NULL, ...) {
    type <- match.arg(type)
    if (is.null(maxn)) maxn <- round(nrow(object$x) / 10)
    # set up data
    if (is.null(newdata)) {
      # use already set up inference data if not specified
      newdata <- object$x
      # get mean on raw data
      mn <- object$mnfun(object$obsx)
    } else {
      # convert any ints to numerics
      for (i in 1:ncol(newdata)) if (is.integer(newdata[, i])) newdata[, i] <- as.numeric(newdata[, i])

      if (is.data.frame(newdata) & all(sapply(object$obsx, class) == sapply(newdata, class))) {

        # get mean on raw data
        mn <- object$mnfun(newdata)

        k <- ncol(newdata)
        # numericize factors
        for (fac in object$facs) {
          newdata[, fac] <- as.numeric(newdata[, fac])
        }
        # convert to a matrix
        newdata <- as.matrix(newdata)
        # scale, if needed
        if (!is.null(object$scaling)) {
          notfacs <- (1:k)
          if (length(object$facs) > 0) notfacs <- notfacs[-object$facs]
          for (i in 1:length(notfacs)) {
            newdata[, notfacs[i]] <- (newdata[, notfacs[i]] - object$scaling[1, i]) / object$   scaling[2, i]
          }
        }
      } else {
        stop("newdata must be either a dataframe with the same elements as used for inference, or NULL")
      }
    }

    # check CI
    if (!is.null(CI)) {
      if (!(CI == "std" & type == "latent")) {
        if (CI >= 1 | CI <= 0) {
          stop("CI must be a number between 0 and 1, or NULL")
        }
        err <- stats::qnorm(1 - (1 - CI) / 2)
      }
    }
    # latent case
    if (type == "latent") {
      if (is.null(CI)) {
        # if CIs aren't wanted
        ans <- pred(predx = newdata, fit = object, mn = mn, std = FALSE, maxn = maxn)
        colnames(ans) <- "posterior mean"
      } else if (CI == "std") { # if standard deviations are wanted instead
        ans <- pred(newdata, object, mn, std = TRUE, maxn = maxn)
        colnames(ans) <- c("posterior mean", "posterior std")
      } else {
        # if they are
        pred <- pred(newdata, object, mn, std = TRUE, maxn = maxn)
        upper <- pred[, 1] + err * pred[, 2]
        lower <- pred[, 1] - err * pred[, 2]
        ans <- cbind(pred[, 1], lower, upper)
        colnames(ans) <- c(
          "posterior mean", paste("lower ", round(100 * CI), "% CI", sep = ""),
          paste("upper ", round(100 * CI), "% CI", sep = "")
        )
      }
    } else {
      # response case
      if (is.null(CI)) {
        # if CIs aren't required
        ans <- stats::pnorm(pred(newdata, object, mn, std = FALSE, maxn = maxn))
        colnames(ans) <- "posterior mode"
      } else {
        # if CIs are required
        pred <- pred(newdata, object, mn, std = TRUE, maxn = maxn)
        upper <- pred[, 1] + err * pred[, 2]
        lower <- pred[, 1] - err * pred[, 2]
        ans <- stats::pnorm(cbind(pred[, 1], lower, upper))
        colnames(ans) <- c(
          "posterior mode", paste("lower ", round(100 * CI), "% CI", sep = ""),
          paste("upper ", round(100 * CI), "% CI", sep = "")
        )
      }
    }
    ans
  }
