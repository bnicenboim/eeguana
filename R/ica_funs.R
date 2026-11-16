#' Wrapper for FastICA method for Independent Component Analysis from fastICA package
#'
#' `fast_ICA()` is a wrapper for [fastICA::fastICA], with different defaults (runs in C, maximum iteration = 1000, tolerance = 1e-04, verbose), and that throws a warning in case of non-convergence. It returns an estimated unmixing matrix W (equivalent to the original `K %*% W`), and the mixing matrix A, consistent with the formulation `X= S %*% A`, and `X %*% W = S` where X is the matrix of data with N_samples by N_channels, and S is a matrix of sources with N_samples by N_sources. They are meant to be  used with [eeg_ica()].
#'
#' @param X A matrix or data frame.
#' @family ica methods
#' @inheritParams fastICA::fastICA
#' @return A list with the unmixing matrix W and the mixing matrix A.
#' @export
fast_ICA <- function(X, n.comp = NULL, alg.typ = "parallel",
                     fun = "logcosh", alpha = 1.0, method = "C",
                     row.norm = FALSE, maxit = 1000, tol = 1e-06,
                     w.init = NULL) {
  if (is.null(n.comp)) {
    n.comp <- ncol(X)
  }
  capture <- utils::capture.output(res <- fastICA::fastICA(
    X = X, n.comp = n.comp, alg.typ = alg.typ,
    fun = fun, alpha = alpha, method = method,
    row.norm = row.norm, maxit = maxit, tol = tol, verbose = TRUE,
    w.init = w.init
  ))
  last_line <- chr_match(capture[length(capture)], "Iteration (\\d*) tol=(.*)")

  out_tol <- as.numeric(last_line[, 3])
  out_maxit <- as.numeric(last_line[, 2])
  if (out_tol > tol) {
    warning("fastICA didn't converge. Expected tolerance: ", tol,
      ". Tolerance: ", res$tol, ". Increase the number of iterations to more than ",
      maxit, " with `maxit`",
      call. = FALSE
    )
  }
  message_verbose(
    "# ICA finished in ", out_maxit, " iterations. With a tolerance of ", out_tol,
    ". (Maximum expected tolerance: ", tol, ".)"
  )
  list(A = res$A, W = res$K %*% res$W)
}


#' Wrapper for FastICA methods for Independent Component Analysis from fICA package
#'
#' `adapt_fast_ICA()` implements the adaptive deflation-based FastICA of [fICA::adapt_fICA] in R,
#' with the fixed-point update written as a matrix-vector product, so that no samples-by-channels
#' matrix is allocated inside the iterations; on long recordings this is much faster than fICA's
#' C code. `fast_ICA2()` is a wrapper for [fICA::fICA] that only runs in C. Both return a transposed
#' version
#' of the original estimated unmixing matrix in W, and the mixing matrix A,
#' consistent with the formulation `X= S %*% A`, where X is the matrix of data with
#'  N_samples by N_channels, and S is a matrix of sources with N_samples by N_sources.
#'  They are meant to be  used with [eeg_ica()]. In order to save memory, these wrappers do not
#'  return the estimated
#' source components.
#'
#' @param X A matrix of data frame.
#' @family ica methods
#' @inheritParams fICA::adapt_fICA
#' @inheritParams fICA::fICA
#' @return A list with the unmixing matrix W and the mixing matrix A.
#' @name fICA
NULL
# > NULL

#' @rdname fICA
#' @export
adapt_fast_ICA <-
  function(X, gs = fICA::gf, dgs = fICA::dgf, kj = 0, eps = 1e-06, maxiter = 1000) {
    X <- as.matrix(X)
    p <- ncol(X)
    eps <- p * eps
    if (!(kj %in% 1:p)) {
      W0 <- JADE::FOBI(X)$W
    } else {
      W0 <- JADE::k_JADE(X, k = kj, eps = eps, maxiter = maxiter)$W
    }
    Z <- tcrossprod(X, W0)
    rm(X)
    Z <- sweep(Z, 2, colMeans(Z))
    V <- adaptive_deflation(Z, gs, dgs, eps, maxiter)
    W <- crossprod(V, W0)
    W <- t(crossprod(diag(sign(rowMeans(W))), W))
    A <- MASS::ginv(W)
    list(W = W, A = A)
  }

## Adaptive deflation-based FastICA (Miettinen et al., 2014), as fICA's adfica
## does it, but with the update written as a matrix-vector product, so that no
## samples-by-channels matrix is allocated inside the iterations.
## Z is whitened and centered. Returns the p x p matrix of unit vectors V.
adaptive_deflation <- function(Z, gs, dgs, eps, maxiter) {
  p <- ncol(Z)
  alphas <- asymptotic_variances(Z, gs, dgs)
  alphas[alphas <= 0] <- Inf
  V <- matrix(0, p, p)
  for (j in seq_len(p - 1)) {
    repeat {
      if (all(is.infinite(alphas))) stop("no convergence")
      ## first minimum, scanning the components within each nonlinearity, as fICA does
      am <- which(t(alphas) == min(alphas), arr.ind = TRUE)[1, ]
      g <- am[[2]]
      comp <- am[[1]]
      w0 <- diag(p)[, comp]
      w <- one_component(Z, gs[[g]], dgs[[g]], eps, maxiter, w0, V)
      if (!is.null(w)) {
        V[, j] <- w
        alphas[, comp] <- Inf
        break
      }
      alphas[g, comp] <- Inf
    }
  }
  w <- diag(p)[, 1]
  w <- w - V %*% crossprod(V, w)
  V[, p] <- w / sqrt(sum(w^2))
  V
}

## alpha_{g, j}: asymptotic variance of the estimate of component j with
## nonlinearity g, which decides which nonlinearity is used for which component.
asymptotic_variances <- function(Z, gs, dgs) {
  p <- ncol(Z)
  alphas <- matrix(0, length(gs), p)
  for (j in seq_len(p)) {
    z <- Z[, j]
    for (i in seq_along(gs)) {
      gz <- as.vector(gs[[i]](z))
      Eg <- mean(gz)
      Eg2 <- mean(gz^2)
      Egz <- mean(gz * z)
      Edg <- mean(dgs[[i]](z))
      if (Egz != Edg) {
        alphas[i, j] <- (Eg2 - Eg^2 - Egz^2) / (Egz - Edg)^2
      }
    }
  }
  alphas
}

## One FastICA fixed-point iteration with the stabilization of fICA's adfica,
## orthogonal to the components in V. NULL when it does not converge.
one_component <- function(Z, g, dg, eps, maxiter, w0, V) {
  n <- nrow(Z)
  VVt <- tcrossprod(V)
  wn <- w0
  iter <- 0L
  restarts <- 0L
  repeat {
    iter <- iter + 1L
    w <- wn
    zw <- Z %*% w
    gzw <- as.vector(g(zw))
    wn <- as.vector(crossprod(Z, gzw)) / n - mean(dg(zw)) * w
    wn <- wn / sqrt(sum(wn^2))
    if (sqrt(sum((wn - w)^2)) > 1) wn <- -wn
    if (restarts > 0L) {
      wn <- (1 - 1 / 5) * wn + w / 5
    } else {
      wn <- (1 - 1 / (iter + 20)) * wn + w / (iter + 20)
    }
    wn <- wn - as.vector(VVt %*% wn)
    wn <- wn / sqrt(sum(wn^2))
    if (sqrt(sum((w - wn)^2)) < eps || sqrt(sum((w + wn)^2)) < eps) {
      return(wn)
    }
    if (iter == maxiter) {
      restarts <- restarts + 1L
      iter <- 0L
      if (restarts > 10L) return(NULL)
    }
  }
}

#' @rdname fICA
#' @export
fast_ICA2 <- function(X, g = "tanh", dg = NULL, G = NULL, init = NULL, n.init = 1,
                      method = "def", eps = 1e-06, maxiter = 1000) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  eps <- p * eps
  name <- c("pow3", "tanh", "gaus")
  method <- match.arg(method, c("sym2", "sym", "def"))
  S0 <- stats::cov(X)
  EVD <- eigen(S0, symmetric = TRUE)
  S0.5 <- EVD$vectors %*% tcrossprod(
    diag(EVD$values^(0.5)),
    EVD$vectors
  )
  S0.5inv <- EVD$vectors %*% tcrossprod(
    diag(EVD$values^(-0.5)),
    EVD$vectors
  )
  X <- tcrossprod(sweep(X, 2, colMeans(X)), S0.5inv)
  if (is.null(init)) {
    VN <- diag(p)
  } else {
    mat.sqrt <- function(A) {
      eig <- eigen(A, symmetric = TRUE)
      eig$vectors %*% (diag(eig$values^(1 / 2))) %*% t(eig$vectors)
    }

    VN <- crossprod(t(init), S0.5)
    VN <- crossprod(
      solve(mat.sqrt(tcrossprod(VN, VN))),
      VN
    )
  }
  V <- switch(method,
    sym2 = {
      if (!(is.function(g) && is.function(dg) && is.function(G))) {
        gi <- which(name == g[1])
        g1 <- fICA::gf[[gi]]
        dg1 <- fICA::dgf[[gi]]
        G1 <- fICA::Gf[[gi]]
      } else {
        g1 <- g
        dg1 <- dg
        G1 <- G
      }
      stop("c++ code not available for method sym2 yet. Use fICA::fICA")
    },
    sym = {
      gi <- which(name == g[1])
      V <- .Call("ficasym", X, gi, VN, p * eps, maxiter,
        PACKAGE = "fICA"
      )
    },
    def = {
      gi <- which(name == g[1])
      V <- .Call("ficadef", X, gi, VN, eps, maxiter, PACKAGE = "fICA")
    }
  )
  if (sum(abs(V$W)) > 0) {
    W <- crossprod(V$W, S0.5inv)
    W <- t(crossprod(diag(sign(rowMeans(W))), W))
  } else {
    stop("no convergence")
  }
  A <- MASS::ginv(W)
  list(W = W, A = A)
}
