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
#' `adapt_fast_ICA()` and `fast_ICA2()` are wrappers for [fICA::adapt_fICA] and [fICA::fICA] that only run in C, and return a transposed
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
    n <- nrow(X)
    p <- ncol(X)
    eps <- p * eps
    init_est <- "k-JADE"
    if (!(kj %in% 1:p)) {
      W0 <- JADE::FOBI(X)$W
      init_est <- "FOBI"
    } else {
      W0 <- JADE::k_JADE(X, k = kj, eps = eps, maxiter = maxiter)$W
      init_est <- paste(kj, "-JADE", sep = "")
    }
    X <- tcrossprod(X, W0)
    X <- sweep(X, 2, colMeans(X))
    name <- fICA::gnames
    # garbage collection before calling the function:
    # TODO: may need to clean this up later
    if ("full" %in% methods::formalArgs(gc)) {
      gc(full = TRUE)
    } else {
      # older version of R
      gc()
    }

    res <- .Call("adfica", X, eps, maxiter, PACKAGE = "fICA")
    cnam <- paste("comp", 1:p)
    V <- res$W
    alphas <- res$alphas
    ord <- res$ord + 1
    usedg <- res$usedg + 1
    used_gs <- NULL
    for (i in 1:(p - 1)) {
      used_gs[i] <- name[usedg[i]]
    }
    if (length(ord) == (p - 1)) {
      ord[p] <- sum(1:p) - sum(ord)
    } else {
      ord <- 1:p
    }
    W <- crossprod(V, W0)
    W <- t(crossprod(diag(sign(rowMeans(W))), W))
    alphas <- matrix(alphas[, ord], ncol = p)
    dimnames(alphas) <- list(name, cnam)
    A <- MASS::ginv(W)
    list(W = W, A = A)
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
