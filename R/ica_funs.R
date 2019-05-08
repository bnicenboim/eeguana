#' Wrapper for   FastICA methods for Independent Component Analysis from fICA package
#' 
#' This is a wrapper for [fICA::adapt_fICA] and [fICA::fICA] that only run in C, and return a transposed
#' version 
#' of the original estimated unmixing matrix in W, and the mixing matrix A,
#' consistent with the formulation `X= S %*% A`, where X is the matrix of data with
#'  N_samples $\times$ N_channels, and S is a matrix of sources with N_samples $\times$ N_sources. 
#'  They are meant to be  used with [eeg_ica()]. In order to save memory, these wrappers do not
#'  return the estimated 
#' source components.
#' 
#' @param X A matrix of data frame.
#'
#' @inheritParams fICA::adapt_fICA
#' @inheritParams fICA::fICA
#' @return A list with the unmixing matrix W and the mixing matrix A.
#' @name ica
NULL
# > NULL

#' @rdname ica
#' @export
adapt_fast_ICA <-
function (X, gs = fICA::gf, dgs = fICA::dgf, kj = 0, eps = 1e-06, maxiter = 1000) 
{
    X <- as.matrix(X)
    n <- nrow(X)
    p <- ncol(X)
    eps <- p * eps
    init_est <- "k-JADE"
    if (!(kj %in% 1:p)) {
        W0 <- JADE::FOBI(X)$W
        init_est <- "FOBI"
    }
    else {
        W0 <- JADE::k_JADE(X, k = kj, eps = eps, maxiter = maxiter)$W
        init_est <- paste(kj, "-JADE", sep = "")
    }
    Z <- tcrossprod(X, W0)
    Z <- sweep(Z, 2, colMeans(Z))
        name <- fICA::gnames
        res <- .Call("adfica", Z, eps, maxiter, PACKAGE = "fICA")
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
    }
    else ord <- 1:p
    W <- crossprod(V, W0)
    W <- t(crossprod(diag(sign(rowMeans(W))), W))
    alphas <- matrix(alphas[, ord], ncol = p)
    dimnames(alphas) <- list(name, cnam)
    A =  MASS::ginv(W)
     list(W = W, A=A)
}

#' @rdname ica
#' @export
fast_ICA <- function (X, g = "tanh", dg = NULL, G = NULL, init = NULL, n.init = 1, 
          method = "sym2", eps = 1e-06, maxiter = 1000) 
{
    X <- as.matrix(X)
    n <- nrow(X)
    p <- ncol(X)
    eps <- p * eps
    name <- c("pow3", "tanh", "gaus")
    method <- match.arg(method, c("sym2", "sym", "def"))
    S0 <- cov(X)
    EVD <- eigen(S0, symmetric = TRUE)
    S0.5 <- EVD$vectors %*% tcrossprod(diag(EVD$values^(0.5)), 
                                       EVD$vectors)
    S0.5inv <- EVD$vectors %*% tcrossprod(diag(EVD$values^(-0.5)), 
                                          EVD$vectors)
    Z <- tcrossprod(sweep(X, 2, colMeans(X)), S0.5inv)
    if (is.null(init)) {
        VN <- diag(p)
    }
    else {
        VN <- crossprod(t(init), S0.5)
        VN <- crossprod(solve(mat.sqrt(tcrossprod(VN, VN))), 
                        VN)
    }
    V <- switch(method, sym2 = {
            if (!(is.function(g) && is.function(dg) && is.function(G))) {
                gi <- which(name == g[1])
                g1 <- gf[[gi]]
                dg1 <- dgf[[gi]]
                G1 <- Gf[[gi]]
            } else {
                g1 <- g
                dg1 <- dg
                G1 <- G
            }
            warning("c++ code not available for method sym2 yet. Computations are done in R.")
            V <- fICA.sym2(Z, VN, g = g1, dg = dg1, G = G1, n.init = n.init, 
                           eps = p * eps, maxiter = maxiter)
    }, sym = {
        
            gi <- which(name == g[1])
            V <- .Call("ficasym", Z, gi, VN, p * eps, maxiter, 
                       PACKAGE = "fICA")
        
    }, def = {
            gi <- which(name == g[1])
            V <- .Call("ficadef", Z, gi, VN, eps, maxiter, PACKAGE = "fICA")
    })
    if (sum(abs(V$W)) > 0) {
        W <- crossprod(V$W, S0.5inv)
        W <- tcrossprod(diag(sign(rowMeans(W))), W)
    }
    else stop("no convergence")
    A =  MASS::ginv(W)
    list(W = W, A=A)
}