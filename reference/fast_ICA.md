# Wrapper for FastICA method for Independent Component Analysis from fastICA package

`fast_ICA()` is a wrapper for
[fastICA::fastICA](https://rdrr.io/pkg/fastICA/man/fastICA.html), with
different defaults (runs in C, maximum iteration = 1000, tolerance =
1e-04, verbose), and that throws a warning in case of non-convergence.
It returns an estimated unmixing matrix W (equivalent to the original
`K %*% W`), and the mixing matrix A, consistent with the formulation
`X= S %*% A`, and `X %*% W = S` where X is the matrix of data with
N_samples by N_channels, and S is a matrix of sources with N_samples by
N_sources. They are meant to be used with
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md).

## Usage

``` r
fast_ICA(
  X,
  n.comp = NULL,
  alg.typ = "parallel",
  fun = "logcosh",
  alpha = 1,
  method = "C",
  row.norm = FALSE,
  maxit = 1000,
  tol = 1e-06,
  w.init = NULL
)
```

## Arguments

- X:

  A matrix or data frame.

- n.comp:

  number of components to be extracted

- alg.typ:

  if `alg.typ == "parallel"` the components are extracted simultaneously
  (the default). if `alg.typ == "deflation"` the components are
  extracted one at a time.

- fun:

  the functional form of the \\G\\ function used in the approximation to
  neg-entropy (see ‘details’).

- alpha:

  constant in range \[1, 2\] used in approximation to neg-entropy when
  `fun == "logcosh"`

- method:

  if `method == "R"` then computations are done exclusively in R
  (default). The code allows the interested R user to see exactly what
  the algorithm does. if `method == "C"` then C code is used to perform
  most of the computations, which makes the algorithm run faster. During
  compilation the C code is linked to an optimized BLAS library if
  present, otherwise stand-alone BLAS routines are compiled.

- row.norm:

  a logical value indicating whether rows of the data matrix `X` should
  be standardized beforehand.

- maxit:

  maximum number of iterations to perform.

- tol:

  a positive scalar giving the tolerance at which the un-mixing matrix
  is considered to have converged.

- w.init:

  Initial un-mixing matrix of dimension `c(n.comp, n.comp)`. If `NULL`
  (default) then a matrix of normal r.v.'s is used.

## Value

A list with the unmixing matrix W and the mixing matrix A.

## See also

Other ica methods:
[`fICA`](https://bruno.nicenboim.me/eeguana/reference/fICA.md),
[`ica_matrix_lst()`](https://bruno.nicenboim.me/eeguana/reference/ica_matrix_lst.md)
