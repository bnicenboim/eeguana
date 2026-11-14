# Wrapper for FastICA methods for Independent Component Analysis from fICA package

`adapt_fast_ICA()` and `fast_ICA2()` are wrappers for
[fICA::adapt_fICA](https://rdrr.io/pkg/fICA/man/adapt_fICA.html) and
[fICA::fICA](https://rdrr.io/pkg/fICA/man/fICA.html) that only run in C,
and return a transposed version of the original estimated unmixing
matrix in W, and the mixing matrix A, consistent with the formulation
`X= S %*% A`, where X is the matrix of data with N_samples by
N_channels, and S is a matrix of sources with N_samples by N_sources.
They are meant to be used with
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md).
In order to save memory, these wrappers do not return the estimated
source components.

## Usage

``` r
adapt_fast_ICA(
  X,
  gs = fICA::gf,
  dgs = fICA::dgf,
  kj = 0,
  eps = 1e-06,
  maxiter = 1000
)

fast_ICA2(
  X,
  g = "tanh",
  dg = NULL,
  G = NULL,
  init = NULL,
  n.init = 1,
  method = "def",
  eps = 1e-06,
  maxiter = 1000
)
```

## Arguments

- X:

  A matrix of data frame.

- gs:

  a list of functions containing the nonlinearities.

- dgs:

  a list of functions containing the first derivatives of the
  nonlinearities.

- kj:

  defines the initial estimate of the unmixing matrix, see details.

- eps:

  convergence tolerance.

- maxiter:

  maximum number of iterations.

- g:

  the nonlinearity, tanh by default, see details.

- dg:

  the first derivative of the nonlinearity, see details.

- G:

  the integral function of the nonlinearity, see details.

- init:

  a numeric matrix for the initial value of the algorithm

- n.init:

  a positive integer for the number of initial values in symmetric
  algorithms, see details.

- method:

  squared symmetric (`"sym2"`), symmetric (`"sym"`) or deflation-based
  (`"def"`)

## Value

A list with the unmixing matrix W and the mixing matrix A.

## See also

Other ica methods:
[`fast_ICA()`](https://bruno.nicenboim.me/eeguana/reference/fast_ICA.md),
[`ica_matrix_lst()`](https://bruno.nicenboim.me/eeguana/reference/ica_matrix_lst.md)
