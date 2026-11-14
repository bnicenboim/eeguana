# Convert a psd_lst to a (base) data frame.

Convert a psd_lst to a (base) data frame.

## Usage

``` r
# S3 method for class 'psd_lst'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `psd_lst` object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If `TRUE`, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html)) is optional.
  Note that all of R's base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use `optional` only for column names treatment, basically with the
  meaning of
  [`data.frame`](https://rdrr.io/r/base/data.frame.html)`(*, check.names = !optional)`.
  See also the `make.names` argument of the `matrix` method.

- ...:

  Additional arguments to be passed to or from other methods.

## Value

A data.frame.

## See also

Other tibble:
[`as_tibble.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.eeg_lst.md),
[`as_tibble.psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.psd_lst.md)
