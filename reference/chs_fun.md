# Get the by-sample (or by-row) function of the specified channels.

Get the by-sample (or by-row) function of the specified channels.

## Usage

``` r
chs_fun(x, .funs, ..., .pars = list())
```

## Arguments

- x:

  An eeg_lst.

- .funs:

  A function `fun`, a quosure style lambda `~ fun(.)` or a list of
  either form.

- ...:

  A group of channels, it can be used in combination with `across`.

- .pars:

  List that contains the additional arguments for the function calls in
  .funs.

## Value

A new channel or an `eeg_lst` object with a channel where a function was
instead of the previous channels.

## See also

Other channel functions:
[`chs_mean()`](https://bruno.nicenboim.me/eeguana/reference/chs_mean.md)
