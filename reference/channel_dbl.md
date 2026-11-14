# Builds a channel.

Builds a channel from a vector of numbers.

## Usage

``` r
channel_dbl(
  values,
  x = NA_real_,
  y = NA_real_,
  z = NA_real_,
  reference = NA,
  ...
)
```

## Arguments

- values:

  Vector of doubles indicating amplitudes.

- x:

  Position in the scalp.

- y:

  Position in the scalp.

- z:

  Position in the scalp.

- reference:

  Reference electrode.

- ...:

  further arguments passed to or from other methods.

## Value

A channel_dbl.

## See also

Other channel:
[`as_channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/as_channel_dbl.md),
[`is_channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/is_channel_dbl.md)

## Examples

``` r

Cz <- channel_dbl(runif(100, -5, 5))
```
