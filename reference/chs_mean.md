# Get the by-sample (or by-row) mean of the specified channels.

Wrapper of `rowMeans` that performs a by-sample mean of the specified
channels.

## Usage

``` r
chs_mean(x, ..., na.rm = FALSE)
```

## Arguments

- x:

  An `eeg_lst` object.

- ...:

  A group of channels, it can be used in combination with `across`.

- na.rm:

  a logical evaluating to `TRUE` or `FALSE` indicating whether `NA`
  values should be stripped before the computation proceeds.

## Value

A new channel or an `eeg_lst` object with a `mean` channel instead of
the previous channels.

## See also

Other channel functions:
[`chs_fun()`](https://bruno.nicenboim.me/eeguana/reference/chs_fun.md)

## Examples

``` r
if (FALSE) { # \dontrun{
faces_segs_some %>%
  eeg_transmute(
    Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
    Parietal = chs_mean(P3, P4, P7, P8, Pz, na.rm = TRUE)
  )

faces_segs_some %>%
  eeg_transmute(
    Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
    Parietal = chs_mean(P3, P4, P7, P8, Pz, na.rm = TRUE)
  )
faces_seg %>%
  eeg_transmute(
    Occipital = chs_mean(across(starts_with("O")), na.rm = TRUE), # O1, O2, Oz
    Parietal = chs_mean(across(starts_with("O")), na.rm = TRUE) # P3, P4, P7, P8, Pz
  )
faces_segs_some %>%
  chs_mean(na.rm = TRUE)
} # }
```
