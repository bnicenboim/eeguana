# Choose samples by the position

Choose samples by their ordinal position in the signal table. Grouped
eeg_lst object use the ordinal position in the signal table within the
group.

## Usage

``` r
eeg_slice_signal(.data, ..., .preserve = FALSE)

slice_signal(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  An eeg_lst object.

- ...:

  Integer row values.

- .preserve:

  Not in use.

## See also

Other tidyverse-like functions:
[`dplyr_verbs`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md),
[`drop_incomplete_segments()`](https://bruno.nicenboim.me/eeguana/reference/drop_incomplete_segments.md),
[`eeg_bind()`](https://bruno.nicenboim.me/eeguana/reference/eeg_bind.md)
