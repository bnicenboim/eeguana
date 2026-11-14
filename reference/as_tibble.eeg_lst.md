# Convert an eeg_lst to a long table in [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) format.

Convert the signal_tbl table from wide to long format.

## Usage

``` r
as_tibble.eeg_lst(x, .unit = "s", ...)

as_data_frame.eeg_lst(x, .unit = "s", ...)

# S3 method for class 'eeg_lst'
as_long_tbl(x, .unit = "s", ...)
```

## Arguments

- x:

  An `eeg_lst` object.

- .unit:

  Unit for the `.time` column of the transformed object: "s" (default),
  "ms", "samples".

- ...:

  Additional arguments to be passed to or from other methods.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

Other tibble:
[`as.data.frame.psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/as.data.frame.psd_lst.md),
[`as_tibble.psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.psd_lst.md)
