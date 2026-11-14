# Convert an eeg_lst to a long table in [`tidytable`](https://markfairbanks.github.io/tidytable/reference/tidytable.html) format.

Convert the signal_tbl table from wide to long format.

## Usage

``` r
as_tidytable.eeg_lst(x, .unit = "s", ...)
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

A
[`tidytable`](https://markfairbanks.github.io/tidytable/reference/tidytable.html).
