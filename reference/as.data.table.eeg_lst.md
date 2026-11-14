# Convert an eeg_lst to a long table in [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) format.

Convert the signal_tbl table from wide to long format.

## Usage

``` r
as.data.table.eeg_lst(x, .unit = "s", ...)
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

A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html).
