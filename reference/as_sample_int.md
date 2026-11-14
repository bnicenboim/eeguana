# Convert a time point into a sample.

Convert a time point into a sample.

## Usage

``` r
as_sample_int(x, ...)

# S3 method for class 'numeric'
as_sample_int(x, .sampling_rate = NULL, .unit = "s", ...)
```

## Arguments

- x:

  A vector of numeric values.

- ...:

  Not in use.

- .sampling_rate:

  Sampling rate in Hz

- .unit:

  "seconds" (or "s"), "milliseconds" (or "ms"), or "samples"

## Value

A sample_int object.
