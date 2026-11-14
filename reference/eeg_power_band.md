# Compute power bands.

Compute power bands.

## Usage

``` r
eeg_power_band(
  .data,
  .bands = list(delta = c(0.5, 4), theta = c(4, 8), alpha = c(8, 13), beta = c(13, 30)),
  .relative = FALSE,
  ...
)
```

## Arguments

- .data:

  An `eeg_lst` or `psd_lst`.

- .bands:

  A named list of frequency bands.

- .relative:

  Calculate band power relative to total power.

- ...:

  Other arguments passed to
  [`eeg_psd()`](https://bruno.nicenboim.me/eeguana/reference/eeg_psd.md)

## See also

Other frequency-based functions:
[`eeg_psd()`](https://bruno.nicenboim.me/eeguana/reference/eeg_psd.md)
