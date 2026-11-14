# Compute the power spectral density (PSD) of an EEG signal.

Compute the power spectral density (PSD) based on an `eeg_lst` object.
For now it only uses Welch's method, so essentially it is a wrapper of
the [gsignal::pwelch](https://rdrr.io/pkg/gsignal/man/pwelch.html)
function.

## Usage

``` r
eeg_psd(
  .data,
  .method = "welch",
  .config = list(window = function(x) 2^ceiling(log2(sqrt(NROW(x)))), overlap = 0.5, nfft
    = NULL, detrend = c("long-mean", "short-mean", "long-linear", "short-linear",
    "none"), range = "half")
)
```

## Arguments

- .data:

  A `eeg_lst` object.

- .method:

  "welch" for Welch's method.

- .config:

  See [gsignal::pwelch](https://rdrr.io/pkg/gsignal/man/pwelch.html).

## Value

A `psd_lst` object

## See also

Other frequency-based functions:
[`eeg_power_band()`](https://bruno.nicenboim.me/eeguana/reference/eeg_power_band.md)

## Examples

``` r
psd_faces <- eeg_psd(data_faces_ERPs)
psd_faces %>%
  eeg_select(F3) %>%
  plot()
```
