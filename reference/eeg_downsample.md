# Downsample EEG data

Downsample a signal_tbl by a factor `q`, using an FIR or IIR filter.
This is a wrapper for
[`decimate`](https://rdrr.io/pkg/gsignal/man/decimate.html) from the
`gsignal` package, see its documentation for details.

## Usage

``` r
eeg_downsample(
  .data,
  .q = 2,
  .max_sample = NULL,
  .n = if (.ftype == "iir") 8 else 30,
  .ftype = "iir",
  .multiple_times = FALSE,
  ...
)
```

## Arguments

- .data:

  An eeg_lst object.

- .q:

  integer factor(s) to downsample by.

- .max_sample:

  Optionally, the (approximated) maximum sample number can be defined
  here, which is at least half of the total number of samples

- .n:

  Order of the filter used prior to the downsampling, specified as a
  positive integer. Default: 8 if `ftype` equals `"iir"`; 30 of `ftype`
  equals `"fir"`.

- .ftype:

  filter type; either `"fir"`, specifying a FIR filter of length `n`
  designed with the function
  [`fir1`](https://rdrr.io/pkg/gsignal/man/fir1.html), or `"iir"`
  (default), specifying an IIR Chebyshev filter of order 8 using the
  function [`cheby1`](https://rdrr.io/pkg/gsignal/man/cheby1.html).

- .multiple_times:

  Indicates whether to factorize \`q“ and apply the downsampling in
  steps.

- ...:

  Not in use.

## Details

A factor q larger than 13 can result in NAs. To avoid this, the
downsampling can be done in steps. For example, instead of setting
`q = 20`, it is possible to set `q = c(2,10)`.

## See also

Other preprocessing functions:
[`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_rereference()`](https://bruno.nicenboim.me/eeguana/reference/eeg_rereference.md),
[`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md),
[`filt`](https://bruno.nicenboim.me/eeguana/reference/filt.md)

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)
