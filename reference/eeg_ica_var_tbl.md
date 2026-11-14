# Show the variance explained for each ICA sources.

This function shows the proportion of the variance of the channels that
each ICA component explains, as the function `eeg_pvaf()` of EEGLAB
does: one minus the variance left in the channels after removing
everything but that component, divided by the variance of the channels,
where the variance of several channels is the mean of their variances.

## Usage

``` r
eeg_ica_var_tbl(.data, ..., .max_sample = NULL)
```

## Arguments

- .data:

  An `eeg_ica_lst` object

- ...:

  Not in use.

- .max_sample:

  The maximum number of samples to use for calculating the variance
  explained. `NULL`, the default, uses all of them; a number downsamples
  the data to about that many samples first.

## Value

A table with the variance explained by each component in each recording.

## Details

If the components are uncorrelated, as the ones found with fastICA in
the data they were fitted to, the proportions of each recording add up
to one.

## See also

Other ICA functions:
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_ica_show()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_show.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)

Other summary functions:
[`count_complete_cases_tbl()`](https://bruno.nicenboim.me/eeguana/reference/count_complete_cases_tbl.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`summaries`](https://bruno.nicenboim.me/eeguana/reference/summaries.md),
[`summary.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/summary.eeg_lst.md)
