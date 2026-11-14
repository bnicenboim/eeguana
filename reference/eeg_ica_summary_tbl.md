# Show a table with a summary of the results of the ICA.

This function generates a table with the variance explained by each ICA
component, and the correlations between ICA components and EOG channels.
See more details in
[eeg_ica_cor_tbl](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md)
and
[eeg_ica_var_tbl](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md).

## Usage

``` r
eeg_ica_summary_tbl(.data, ...)
```

## Arguments

- .data:

  An `eeg_ica_lst` object

- ...:

  If left empty, the function will assume that EOG channels include
  eog/EOG in their names, alternatively, EOG channels can be selected
  here.

## Value

A table with the variance explained by each component, and the
correlation between EOG channels and each components in each recording.

## See also

Other ICA functions:
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_ica_show()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_show.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)

Other summary functions:
[`count_complete_cases_tbl()`](https://bruno.nicenboim.me/eeguana/reference/count_complete_cases_tbl.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`summaries`](https://bruno.nicenboim.me/eeguana/reference/summaries.md),
[`summary.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/summary.eeg_lst.md)
