# Display information of the eeg_lst object.

- `*_names()` functions return a vector of names,

- `n*()` return the number of elements channels or components.

## Usage

``` r
channel_names(x, ...)

channel_ica_names(x, ...)

nchannels(x, ...)

component_names(x, ...)

ncomponents(x, ...)

sampling_rate(x, ...)

nsamples(x, ...)
```

## Arguments

- x:

  An eeg_lst object.

- ...:

  Not in use.

## Details

Components are only available after running
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md).
`channel_ica_names()` refers to channels used in the ICA.

## See also

Other summary functions:
[`count_complete_cases_tbl()`](https://bruno.nicenboim.me/eeguana/reference/count_complete_cases_tbl.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`summary.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/summary.eeg_lst.md)
