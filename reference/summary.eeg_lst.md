# Summary of eeg_lst information.

It's also possible to extract the elements by accessing it as a list.

## Usage

``` r
# S3 method for class 'eeg_lst'
summary(object, ...)
```

## Arguments

- object:

  An eeg_lst object.

- ...:

  additional arguments affecting the summary produced.

## See also

Other summary functions:
[`count_complete_cases_tbl()`](https://bruno.nicenboim.me/eeguana/reference/count_complete_cases_tbl.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`summaries`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)

## Examples

``` r

summary(data_faces_ERPs)
#> # EEG data:
#> # Sampling rate: 500 Hz.
#> # Size in memory: 180.5 Kb.
#> # Total duration: 00:00:00.
#> # Summary of segments
#>    .recording n_segments n_incomplete
#>        <char>      <int>        <int>
#> 1: faces.vhdr          2            0
#> # Summary of events
#> Empty data.table (0 rows and 3 cols): .type,.description,n
```
