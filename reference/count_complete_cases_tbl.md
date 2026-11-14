# Count number of complete segments of an eeg_lst object.

Count number of complete segments of an eeg_lst object.

## Usage

``` r
count_complete_cases_tbl(x, ...)
```

## Arguments

- x:

  An `eeg_lst` object.

- ...:

  Variables from the segment table to group by.

## Value

A tbl.

## See also

Other summary functions:
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`summaries`](https://bruno.nicenboim.me/eeguana/reference/summaries.md),
[`summary.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/summary.eeg_lst.md)

## Examples

``` r
if (FALSE) { # \dontrun{
faces_segs_some %>% count_complete_cases(.recording, .description)
} # }
```
