# Add independent components (or sources) to the signal table for visualization.

Add independent components (or sources) to the signal table for
visualization.

## Usage

``` r
eeg_ica_show(.data, ...)
```

## Arguments

- .data:

  An eeg_ica_lst object

- ...:

  Components to extract from the mixing matrix of the ICA
  transformation.

## See also

Other ICA functions:
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)

## Examples

``` r

# For demonstration only, since ICA won't converge
library(ggplot2)
# Suppressing an important warning:
suppressWarnings(data_faces_10_trials %>%
  eeg_ica(-EOGH, -EOGV, -M1, -M2, .method = fast_ICA, .config = list(maxit = 10))) %>%
  eeg_ica_show(ICA1) %>%
  plot()
#> # ICA is being done using fast_ICA...
#> # ICA finished in 9 iterations. With a tolerance of 0.962562. (Maximum expected tolerance: 1e-06.)
#> # ICA took 0.67 secs
#> # Downsampling from 500Hz to 250Hz.
#> # Object size in memory 6.1 Mb
```
