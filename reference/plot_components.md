# Generates topographic plots of the components after running ICA on an eeg_lst

Note that unlike
[plot_topo](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
there is no need for faceting, or adding layers.

## Usage

``` r
plot_components(data, ..., .projection = "polar", .standardize = TRUE)
```

## Arguments

- data:

  An eeg_ica_lst

- ...:

  arguments passed to interpolate.

- .projection:

  .Projection type for converting the 3D coordinates of the electrodes
  into 2d coordinates. .Projection types available: "polar" (default),
  "orthographic", or "stereographic"

- .standardize:

  Whether to standardize the color scale of each topographic plot.

## See also

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

Other ICA functions:
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_ica_show()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_show.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md)

Other topographic plots and layouts:
[`layout_32_1020`](https://bruno.nicenboim.me/eeguana/reference/layout_32_1020.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md)

## Examples

``` r
# For demonstration only, since ICA won't converge
library(ggplot2)
# Suppressing an important warning:
suppressWarnings(data_faces_10_trials %>%
  eeg_ica(-EOGH, -EOGV, -M1, -M2, .method = fast_ICA, .config = list(maxit = 10))) %>%
  eeg_ica_keep(ICA1, ICA2) %>%
  plot_components() +
  annotate_head() +
  geom_contour() +
  annotate_electrodes(color = "black") +
  theme(legend.position = "none")
#> # ICA is being done using fast_ICA...
#> # ICA finished in 9 iterations. With a tolerance of 0.847028. (Maximum expected tolerance: 1e-06.)
#> # ICA took 0.54 secs
```
