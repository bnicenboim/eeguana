# Adds the electrode labels to a head shape

Adds the electrode labels to a head shape

## Usage

``` r
annotate_electrodes(.label = .key, ...)
```

## Arguments

- .label:

  Label name for the electrodes

- ...:

  other arguments to control the text

## Value

A layer for a ggplot

## See also

Other plotting functions:
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

## Examples

``` r
library(ggplot2)
data_faces_ERPs %>%
  eeg_filter(between(as_time(.sample, .unit = "milliseconds"), 100, 200)) %>%
  eeg_group_by(condition) %>%
  eeg_summarize(across_ch(mean, na.rm = TRUE)) %>%
  plot_topo() +
  annotate_head(size = .9, color = "black", stroke = 1) +
  annotate_electrodes(color = "gray")
```
