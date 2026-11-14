# Add a head shape to a ggplot

Adds the outline of a head and nose to a ggplot.

## Usage

``` r
annotate_head(size = 1.1, color = "black", stroke = 1)
```

## Arguments

- size:

  Size of the head

- color:

  Color of the head

- stroke:

  Line thickness

## Value

A layer for a ggplot

## See also

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
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
  annotate_electrodes()
```
