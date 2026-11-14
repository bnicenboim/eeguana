# Adds a layer with the events on top of a plot of an eeg_lst.

Adds a layer with the events on top of a plot of an eeg_lst.

## Usage

``` r
annotate_events(data = NULL, alpha = 0.2)
```

## Arguments

- data:

  The data to be displayed in this layer. There are three options:

  - If NULL, the default, the events table is inherited from the plot
    data as specified in the call to ggplot().

  - An events table will override the plot events table data.

- alpha:

  new alpha level in 0,1.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) layer.

## See also

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)
