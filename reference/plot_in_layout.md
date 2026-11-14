# Arrange ERP plots according to scalp layout

Arranges a ggplot so that the facet for each channel appears in its
position on the scalp.

## Usage

``` r
plot_in_layout(plot, ...)

# S3 method for class 'gg'
plot_in_layout(plot, .projection = "polar", .ratio = c(1, 1), ...)
```

## Arguments

- plot:

  A ggplot object with channels

- ...:

  Not in use.

- .projection:

  .Projection type for converting the 3D coordinates of the electrodes
  into 2d coordinates. .Projection types available: "polar" (default),
  "orthographic", or "stereographic"

- .ratio:

  Ratio of the individual panels

## Value

A ggplot object

## Details

This function requires two steps: first, a ggplot object must be created
with ERPs faceted by channel (`.key`). Then, the ggplot object is called
in `plot_in_layout`. The function uses grobs arranged according to .x .y
coordinates extracted from the `eeg_lst` object, by default in polar
arrangement. The arrangement can be changed with the ` .projection`
argument. White space in the plot can be reduced by changing `ratio`.

Additional components such as titles and annotations should be added to
the plot object using `+` exactly as you would for
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).
Title and legend adjustments will be treated as applying to the whole
plot object, while other theme adjustments will be treated as applying
to individual facets. x-axis and y-axis labels cannot be added at this
stage.

## See also

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

Other topographic plots and layouts:
[`layout_32_1020`](https://bruno.nicenboim.me/eeguana/reference/layout_32_1020.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md)

## Examples

``` r
library(ggplot2)
# Create a ggplot object with some grand averaged ERPs
ERP_plot <- data_faces_ERPs %>%
  # select a few electrodes
  eeg_select(Fz, FC1, FC2, C3, Cz, C4, CP1, CP2, Pz) %>%
  # group by time point and condition
  eeg_group_by(.sample, condition) %>%
  # compute averages
  eeg_summarize(across_ch(mean, na.rm = TRUE)) %>%
  ggplot(aes(x = .time, y = .value)) +
  # plot the averaged waveforms
  geom_line(aes(color = condition)) +
  # facet by channel
  facet_wrap(~.key) +
  # add a legend and title
  theme(legend.position = "bottom") +
  ggtitle("ERPs for faces vs non-faces")

# Call the ggplot object with the layout function
plot_in_layout(ERP_plot)
#> Warning: `aes_()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`
#> ℹ The deprecated feature was likely used in the eeguana package.
#>   Please report the issue at <https://github.com/bnicenboim/eeguana/issues>.
```
