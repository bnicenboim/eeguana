# Create an ERP plot

`ggplot` initializes a ggplot object which takes an `eeg_lst` object as
its input data. Layers can then be added in the same way as for a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Usage

``` r
# S3 method for class 'eeg_lst'
ggplot(data = NULL, mapping = ggplot2::aes(), ..., .max_sample = 64000)
```

## Arguments

- data:

  An `eeg_lst` object.

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

- ...:

  Other arguments passed on to methods. Not currently used.

- .max_sample:

  Downsample to approximately 6400 samples by default.

## Value

A ggplot object

## Details

If necessary, t will first downsample the `eeg_lst` object so that there
is a maximum of 6400 samples. The `eeg_lst` object is then converted to
a long-format tibble via
[as_tibble](https://tibble.tidyverse.org/reference/as_tibble.html). In
this tibble, the `.key` variable is the channel/component name and
`.value` its respective amplitude. The sample number (`.sample` in the
`eeg_lst` object) is automatically converted to milliseconds to create
the variable `.time`. By default, time is plotted on the x-axis and
amplitude on the y-axis.

To add additional components to the plot such as titles and annotations,
simply use the `+` symbol and add layers exactly as you would for
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

## See also

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

## Examples

``` r
library(ggplot2)
# Plot grand averages for selected channels
data_faces_ERPs %>%
  # select the desired electrodes
  eeg_select(O1, O2, P7, P8) %>%
  ggplot(aes(x = .time, y = .key)) +
  # add a grand average wave
  stat_summary(
    fun.y = "mean", geom = "line", alpha = 1, linewidth = 1.5,
    aes(color = condition)
  ) +
  # facet by channel
  facet_wrap(~.key) +
  theme(legend.position = "bottom")
#> Warning: The `fun.y` argument of `stat_summary()` is deprecated as of ggplot2 3.3.0.
#> ℹ Please use the `fun` argument instead.
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

  
```
