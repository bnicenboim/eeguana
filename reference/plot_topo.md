# Create a topographic plot

`plot_topo` initializes a ggplot object which takes an `eeg_lst` object
as its input data. Layers can then be added in the same way as for a
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Usage

``` r
plot_topo(data, ...)

# S3 method for class 'data.frame'
plot_topo(data, .value = .value, .label = .key, ...)

# S3 method for class 'eeg_lst'
plot_topo(data, .projection = "polar", ...)
```

## Arguments

- data:

  A table of interpolated electrodes as produced by
  `eeg_interpolate_tbl`, or an `eeg_lst`, or `ica_lst` appropiately
  grouped.

- ...:

  If data are an `eeg_lst` or `ica_lst`, these are arguments passed to
  `eeg_interpolate_tbl`, such as, radius, size, etc.

- .value:

  Values used for the interpolation, generally `.value` (default).

- .label:

  Label of the points that are used for the interpolation, generally
  `.key` (default).

- .projection:

  .Projection type for converting the 3D coordinates of the electrodes
  into 2d coordinates. .Projection types available: "polar" (default),
  "orthographic", or "stereographic"

## Value

A ggplot object

## Details

Before calling `plot_topo`, the `eeg_lst` object object must be
appropriately grouped (e.g. by condition) and/or summarized into mean
values such that each .x .y coordinate has only one amplitude value. By
default, `plot_topo` interpolates amplitude values via
`eeg_interpolate_tbl`, which generates a tibble with `.key` (channel),
`.value` (amplitude), and .x .y coordinate variables. .x .y coordinates
are extracted from the `eeg_lst` object, which in turn reads the
coordinates logged by your EEG recording software. By default,
`plot_topo` will display electrodes in polar arrangement, but can be
changed with the `projection` argument. Alternatively, if
`eeg_interpolate_tbl` is called after grouping/summarizing but before
`plot_topo`, the resulting electrode layout will be stereographic.

`plot_topo` called alone without any further layers will create a
non-annotated topographical plot. To add a head and nose, add the layer
`annotate_head`. Add contour lines with
[`ggplot2::geom_contour`](https://ggplot2.tidyverse.org/reference/geom_contour.html)
and electrode labels with
[`ggplot2::geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html).
These arguments are deliberately not built into the function so as to
allow flexibility in choosing color, font size, and even head size, etc.

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
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

Other topographic plots and layouts:
[`layout_32_1020`](https://bruno.nicenboim.me/eeguana/reference/layout_32_1020.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md)

## Examples

``` r
library(ggplot2)
# Calculate mean amplitude between 100-200 ms and plot the topography
data_faces_ERPs %>%
  # select the time window of interest
  eeg_filter(between(as_time(.sample, .unit = "milliseconds"), 100, 200)) %>%
  # compute mean amplitude per condition
  eeg_group_by(condition) %>%
  eeg_summarize(across_ch(mean, na.rm = TRUE)) %>%
  plot_topo() +
  # add a head and nose shape
  annotate_head() +
  # add electrode labels
  annotate_electrodes(color = "black") +
  facet_grid(~condition)


# The same but with interpolation
data_faces_ERPs %>%
  eeg_filter(between(as_time(.sample, .unit = "milliseconds"), 100, 200)) %>%
  eeg_group_by(condition) %>%
  eeg_summarize(across_ch(mean, na.rm = TRUE)) %>%
  eeg_interpolate_tbl() %>%
  plot_topo() +
  annotate_head() +
  annotate_electrodes(color = "black") +
  facet_grid(~condition)
```
