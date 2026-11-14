# Create a basic signal plot

`plot` creates a ggplot object in which the EEG signal over the whole
recording is plotted by electrode. Useful as a quick visual check for
major noise issues in the recording.

## Usage

``` r
# S3 method for class 'eeg_lst'
plot(x, .max_sample = 6400, ...)

# S3 method for class 'psd_lst'
plot(x, ...)
```

## Arguments

- x:

  An `eeg_lst` object.

- .max_sample:

  Downsample to approximately 6400 samples by default.

- ...:

  Not in use.

## Value

A ggplot object

## Details

Note that for normal-size datasets, the plot may take some time to
compile. If necessary, `plot` will first downsample the `eeg_lst` object
so that there is a maximum of 6,400 samples. The `eeg_lst` object is
then converted to a long-format tibble via `as_tibble`. In this tibble,
the `.key` variable is the channel/component name and `.value` its
respective amplitude. The sample number (`.sample` in the `eeg_lst`
object) is automatically converted to seconds to create the variable
`time`. By default, time is then plotted on the x-axis and amplitude on
the y-axis, and uses `scales = "free"`; see
[`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).

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
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

## Examples

``` r
# Basic plot
plot(data_faces_ERPs)


# Add ggplot layers
library(ggplot2)
plot(data_faces_ERPs) +
  coord_cartesian(ylim = c(-500, 500))
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
```
