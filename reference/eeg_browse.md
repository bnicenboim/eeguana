# Browse the signal or the ICA components interactively

`eeg_browse()` opens a Shiny app to look through the data. For an
`eeg_lst`, it shows the signal of the channels and, on request, the
topography of the window, and segments can be selected, for example the
ones to remove. For an `eeg_ica_lst`, created with
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
it shows the activations of the components, together with the EOG
channels or any other channels, and the topography of each component,
labeled with the proportion of the variance of the channels it explains
(see
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md))
and its correlation with the EOG channels (see
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md)),
and components can be selected, for example the ones to remove.

## Usage

``` r
eeg_browse(.data, ...)

# S3 method for class 'eeg_lst'
eeg_browse(
  .data,
  ...,
  .negative_up = getOption("eeguana.negative_up", FALSE),
  .viewer = NULL
)

# S3 method for class 'eeg_ica_lst'
eeg_browse(
  .data,
  ...,
  .eog = NULL,
  .eog_freq = c(0.1, 30),
  .n_components = 16,
  .negative_up = getOption("eeguana.negative_up", FALSE),
  .viewer = NULL
)
```

## Arguments

- .data:

  An `eeg_lst`, or an `eeg_ica_lst` created with
  [`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md).

- ...:

  For an `eeg_lst`, the channels shown when the app opens, all of them
  by default. Not used for an `eeg_ica_lst`.

- .negative_up:

  Whether negative amplitudes are drawn up, as is usual in some EEG
  traditions. By default, the value of the option `eeguana.negative_up`,
  which is `FALSE`, positive up. It can also be changed in the app.

- .viewer:

  Where the app opens, see
  [shiny::viewer](https://rdrr.io/pkg/shiny/man/viewer.html). By
  default, in the browser.

- .eog:

  Names of the EOG channels, used for the correlations and shown under
  the components when the app opens. By default, the channels whose
  names start or end with "eog", ignoring case.

- .eog_freq:

  Cutoff frequencies in Hz of the filter applied to the EOG channels
  before correlating them with the components: a band-pass filter from
  0.1 to 30 Hz by default. With `NA` as the first value, it is only a
  low-pass filter, and with `NA` as the second, only a high-pass filter.
  `NULL` does not filter. It can also be changed in the app.

- .n_components:

  Number of components shown when the app opens, in the order of the
  variance they explain.

## Value

For an `eeg_lst`, invisibly, the `.id` of the segments selected. A
message lists them, and shows the calls to
[`eeg_filter()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
that keep only them or remove them. When each recording is a single
segment, `NULL`, invisibly, and no message.

For an `eeg_ica_lst`, invisibly, a list with one element per recording,
holding the names of the components selected. A message lists them, and
shows the calls to
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md)
that keep only them or remove them.

## Details

The window can be placed around events and moved from one event to the
next, or it can be moved through the recording. For an `eeg_lst`, the
app opens moving through the recording, and for an `eeg_ica_lst`, around
events, when there are any. The window goes on from one segment into the
next, and each segment is shown in its own column. Through the
recording, the start of the window is set relative to the time zero of
its segment, or with a slider over the whole recording, with the
segments one after the other; the window starts where it is asked, and
near the end it stops with the recording. With several recordings, the
buttons "Previous recording" and "Next recording", or the field, change
the recording shown, whose name is in the title. The events are chosen
by their type or their description: the ones that are equal to some
values, start with, end with, or contain some text, or match a regular
expression. For example, the descriptions that start with "peak" are the
blinks found with
[`eeg_artif_peak()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md).
The limits of the window can be set in samples, milliseconds, or
seconds. The left and right arrow keys move to the previous and next
window, and the up and down arrow keys zoom the amplitudes in and out.

The amplitudes are shown in units of their typical standard deviation:
the median, over stretches of one second of the whole recording, of
their standard deviations, each estimated with the median absolute
deviation (MAD, see [`stats::mad()`](https://rdrr.io/r/stats/mad.html)),
which blinks and other large events barely change. So a window with a
blink and one without are drawn at the same scale, and slow drifts do
not flatten the traces. By default, all the channels share one scale,
and all the components another, so their sizes can be compared; each
trace can also be drawn at its own scale, which makes the small ones
visible. Large amplitudes go on into the rows of the traces around them,
unless the option to cut the traces at the edges of their rows is on.

The app needs the packages shiny and bslib.

## Selecting segments of an `eeg_lst`

Clicking a segment on the signal selects it, and clicking it again
deselects it; the S key and the "Select segment" button do the same for
the segment of the event, or for the first segment shown. Selected
segments are tinted red. "Done" closes the app and returns the `.id` of
the selected segments; closing the window returns them as well. What to
do with them is up to
[`eeg_filter()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md):
keeping only them, or removing them. Only segmented data can be
selected: when each recording is a single segment, as before
[`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md),
the app is only for browsing, and returns `NULL`.

The topography is in a sidebar on the right, closed at first, and it is
computed only while it is open. It shows the mean of each channel over
the part of the window in each segment, one head per segment; a box
shows one head for all the segments of the window, their mean, instead.
It needs the positions of the electrodes, see
[`channels_tbl()`](https://bruno.nicenboim.me/eeguana/reference/channels_tbl.md).

## Selecting ICA components

Clicking a topography selects the component, and clicking it again
deselects it. "Done" closes the app and returns the selected components;
closing the window returns them as well. What to do with them is up to
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md):
keeping only them, or removing them.

The EOG channels are filtered before they are correlated with the
components, because slow drifts and offsets can hide how closely they
follow a component. In the intro vignette, the blink component
correlates 0.31 with the unfiltered VEOG and 0.91 with the filtered one.
The filter only affects the correlations: the channels are shown
unfiltered.

## See also

Other ICA functions:
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_ica_show()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_show.md),
[`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
[`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)

Other plotting functions:
[`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md),
[`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md),
[`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md),
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`ggplot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md),
[`plot.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md),
[`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md),
[`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md),
[`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md),
[`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)

## Examples

``` r
if (interactive()) {
  ## select the segments to remove
  segs <- eeg_segment(data_faces_10_trials, .description %in% c("s70", "s71"),
    .lim = c(-.2, .5)
  )
  to_remove <- eeg_browse(segs)
  clean <- eeg_filter(segs, !.id %in% to_remove)

  ## select the components to remove
  ica <- eeg_ica(data_faces_10_trials, -EOGH, -EOGV, -M1, -M2,
    .method = fast_ICA
  )
  to_remove <- eeg_browse(ica)
  clean <- eeg_ica_keep(ica, -tidyselect::all_of(to_remove[[1]]))
}
```
