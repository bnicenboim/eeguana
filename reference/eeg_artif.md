# Detect artifacts and add them in the events table of an `eeg_lst`

These functions search for artifacts on the signal table based on a
threshold and a sliding window (when relevant), and annotate an event in
the events table that spans from `-lim` to `+lim`. The signal table
remains unchanged until
[`eeg_events_to_NA()`](https://bruno.nicenboim.me/eeguana/reference/eeg_events_to_NA.md).

## Usage

``` r
eeg_artif_minmax(
  .data,
  ...,
  .threshold = 100,
  .direction = "above",
  .window = 0.2,
  .lim = c(-.window, .window),
  .unit = "s",
  .freq = NULL,
  .config = list()
)

eeg_artif_step(
  .data,
  ...,
  .threshold = 50,
  .window = 0.2,
  .lim = c(-.window, .window),
  .unit = "s",
  .freq = NULL,
  .config = list()
)

eeg_artif_amplitude(
  .data,
  ...,
  .threshold = c(-200, 200),
  .lim = c(-0.2, 0.2),
  .unit = "s",
  .freq = NULL,
  .config = list()
)

eeg_artif_peak(
  .data,
  ...,
  .threshold = 30,
  .direction = "any",
  .window = 0.2,
  .lim = c(-.window, .window),
  .unit = "s",
  .freq = NULL,
  .config = list()
)
```

## Arguments

- .data:

  An `eeg_lst` object.

- ...:

  Channels to include. All the channels by default, but eye channels
  should be removed.

- .threshold:

  Voltage threshold that indicates an artifact

- .direction:

  For `eeg_artif_minmax()`, whether to look "above" (default) or "below"
  the threshold. For `eeg_artif_peak()`, whether the peaks are positive
  ("above"), negative ("below"), or either of them ("any", default).

- .window:

  Sliding window length for the artifact detection (same unit as `lim`).
  This is the full width of the step function: this means that we are
  looking for a period of one voltage for half of the window immediately
  followed by a period of a different voltage (indicated by the
  threshold) for half of the window.

- .lim:

  Vector with two values indicating the time before and after the
  artifact that will be included in events_tbl (by default the size the
  window before and afterwards).

- .unit:

  "seconds" (or "s"), "milliseconds" (or "ms")

- .freq:

  Vector with two values indicates whether to prefilter the signal prior
  to the artifact detection. (The filtering is not saved in the signal).
  For a low pass filter the first value should be `NA`, for a high-pass
  filter the second value should be `NA`.

- .config:

  List with the configuration of the filter.

## Value

An `eeg_lst`.

## Details

`eeg_artif_peak()` is a wrapper around
[`pracma::findpeaks()`](https://rdrr.io/pkg/pracma/man/findpeaks.html):
`.threshold` is the minimum absolute height that a peak needs to be
counted, and `.window` is the minimum distance between two peaks.
`.direction` sets whether to look for positive peaks ("above"), negative
peaks ("below"), or both ("any"). The signal should stay around zero: if
a drift or an offset keeps it beyond the threshold, use `.freq` or
`eeg_artif_minmax()`.

`eeg_artif_minmax()` is also refered as a peak-to-peak artifact
detector. It is less sensitive to drifts than `eeg_artif_peak()`.

`eeg_artif_minmax()` and `eeg_artif_step()` can be used to detect blinks
and horizontal eye movements in the electro-oculographic (V/HEOG)
channels or large voltage jumps in other channels. See chapter 6 of Luck
(2014).

For the EOG channels, a relatively low threshold (e.g., 30 µV) is
recommended. For non EOG channels, a relatively high threshold (e.g.,
100 µV) would be more appropriate.

## References

Luck, S. J. (2014). An introduction to the event-related potential
technique. MIT press.

## See also

Other events functions:
[`eeg_events_to_NA()`](https://bruno.nicenboim.me/eeguana/reference/eeg_events_to_NA.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Artifacts are annotated in the events table:
faces_seg_artif <- faces_seg %>%
  eeg_artif_minmax(-HEOG, -VEOG, .threshold = 100, .window = 150, unit = "ms") %>%
  eeg_artif_step(-HEOG, -VEOG, .threshold = 50, .window = 200, unit = "ms")

# Signals with artifacts are turned into NA values:
faces_clean <- faces_seg_artif %>%
  eeg_events_to_NA(.type == "artifact", .entire_seg = TRUE, .drop_events = TRUE)
} # }
```
