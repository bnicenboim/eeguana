# Create an eeg_lst

Builds an eeg_lst object composed of two
[`data.table::data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
objects and one
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html).
All three are linked by a unique identifier `.id`. Amplitude values and
timestamps appear in the `signal` table. Triggers, blinks, artifact
rejection markings, and other events logged by the EEG recording
software appear in the `events` table. Segment information and recording
IDs appear in the `segments` tibble.

## Usage

``` r
eeg_lst(
  signal_tbl = NULL,
  events_tbl = NULL,
  segments_tbl = NULL,
  channels_tbl = NULL,
  .sampling_rate = NULL
)
```

## Arguments

- signal_tbl:

  See
  [`signal_tbl()`](https://bruno.nicenboim.me/eeguana/reference/signal_tbl.md).

- events_tbl:

  See
  [`events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md).

- segments_tbl:

  A data table of segment numbers and related information. See
  [`segments_tbl()`](https://bruno.nicenboim.me/eeguana/reference/segments_tbl.md).

- channels_tbl:

  Optionally a table with channels information. See
  [`channels_tbl()`](https://bruno.nicenboim.me/eeguana/reference/channels_tbl.md).

- .sampling_rate:

  Optional: If the signal_tbl doesn't have samples, they will be
  included with this sampling rate.

## Value

A valid eeg_lst.

## Details

The `signal` table is organized into columns representing timestamps
(`.sample`) and individual electrodes. Each `.sample` corresponds to 1
sample in the original recording, i.e. if the sampling rate of the EEG
recording is 500 Hz, then each `.sample` corresponds to 2 milliseconds.
These timestamps correspond to `.initial` in the `events` table, which
displays only the timestamps where logged events began.

The `events` table is organized into columns representing the `.type` of
event associated with the trigger listed under `.description`. The
timestamp marking the beginning and the end of the event is listed under
`.initial` and `.final` (in samples). The `.channel` column is a linking
variable only, so will generally only contain NAs, unless the event is
specific to a certain channel.

The `segments` tibble contains the subject ID under `recording`, which
is the file name unless otherwise specified. If the data has been
segmented in BrainVision, the segment number will be listed under
`segment`. The data can also be segmented according to trigger labels in
`eeguana`, see `segment`. `segment` will be place the segment number
under `segment`, the trigger name under `.type.x`, and the trigger label
under `.description.x`. Other information such as condition labels or
response times can be added by the user by merging into the `segments`
tibble using non-eeguana merge functions, e.g. the `dplyr` join series.

## See also

Other eeg_lst:
[`is_eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/is_eeg_lst.md)
