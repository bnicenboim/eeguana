# Baseline an eeg_lst

Subtract the average or baseline of the points in a defined interval
from all points in the segment.

## Usage

``` r
eeg_baseline(x, ..., .lim = -Inf, .unit = "s")
```

## Arguments

- x:

  An `eeg_lst` object or a channel.

- ...:

  Channels to include. All channels by default.

- .lim:

  The start of the baseline interval, or a vector with its start and
  end. The interval includes its start but not its end. A single value,
  such as the default `-Inf`, is the start of an interval that ends at
  time zero, whatever the `.unit`, so the default uses all the samples
  before time zero.

- .unit:

  "seconds" (or "s"), "milliseconds" (or "ms")

## Value

An eeg_lst.

## See also

Other preprocessing functions:
[`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md),
[`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md),
[`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md),
[`eeg_rereference()`](https://bruno.nicenboim.me/eeguana/reference/eeg_rereference.md),
[`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md),
[`filt`](https://bruno.nicenboim.me/eeguana/reference/filt.md)
