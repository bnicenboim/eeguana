# Defunct functions in eeguana

These functions are defunct.

## Usage

``` r
downsample(x, q = 2, max_sample = NULL, ...)

segment(x, ...)

interpolate_tbl(.data, ...)

event_to_ch_NA(x, ...)

eeg_intervals_to_NA(x, ...)

ch_filt_low_pass(x, ...)

ch_filt_high_pass(x, ...)

ch_filt_band_pass(x, ...)

ch_filt_stop_pass(x, ...)

events(x, ...)

events(x) <- value

summarize_all_ch(x, ...)

summarize_at_ch(x, ...)

plot_gg(x, ...)

ch_baseline(x, ...)
```

## Arguments

- x:

  Variable storing the x coordinate, generally `.x` (default).

- q:

  integer factor(s) to downsample by.

- max_sample:

  Optionally, the (approximated) maximum sample number can be defined
  here, which is at least half of the total number of samples

- ...:

  Not in use.

- .data:

  An eeg_lst object.

- value:

  Values used for the interpolation, generally `.value` (default).

## Details

- 'downsample()' should be changed to 'eeg_downsample()'.

- 'segment()' should be changed to 'eeg_segment()'.

- 'interpolate_tbl()' should be changed to 'eeg_interpolate_tbl()'.

- 'event_to_ch_NA()' should be changed to 'eeg_intervals_to_NA()'.

- 'ch_filt_low_pass()' should be changed to 'eeg_filt_low_pass()'.

- 'ch_filt_high_pass()' should be changed to 'eeg_filt_high_pass()'.

- 'ch_filt_band_pass()' should be changed to 'eeg_filt_band_pass()'.

- 'ch_filt_stop_pass()' should be changed to 'eeg_filt_band_stop()'.

- 'ch_baseline()' should be changed to 'eeg_baseline()'.

- 'events()' should be changed to 'events_tbl()'.

- 'plot_gg()' should be changed to 'ggplot(aes(x=.time, y=.value))'.

- 'summarize_all_ch(...)' should be changed to
  `summarize_at(channel_names(.),...)`.

- 'summarize_at_ch(...)' should be changed to `summarize_at(...)`.
