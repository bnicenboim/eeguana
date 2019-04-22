# eeguana 0.0.7.9000
  - Changes
      - FastICA was implemented in `eeg_ica()`.
      - Artifact detection based on voltage steps implemented in `eeg_grad_artifact()`.
      - Filters were greatly improved and checked, only FIR filters are supported for now.
      - Better documentation (mostly [Kate Stone](https://github.com/auskate)).
      - `eeg_intervals_to_NA()` was renamed to `eeg_events_to_NA()`.
      - `events()` was renamed to `events_tbl()`.
      - `ch_filt_*` functions were renamed to `eeg_filt_*` and they get a `...` argument to select the relevant channels.
      - Changes in the creation of `eeg_lst` objects.
# eeguana 0.0.6.9000
  - Changes
      - There is a logo!
      - Doi was added.
      - `segment()` was renamed to `eeg_segment()`
      - `downsample()` was renamed to `eeg_downsample()`
      - `interpolate_tbl()` was renamed to `eeg_interpolate_tbl()`
      - `plot_in_layout()` to place facets in the electrode layout was added.
      - `plot_topo()` has improved colors, and does "less", `geom_contour` and `geom_text` need to be added.
      - `annotate_head()` was added to the plot functions.
      - `change_coord()` was added as an auxiliary function to change the coordinate system for topographic or layout plots.
  - Bugs
      - Various minor bugs fixed.
  - Internal changes
      - More tests were added.
    
# eeguana 0.0.5.9000
 - Changes
      - `read_edf()` was added.
 - Bugs
      - Bug in `channel_names()` was fixed.
      - Bug  #34 fixed.
 - Internal changes:
      - Better test structure.
      - Better validation of objects.  

# eeguana 0.0.4.9000
 - Changes
      - `chs_fun()` to apply arbitrary functions to the channels.
      - Fieldtrip tutorial added as a vignette.
 - Bugs
      - Channels appear in the right order in plots.
      - Other minor bugs.

# eeguana 0.0.3.9000
 - Changes
      - ggplot2 theme improved.
      - changed the default downsampling in plots.
      - better handling of weird channel names.
      - `segment()` accepts initial and final segments.
      - `summarize` adds a summarize_n column.
      - `events` function to visualize and edit events was added.
      - various filters were added.
 - Bugs
      - `ch_rereference` updates the relevanta attributes.
      - bugs in various reading functions were fixed (int encoding issues).
      - `mutate` recognize better channels.
      - `*_join` functions fixed.
   
# eeguana 0.0.2.9001

- Signals table is now powered by data.table.


# eeguana 0.0.2.9000

* Added a `NEWS.md` file to track changes to the package.
