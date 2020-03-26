# eeguana 0.1.3.9000
  - Changes:
    - 
  - Bugs:
    - `read_edf()` wasn't reading events from the status channel
    - other minor bugs
# eeguana 0.1.2.9000
  - Changes:
    - `drop_incomplete_segments()` added.
    - More unit tests.
    - Some minor bugs fixed.

# eeguana 0.1.1.9000
  - Changes:
       - The introductory vignette (https://bnicenboim.github.io/eeguana/articles/intro.html) was slightly modified.
       - `eeg_segment()` accepts unpaired events when `end` argument is used, and should be able to deal with duplicated triggers.
       - `eeg_artif_peak()` detect peaks in the EEG signal.
       - `eeg_ica_cor_tbl()` and `eeg_ica_var_tbl` show the correlation of components (ICA) with EOG channels and their variance explained.
       - `eeg_ica_summary_tbl()` summarizes `eeg_ica_cor_tbl()` and `eeg_ica_var_tbl()`.
  - Bugs:
      - `events_tbl()` keep attributes.
      
# eeguana 0.1.0.9000
  - Changes
      - New vignette that introduces the package [here](https://bnicenboim.github.io/eeguana/articles/intro.html).
      - FastICA was implemented in `eeg_ica()`.
      - Artifact detection functions in `eeg_artif_*()`.
      - Filters were greatly improved and checked, only FIR filters are supported for now, based on [MNE](https://www.martinos.org/mne).
      - Better documentation (mostly [Kate Stone](https://github.com/auskate)).
      - Easy access to the information of the different tables with `signal_tbl()`, `events_tbl()`, and `segments_tbl()`.
      - Changes in the creation of `eeg_lst` objects.
      - Faster `as_tibble()`.
      - `as.data.table()` was added.
      - Renamed functions:
          + `eeg_intervals_to_NA()` was renamed to `eeg_events_to_NA()`.
          + `events()` was renamed to `events_tbl()`.
          + `plot_gg()` should be changed to 'ggplot(aes(x=.time, y=.value))'.
          + `summarize_all_ch(...)` should be changed to `summarize_at(channel_names(.),...)`.
          + `summarize_at_ch(...)` should be changed to `summarize_at(...)`.
          + `ch_filt_*()` functions were renamed to `eeg_filt_*()` and they get a `...` argument to select the relevant channels.
          + `ch_baseline()` was renamed to `eeg_baseline()`.
   - Bugs
      - Events in Brain Vision version 1.0 file are now correctly read. 
      - Various minor bugs fixed.
     
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
