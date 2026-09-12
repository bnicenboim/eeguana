# eeguana 0.1.12.9003

## Bugs fixed

- `read_vhdr()` now checks the `.dat` against its header before reading it. A
  file that is too short, usually an incomplete copy or download, stops with
  both sizes named instead of failing inside data.table with
  `Supplied 38410 items to be assigned to 38409 items of column '.id'`. A file
  that is too long warns and is read in full.

# eeguana 0.1.12.9002

## New features

- `read_edf()` gains `.trigger_channel`, for EDF files that record triggers as
  an ordinary channel rather than as annotations. Name the channel, or use
  `"last"` for the last one. Its values become events and it is dropped from
  the signal table. When the argument is left out and the last channel looks
  like triggers, a message says so.

## Bugs fixed

- `read_ft()` and `read_set()` failed with `invalid substring arguments` on
  files with an empty text field. The cause is a regression in R.matlab
  3.8.0; `Remotes:` points at R.matlab's development version until the fix
  reaches CRAN.
- `read_fif()` and `as_eeg_lst()` on an MNE object no longer fail with
  `KeyError: 'bad'`.
- The join verbs now take `copy` in dplyr's position, so
  `left_join(x, y, by, copy)` no longer lands on `suffix`, and
  `semi_join()`/`anti_join()` no longer reject `copy`. `copy` and extra
  arguments are ignored with a warning.
- `keep = NULL`, dplyr's default for the joins, no longer errors.
  `keep = TRUE` warns and is ignored: it would split `.id`, which ties the
  signal, events and segments tables together.
- `ungroup(x = data)` works. The method took its first argument as `.data`
  while dplyr's generic calls it `x`.
- `annotate_events()` no longer warns about ggplot2's deprecated `%+%`.

## Internal

- Guards in `eeg_summarize()` and the `validate_*_tbl()` functions against a
  `data.table::setcolorder()` bug affecting tables with 64 or more columns.
  The validators now return their table instead of working by reference.
- S3 methods registered in `NAMESPACE`; no change to the public API.
- Documentation regenerated with roxygen2 8.0.0.
- Large test files moved out of the package sources into a per-user cache;
  see `dev/README.md`.
- Tests no longer write to the package sources or the user's home directory.

# eeguana 0.1.12.9001
- Updated documentation
- Signal table format consistent

# eeguana 0.1.11.9001

## Bug fixed
- `could not find function "across."` error fixed (thanks @prattems)

# eeguana 0.1.10.9001

## New Features

- Added `chs_mean` method for `psd_list` objects.
- Introduced experimental `read_fif()` function to read fif files created by MNE Python.
- New `install_py_eeguana()` function to install the necessary Python packages for reading fif files.

## Bugs and Improvements

- Improved behavior of `channel_dbl` and `sample_int` classes.
- Enhanced `annotate_events()` function to allow plotting even if there are no events.

# eeguana 0.1.9.9001

- fix installation of the dependency tidytable.
- added `annotate_electrodes()` to add electrode labels to a topographic plot. (Previously this was done with `geom_text()`).

# eeguana 0.1.8.9001

- fix of minor bug affecting eeg_artif_step(), when the number of samples in `window` was odd.

# eeguana 0.1.8.9000

- `as_eeg_lst()` makes `eeg_lst` objects created with 
previous versions of eeguana compatible with the current version.

# eeguana 0.1.7.9000
- New features:
- The argument `.ref` of `eeg_rereference()` now allows "tidyselect".
    - `write_vhdr()` writes BrainVision 2.0 files (experimental).
    - `eeg_psd()` computes the power spectral density (PSD) of a en EEG signal (using only the Welch method for now).
    - `eeg_band_power()` computes power bands.
    - `na_omit()` removes NA values from `eeg_lst` objects.
    - `as_tidytable()` added.
    - `sampling_rate()` shows the sampling rate of an object.
    - `eeg_unsegment()` was added.
- Modifying channels with `channels_tbl()` is much faster now.
- Signal filtering is slightly faster and uses the `gsignal` package.
- BUGS:
  - some brainvision files produced by eeglab couldn't be read now work
- DEPRECATIONS:
  - The argument `.all_chs` of `eeg_events_to_NA` was deprecated in favor of the more flexible `.n_chs`.


# eeguana 0.1.6.9000

- **Breaking change**: `_at`, `_if`, `_all` functions don't work anymore. Now one should use the new `across()` (or `across_ch()`) notation. If your code doesn't work anymore and you don't know how to fix it, please post it in https://github.com/bnicenboim/eeguana/discussions
- **Potential breaking change**: when segments with repeated columns are joined, they use data.table notation (`i...`) rather than dplyr notation. This might affect the `join` family and `eeg_segment`.
- dplyr-like functions get an `eeg_` prefix (e.g., `eeg_mutate`, `eeg_filter`), they still work without the prefix (for now). 
- NEW FEATURES:
    - filters have more options (including IIR, experimental).
    - read eeglab set files (still experimental)
- documentation:
    - more examples
- internal changes:
    - removed dependency with `stringr`
    - changed internal structure of `eeg_lst`, the segments table is a data.table rather than a tibble and they depend on `tidytable`
    
# eeguana 0.1.5.9000
  - Bugs: 
    - weird dplyr filter behavior fixed  [#136](https://github.com/bnicenboim/eeguana/issues/136)
  - Changes
    - `plot_components()` behaves more similarly to `plot_topo()`
    - **BREAKING CHANGE** `...` are always the second argument and most arguments require now a `.` at the beginning: This should help to differentiate them from channels. 
  - Improvements
    - New examples for ICA functions.
    - Much faster mutate functions
    - Better error messages for reading Brain Vision files @jaromilfrossard
    - `scale` works properly for channels now
  - Additions
    - [Gotchas & Pitfall vignette](https://bnicenboim.github.io/eeguana/articles/gotchas.html) was added
    
# eeguana 0.1.4.9000
  - Bugs: tons of compatibility issues with dplyr 1.0
  - Changes
    - Signal processing functions added `sig_fft()`, `sig_ifft()`.
    - Better print method for channels.
    - More consistent events_tbl with obligatory .type and .description
    
    
# eeguana 0.1.3.9000
  - More unit testing.
  - Bugs:
    - `read_edf()` wasn't reading events from the status channel
    - fixed some inconsistencies with `.reference` argument

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
      - `ch_rereference` updates the relevant attributes.
      - bugs in various reading functions were fixed (int encoding issues).
      - `mutate` recognize better channels.
      - `*_join` functions fixed.
   
# eeguana 0.0.2.9001

- Signals table is now powered by data.table.


# eeguana 0.0.2.9000

* Added a `NEWS.md` file to track changes to the package.
