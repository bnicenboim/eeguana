# Changelog

## eeguana 0.1.12.9004

### New features

- [`eeg_artif_peak()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)
  gets a `.direction` argument (“above”, “below”, or “any”), and it now
  also finds negative peaks by default.
- [`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md):
  `.lim` also takes the end of the baseline, `c(start, end)`.
- [`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md)
  opens a Shiny app to look through the data and select parts of it, for
  example the ones to remove. For an `eeg_lst`, it shows the signal of
  the channels and the topography. For an `eeg_ica_lst`, it shows the
  activations of the components next to the EOG channels, or any other
  channels, and their topographies. In order for it to work, shiny and
  bslib need to be manually installed.
- [`print()`](https://rdrr.io/r/base/print.html) shows at most
  `.max_channels` channels, by default the option
  `eeguana.print_max_channels` (`Inf`).

### Bugs fixed

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) no longer
  draws the plot twice when layers are added to it.
- [`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md)
  draws the events without a channel, such as triggers, in every
  channel, instead of in an extra panel called NA.

## eeguana 0.1.12.9003

### Dependencies

- dplyr and tidyr move from `Imports` to `Suggests`, so installing
  eeguana no longer installs them. The dplyr verbs still work on eeguana
  objects whenever dplyr is installed, because their methods are
  registered as soon as dplyr loads, whichever of the two packages loads
  first.

- Examples and vignettes no longer load dplyr. The vignettes, and the
  one example that uses a verb from outside eeguana, load tidytable
  instead, a faster alternative.

- purrr is no longer a dependency. \## Bugs fixed

- [`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md),
  and so
  [`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md),
  took the variance of several channels as the mean of all the entries
  of their covariance matrix, that is, the variance of their average. A
  component then counted only as much as its weights added up, so
  components whose topography is positive on one side and negative on
  the other, such as horizontal eye movements, were underestimated, and
  with an average reference the values were meaningless. The variance of
  several channels is now the mean of their variances, as in EEGLAB’s
  `eeg_pvaf()`. The values change: in the intro vignette, the horizontal
  eye movement component goes from 1% to 5%.

- [`tidytable::as_tidytable()`](https://markfairbanks.github.io/tidytable/reference/as_tidytable.html)
  on a `psd_lst` failed with “rownames incorrect length”, because the
  method for `psd_lst` was never registered, unlike the one for
  `eeg_lst`.

- `plot_ica()` (experimental and not exported) failed with “incompatible
  dimensions” on data with more than one recording, because it filtered
  with `.recording == .recording`, which kept every recording.

- [`drop_incomplete_segments()`](https://bruno.nicenboim.me/eeguana/reference/drop_incomplete_segments.md)
  errored with `could not find function "na.omit"` on every call. It now
  drops the segments holding missing values, as documented.

- [`transmute()`](https://markfairbanks.github.io/tidytable/reference/transmute.html)
  on an events table errored with
  `object 'transmute.tidytable' not found`, because the tidytable method
  it called was removed upstream.

- [`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md)
  accepts any data frame. It previously had a method only for tibbles,
  so a plain `data.frame` had none.

- [`read_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/read_vhdr.md)
  now checks the `.dat` against its header before reading it. A file
  that is too short, usually an incomplete copy or download, stops with
  both sizes named instead of failing inside data.table with
  `Supplied 38410 items to be assigned to 38409 items of column '.id'`.
  A file that is too long warns and is read in full.

### Breaking

- [`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md)
  and
  [`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md)
  report different variances, see above.

- [`eeg_interpolate_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_interpolate_tbl.md)
  returns a tidytable rather than a tibble. It is still a data frame, so
  it keeps working wherever one is expected, but a test such as
  `inherits(x, "tbl_df")` on the result is now `FALSE`.

### Internal

- [`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md)
  computes the variance explained from the activations of the components
  instead of rebuilding the signal once per component. On the intro
  vignette it takes 1 s instead of 35 s, so it no longer downsamples by
  default: `.max_sample` is `NULL`, and a number still downsamples
  first. It does not go through the covariance matrix of the channels,
  which loses digits when the channels are rank-deficient, as
  average-referenced ones are.

- eeguana no longer sets data.table keys. Printed tables lose their
  `Key:` line.

- dplyr is no longer called inside the package. The internal calls go
  through tidytable, and the calls that act on an `eeg_lst` go through
  eeguana’s own `eeg_*` generics.

- `dev/check_without_dplyr.R` runs the package on a library where dplyr,
  tidyr, and tibble are not installed.

- No unexported function of another package is called with `:::` any
  more. One such call had already broken
  [`transmute()`](https://markfairbanks.github.io/tidytable/reference/transmute.html)
  on events tables when tidytable removed the function.

## eeguana 0.1.12.9002

### New features

- [`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md)
  gains `.trigger_channel`, for EDF files that record triggers as an
  ordinary channel rather than as annotations. Name the channel, or use
  `"last"` for the last one. Its values become events and it is dropped
  from the signal table. When the argument is left out and the last
  channel looks like triggers, a message says so.

### Bugs fixed

- [`read_ft()`](https://bruno.nicenboim.me/eeguana/reference/read_ft.md)
  and
  [`read_set()`](https://bruno.nicenboim.me/eeguana/reference/read_set.md)
  failed with `invalid substring arguments` on files with an empty text
  field. The cause is a regression in R.matlab 3.8.0; `Remotes:` points
  at R.matlab’s development version until the fix reaches CRAN.
- [`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md)
  and
  [`as_eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_eeg_lst.md)
  on an MNE object no longer fail with `KeyError: 'bad'`.
- The join verbs now take `copy` in dplyr’s position, so
  `left_join(x, y, by, copy)` no longer lands on `suffix`, and
  [`semi_join()`](https://markfairbanks.github.io/tidytable/reference/left_join.html)/[`anti_join()`](https://markfairbanks.github.io/tidytable/reference/left_join.html)
  no longer reject `copy`. `copy` and extra arguments are ignored with a
  warning.
- `keep = NULL`, dplyr’s default for the joins, no longer errors.
  `keep = TRUE` warns and is ignored: it would split `.id`, which ties
  the signal, events and segments tables together.
- `ungroup(x = data)` works. The method took its first argument as
  `.data` while dplyr’s generic calls it `x`.
- [`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md)
  no longer warns about ggplot2’s deprecated `%+%`.

### Internal

- Guards in
  [`eeg_summarize()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  and the `validate_*_tbl()` functions against a
  [`data.table::setcolorder()`](https://rdrr.io/pkg/data.table/man/setcolorder.html)
  bug affecting tables with 64 or more columns. The validators now
  return their table instead of working by reference.
- S3 methods registered in `NAMESPACE`; no change to the public API.
- Documentation regenerated with roxygen2 8.0.0.
- Large test files moved out of the package sources into a per-user
  cache; see `dev/README.md`.
- Tests no longer write to the package sources or the user’s home
  directory.

## eeguana 0.1.12.9001

- Updated documentation
- Signal table format consistent

## eeguana 0.1.11.9001

### Bug fixed

- `could not find function "across."` error fixed (thanks
  [@prattems](https://github.com/prattems))

## eeguana 0.1.10.9001

### New Features

- Added `chs_mean` method for `psd_list` objects.
- Introduced experimental
  [`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md)
  function to read fif files created by MNE Python.
- New
  [`install_py_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/install_py_eeguana.md)
  function to install the necessary Python packages for reading fif
  files.

### Bugs and Improvements

- Improved behavior of `channel_dbl` and `sample_int` classes.
- Enhanced
  [`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md)
  function to allow plotting even if there are no events.

## eeguana 0.1.9.9001

- fix installation of the dependency tidytable.
- added
  [`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md)
  to add electrode labels to a topographic plot. (Previously this was
  done with
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)).

## eeguana 0.1.8.9001

- fix of minor bug affecting eeg_artif_step(), when the number of
  samples in `window` was odd.

## eeguana 0.1.8.9000

- [`as_eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_eeg_lst.md)
  makes `eeg_lst` objects created with previous versions of eeguana
  compatible with the current version.

## eeguana 0.1.7.9000

- New features:
- The argument `.ref` of
  [`eeg_rereference()`](https://bruno.nicenboim.me/eeguana/reference/eeg_rereference.md)
  now allows “tidyselect”.
  - [`write_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/write_vhdr.md)
    writes BrainVision 2.0 files (experimental).
  - [`eeg_psd()`](https://bruno.nicenboim.me/eeguana/reference/eeg_psd.md)
    computes the power spectral density (PSD) of a en EEG signal (using
    only the Welch method for now).
  - `eeg_band_power()` computes power bands.
  - [`na_omit()`](https://bruno.nicenboim.me/eeguana/reference/na_omit.md)
    removes NA values from `eeg_lst` objects.
  - [`as_tidytable()`](https://markfairbanks.github.io/tidytable/reference/as_tidytable.html)
    added.
  - [`sampling_rate()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
    shows the sampling rate of an object.
  - [`eeg_unsegment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md)
    was added.
- Modifying channels with
  [`channels_tbl()`](https://bruno.nicenboim.me/eeguana/reference/channels_tbl.md)
  is much faster now.
- Signal filtering is slightly faster and uses the `gsignal` package.
- BUGS:
  - some brainvision files produced by eeglab couldn’t be read now work
- DEPRECATIONS:
  - The argument `.all_chs` of `eeg_events_to_NA` was deprecated in
    favor of the more flexible `.n_chs`.

## eeguana 0.1.6.9000

- **Breaking change**: `_at`, `_if`, `_all` functions don’t work
  anymore. Now one should use the new
  [`across()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  (or
  [`across_ch()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md))
  notation. If your code doesn’t work anymore and you don’t know how to
  fix it, please post it in
  <https://github.com/bnicenboim/eeguana/discussions>
- **Potential breaking change**: when segments with repeated columns are
  joined, they use data.table notation (`i...`) rather than dplyr
  notation. This might affect the `join` family and `eeg_segment`.
- dplyr-like functions get an `eeg_` prefix (e.g., `eeg_mutate`,
  `eeg_filter`), they still work without the prefix (for now).
- NEW FEATURES:
  - filters have more options (including IIR, experimental).
  - read eeglab set files (still experimental)
- documentation:
  - more examples
- internal changes:
  - removed dependency with `stringr`
  - changed internal structure of `eeg_lst`, the segments table is a
    data.table rather than a tibble and they depend on `tidytable`

## eeguana 0.1.5.9000

- Bugs:
  - weird dplyr filter behavior fixed
    [\#136](https://github.com/bnicenboim/eeguana/issues/136)
- Changes
  - [`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)
    behaves more similarly to
    [`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md)
  - **BREAKING CHANGE** `...` are always the second argument and most
    arguments require now a `.` at the beginning: This should help to
    differentiate them from channels.
- Improvements
  - New examples for ICA functions.
  - Much faster mutate functions
  - Better error messages for reading Brain Vision files
    [@jaromilfrossard](https://github.com/jaromilfrossard)
  - `scale` works properly for channels now
- Additions
  - [Gotchas & Pitfall
    vignette](https://bnicenboim.github.io/eeguana/articles/gotchas.html)
    was added

## eeguana 0.1.4.9000

- Bugs: tons of compatibility issues with dplyr 1.0
- Changes
  - Signal processing functions added
    [`sig_fft()`](https://bruno.nicenboim.me/eeguana/reference/sig_fft.md),
    [`sig_ifft()`](https://bruno.nicenboim.me/eeguana/reference/sig_ifft.md).
  - Better print method for channels.
  - More consistent events_tbl with obligatory .type and .description

## eeguana 0.1.3.9000

- More unit testing.
- Bugs:
  - [`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md)
    wasn’t reading events from the status channel
  - fixed some inconsistencies with `.reference` argument

## eeguana 0.1.2.9000

- Changes:
  - [`drop_incomplete_segments()`](https://bruno.nicenboim.me/eeguana/reference/drop_incomplete_segments.md)
    added.
  - More unit tests.
  - Some minor bugs fixed.

## eeguana 0.1.1.9000

- Changes:
  - The introductory vignette
    (<https://bnicenboim.github.io/eeguana/articles/intro.html>) was
    slightly modified.
  - [`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md)
    accepts unpaired events when `end` argument is used, and should be
    able to deal with duplicated triggers.
  - [`eeg_artif_peak()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)
    detect peaks in the EEG signal.
  - [`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md)
    and `eeg_ica_var_tbl` show the correlation of components (ICA) with
    EOG channels and their variance explained.
  - [`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md)
    summarizes
    [`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md)
    and
    [`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md).
- Bugs:
  - [`events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md)
    keep attributes.

## eeguana 0.1.0.9000

- Changes
  - New vignette that introduces the package
    [here](https://bnicenboim.github.io/eeguana/articles/intro.html).
  - FastICA was implemented in
    [`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md).
  - Artifact detection functions in `eeg_artif_*()`.
  - Filters were greatly improved and checked, only FIR filters are
    supported for now, based on [MNE](https://www.martinos.org/mne).
  - Better documentation (mostly [Kate
    Stone](https://github.com/auskate)).
  - Easy access to the information of the different tables with
    [`signal_tbl()`](https://bruno.nicenboim.me/eeguana/reference/signal_tbl.md),
    [`events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md),
    and
    [`segments_tbl()`](https://bruno.nicenboim.me/eeguana/reference/segments_tbl.md).
  - Changes in the creation of `eeg_lst` objects.
  - Faster `as_tibble()`.
  - `as.data.table()` was added.
  - Renamed functions:
    - [`eeg_intervals_to_NA()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
      was renamed to
      [`eeg_events_to_NA()`](https://bruno.nicenboim.me/eeguana/reference/eeg_events_to_NA.md).
    - [`events()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
      was renamed to
      [`events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md).
    - [`plot_gg()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
      should be changed to ‘ggplot(aes(x=.time, y=.value))’.
    - `summarize_all_ch(...)` should be changed to
      `summarize_at(channel_names(.),...)`.
    - `summarize_at_ch(...)` should be changed to `summarize_at(...)`.
    - `ch_filt_*()` functions were renamed to `eeg_filt_*()` and they
      get a `...` argument to select the relevant channels.
    - [`ch_baseline()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
      was renamed to
      [`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md).
- Bugs
  - Events in Brain Vision version 1.0 file are now correctly read.
  - Various minor bugs fixed.

## eeguana 0.0.6.9000

- Changes
  - There is a logo!
  - Doi was added.
  - [`segment()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
    was renamed to
    [`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md)
  - [`downsample()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
    was renamed to
    [`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md)
  - [`interpolate_tbl()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
    was renamed to
    [`eeg_interpolate_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_interpolate_tbl.md)
  - [`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md)
    to place facets in the electrode layout was added.
  - [`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md)
    has improved colors, and does “less”, `geom_contour` and `geom_text`
    need to be added.
  - [`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md)
    was added to the plot functions.
  - [`change_coord()`](https://bruno.nicenboim.me/eeguana/reference/change_coord.md)
    was added as an auxiliary function to change the coordinate system
    for topographic or layout plots.
- Bugs
  - Various minor bugs fixed.
- Internal changes
  - More tests were added.

## eeguana 0.0.5.9000

- Changes
  - [`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md)
    was added.
- Bugs
  - Bug in
    [`channel_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
    was fixed.
  - Bug [\#34](https://github.com/bnicenboim/eeguana/issues/34) fixed.
- Internal changes:
  - Better test structure.
  - Better validation of objects.

## eeguana 0.0.4.9000

- Changes
  - [`chs_fun()`](https://bruno.nicenboim.me/eeguana/reference/chs_fun.md)
    to apply arbitrary functions to the channels.
  - Fieldtrip tutorial added as a vignette.
- Bugs
  - Channels appear in the right order in plots.
  - Other minor bugs.

## eeguana 0.0.3.9000

- Changes
  - ggplot2 theme improved.
  - changed the default downsampling in plots.
  - better handling of weird channel names.
  - [`segment()`](https://bruno.nicenboim.me/eeguana/reference/defunct.md)
    accepts initial and final segments.
  - `summarize` adds a summarize_n column.
  - `events` function to visualize and edit events was added.
  - various filters were added.
- Bugs
  - `ch_rereference` updates the relevant attributes.
  - bugs in various reading functions were fixed (int encoding issues).
  - `mutate` recognize better channels.
  - `*_join` functions fixed.

## eeguana 0.0.2.9001

- Signals table is now powered by data.table.

## eeguana 0.0.2.9000

- Added a `NEWS.md` file to track changes to the package.
