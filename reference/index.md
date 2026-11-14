# Package index

## About

- [`eeguana`](https://bruno.nicenboim.me/eeguana/reference/eeguana-package.md)
  [`eeguana-package`](https://bruno.nicenboim.me/eeguana/reference/eeguana-package.md)
  : eeguana: Flexible Manipulation of EEG Data

## Reading and writing functions

Functions for reading external files and return an `eeg_lst`.

- [`read_edf()`](https://bruno.nicenboim.me/eeguana/reference/read_edf.md)
  : Read an edf/edf+/bdf file into R
- [`read_fif()`](https://bruno.nicenboim.me/eeguana/reference/read_fif.md)
  : Read a FIF file into R
- [`read_ft()`](https://bruno.nicenboim.me/eeguana/reference/read_ft.md)
  : Read a Fieldtrip file into R
- [`read_set()`](https://bruno.nicenboim.me/eeguana/reference/read_set.md)
  : Read EEGlab set files (Matlab files) into R
- [`read_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/read_vhdr.md)
  : Read a BrainVision file into R
- [`write_vhdr()`](https://bruno.nicenboim.me/eeguana/reference/write_vhdr.md)
  : Write an eeg_lst object to BrainVision file(s) (experimental)

## Tidyverse verbs (and extensions)

Functions that overload some tidyverse functions and always return an
`eeg_lst`.

- [`eeg_mutate()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_transmute()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_filter()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_summarize()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_summarise()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_group_by()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_ungroup()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_select()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_rename()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_rename_with()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_groups()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_group_vars()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_left_join()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_semi_join()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_anti_join()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`eeg_pull()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`across()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`across_ch()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`c_across_ch()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  [`c_across()`](https://bruno.nicenboim.me/eeguana/reference/dplyr_verbs.md)
  : Dplyr-like functions for manipulating eeg_lst objects.
- [`drop_incomplete_segments()`](https://bruno.nicenboim.me/eeguana/reference/drop_incomplete_segments.md)
  : Drop segments with NAs from the eeg_lst
- [`eeg_bind()`](https://bruno.nicenboim.me/eeguana/reference/eeg_bind.md)
  [`bind()`](https://bruno.nicenboim.me/eeguana/reference/eeg_bind.md) :
  Bind eeg_lst objects.
- [`eeg_slice_signal()`](https://bruno.nicenboim.me/eeguana/reference/eeg_slice_signal.md)
  [`slice_signal()`](https://bruno.nicenboim.me/eeguana/reference/eeg_slice_signal.md)
  : Choose samples by the position

## Specialized summarizing functions

Functions that return useful summaries.

- [`count_complete_cases_tbl()`](https://bruno.nicenboim.me/eeguana/reference/count_complete_cases_tbl.md)
  : Count number of complete segments of an eeg_lst object.
- [`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md)
  : Show correlations between ICA sources and eye (EOG) electrodes.
- [`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md)
  : Show a table with a summary of the results of the ICA.
- [`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md)
  : Show the variance explained for each ICA sources.
- [`channel_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`channel_ica_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`nchannels()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`component_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`ncomponents()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`sampling_rate()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`nsamples()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  : Display information of the eeg_lst object.
- [`summary(`*`<eeg_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/summary.eeg_lst.md)
  : Summary of eeg_lst information.

## EEG-specialized functions

Functions for applying specialized actions on the signal table, channel,
or the entire `eeg_lst`.

- [`eeg_artif_minmax()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)
  [`eeg_artif_step()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)
  [`eeg_artif_amplitude()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)
  [`eeg_artif_peak()`](https://bruno.nicenboim.me/eeguana/reference/eeg_artif.md)
  :

  Detect artifacts and add them in the events table of an `eeg_lst`

- [`eeg_events_to_NA()`](https://bruno.nicenboim.me/eeguana/reference/eeg_events_to_NA.md)
  : Remove (transform to NA) problematic events from the signal table of
  an eeg_lst.

- [`eeg_ica()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica.md)
  : EEG signal decomposition using Independent Component Analysis (ICA)

- [`eeg_ica_cor_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_cor_tbl.md)
  : Show correlations between ICA sources and eye (EOG) electrodes.

- [`eeg_ica_keep()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_keep.md)
  : Select independent components (or sources) to keep.

- [`eeg_ica_show()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_show.md)
  : Add independent components (or sources) to the signal table for
  visualization.

- [`eeg_ica_summary_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_summary_tbl.md)
  : Show a table with a summary of the results of the ICA.

- [`eeg_ica_var_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_ica_var_tbl.md)
  : Show the variance explained for each ICA sources.

- [`eeg_baseline()`](https://bruno.nicenboim.me/eeguana/reference/eeg_baseline.md)
  : Baseline an eeg_lst

- [`eeg_downsample()`](https://bruno.nicenboim.me/eeguana/reference/eeg_downsample.md)
  : Downsample EEG data

- [`eeg_rereference()`](https://bruno.nicenboim.me/eeguana/reference/eeg_rereference.md)
  : Re-reference a channel or group of channels.

- [`eeg_segment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md)
  [`eeg_unsegment()`](https://bruno.nicenboim.me/eeguana/reference/eeg_segment.md)
  : Segment (and unsegment) an eeg_lst.

- [`eeg_filt_low_pass()`](https://bruno.nicenboim.me/eeguana/reference/filt.md)
  [`eeg_filt_high_pass()`](https://bruno.nicenboim.me/eeguana/reference/filt.md)
  [`eeg_filt_band_pass()`](https://bruno.nicenboim.me/eeguana/reference/filt.md)
  [`eeg_filt_band_stop()`](https://bruno.nicenboim.me/eeguana/reference/filt.md)
  : Apply a zero-phase low-pass, high-pass, band-pass, or band-stop FIR
  or IIR filter.

- [`chs_fun()`](https://bruno.nicenboim.me/eeguana/reference/chs_fun.md)
  : Get the by-sample (or by-row) function of the specified channels.

- [`chs_mean()`](https://bruno.nicenboim.me/eeguana/reference/chs_mean.md)
  : Get the by-sample (or by-row) mean of the specified channels.

- [`eeg_power_band()`](https://bruno.nicenboim.me/eeguana/reference/eeg_power_band.md)
  : Compute power bands.

- [`eeg_psd()`](https://bruno.nicenboim.me/eeguana/reference/eeg_psd.md)
  : Compute the power spectral density (PSD) of an EEG signal.

## Plot functions

Wrappers to `ggplot` functions that first transform the `eeg_lst` to a
wide `tibble`, together with some transformation (downsampling,
interpolation, etc).

- [`ggplot(`*`<eeg_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/ggplot.eeg_lst.md)
  : Create an ERP plot
- [`plot(`*`<eeg_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md)
  [`plot(`*`<psd_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/plot.eeg_lst.md)
  : Create a basic signal plot
- [`plot_components()`](https://bruno.nicenboim.me/eeguana/reference/plot_components.md)
  : Generates topographic plots of the components after running ICA on
  an eeg_lst
- [`plot_in_layout()`](https://bruno.nicenboim.me/eeguana/reference/plot_in_layout.md)
  : Arrange ERP plots according to scalp layout
- [`plot_topo()`](https://bruno.nicenboim.me/eeguana/reference/plot_topo.md)
  : Create a topographic plot
- [`annotate_electrodes()`](https://bruno.nicenboim.me/eeguana/reference/annotate_electrodes.md)
  : Adds the electrode labels to a head shape
- [`annotate_events()`](https://bruno.nicenboim.me/eeguana/reference/annotate_events.md)
  : Adds a layer with the events on top of a plot of an eeg_lst.
- [`annotate_head()`](https://bruno.nicenboim.me/eeguana/reference/annotate_head.md)
  : Add a head shape to a ggplot
- [`theme_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)
  [`theme_eeguana2()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)
  [`default_theme()`](https://bruno.nicenboim.me/eeguana/reference/theme_eeguana.md)
  : Eeguana ggplot themes
- [`eeg_browse()`](https://bruno.nicenboim.me/eeguana/reference/eeg_browse.md)
  : Browse the signal or the ICA components interactively

## ‘base’-inspired functions

Functions for getting specific information about the objects that
compose an `eeg_lst`.

- [`as_channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/as_channel_dbl.md)
  : Coerce a vector of real (double) numbers into a channel object

- [`as_eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_eeg_lst.md)
  : Transforms an object to a eeg_lst

- [`as_sample_int()`](https://bruno.nicenboim.me/eeguana/reference/as_sample_int.md)
  : Convert a time point into a sample.

- [`as_tibble.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.eeg_lst.md)
  [`as_data_frame.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.eeg_lst.md)
  [`as_long_tbl(`*`<eeg_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.eeg_lst.md)
  :

  Convert an eeg_lst to a long table in
  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) format.

- [`as_tibble.psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tibble.psd_lst.md)
  :

  Convert an psd_lst to a long table in
  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) format.

- [`as_tidytable.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tidytable.eeg_lst.md)
  :

  Convert an eeg_lst to a long table in
  [`tidytable`](https://markfairbanks.github.io/tidytable/reference/tidytable.html)
  format.

- [`as_tidytable.psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/as_tidytable.psd_lst.md)
  :

  Convert an psd_lst to a long table in
  [`tidytable`](https://markfairbanks.github.io/tidytable/reference/tidytable.html)
  format.

- [`as_time()`](https://bruno.nicenboim.me/eeguana/reference/as_time.md)
  : Convert a sample point into a time point.

- [`as.data.frame(`*`<eeg_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/as.data.frame.eeg_lst.md)
  : Convert an eeg_lst to a (base) data frame.

- [`as.data.frame(`*`<psd_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/as.data.frame.psd_lst.md)
  : Convert a psd_lst to a (base) data frame.

- [`as.data.table.eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/as.data.table.eeg_lst.md)
  :

  Convert an eeg_lst to a long table in
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  format.

- [`as.data.table.psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/as.data.table.psd_lst.md)
  :

  Convert a psd_lst list to a long table in
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  format.

- [`is_channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/is_channel_dbl.md)
  : Test if the object is a channel or EOG channel

- [`is_component_dbl()`](https://bruno.nicenboim.me/eeguana/reference/is_component_dbl.md)
  : Test if the object is a component This function returns TRUE for
  components.

- [`is_eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/is_eeg_lst.md)
  : Test if the object is an eeg_lst.

- [`is_events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/is_events_tbl.md)
  : Test if the object is an events_tbl This function returns TRUE for
  events_tbl.

- [`is_psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/is_psd_lst.md)
  : Test if the object is a psd_lst.

- [`is_psd_tbl()`](https://bruno.nicenboim.me/eeguana/reference/is_psd_tbl.md)
  : Test if the object is a psd_tbl

- [`is_sample_int()`](https://bruno.nicenboim.me/eeguana/reference/is_sample_int.md)
  : Test if the object is a sample This function returns TRUE for
  samples.

- [`is_signal_tbl()`](https://bruno.nicenboim.me/eeguana/reference/is_signal_tbl.md)
  : Test if the object is a signal_tbl

- [`na_omit()`](https://bruno.nicenboim.me/eeguana/reference/na_omit.md)
  : Handle Missing Values in eeg_lst objects

- [`channel_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`channel_ica_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`nchannels()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`component_names()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`ncomponents()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`sampling_rate()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  [`nsamples()`](https://bruno.nicenboim.me/eeguana/reference/summaries.md)
  : Display information of the eeg_lst object.

- [`ica_matrix_lst()`](https://bruno.nicenboim.me/eeguana/reference/ica_matrix_lst.md)
  : Function to get the list of mixing and unmixing matrices of an
  eeg_ica_lst object.

- [`print(`*`<eeg_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/print.eeg_lst.md)
  [`print(`*`<psd_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/print.eeg_lst.md)
  [`print(`*`<eeg_ica_lst>`*`)`](https://bruno.nicenboim.me/eeguana/reference/print.eeg_lst.md)
  : Print an eeg_lst

## Editing functions

Functions for showing (and sometimes editing) `eeg_lst` objects parts.

- [`signal_tbl()`](https://bruno.nicenboim.me/eeguana/reference/signal_tbl.md)
  : Function to get the signal table of an eeg_lst object.
- [`events_tbl()`](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md)
  [`` `events_tbl<-`() ``](https://bruno.nicenboim.me/eeguana/reference/events_tbl.md)
  : Functions to get or set the events table of an eeg_lst object.
- [`segments_tbl()`](https://bruno.nicenboim.me/eeguana/reference/segments_tbl.md)
  : Function to get the segments table of an eeg_lst object.
- [`channels_tbl()`](https://bruno.nicenboim.me/eeguana/reference/channels_tbl.md)
  [`` `channels_tbl<-`() ``](https://bruno.nicenboim.me/eeguana/reference/channels_tbl.md)
  : Functions to get or set the channel information of an eeg_lst
  object.

## Constructor functions

Functions for creating objects.

- [`eeg_lst()`](https://bruno.nicenboim.me/eeguana/reference/eeg_lst.md)
  : Create an eeg_lst

- [`psd_lst()`](https://bruno.nicenboim.me/eeguana/reference/psd_lst.md)
  :

  Creates a `psd_lst`.

- [`sample_int()`](https://bruno.nicenboim.me/eeguana/reference/sample_int.md)
  : Builds a series of sample numbers.

- [`channel_dbl()`](https://bruno.nicenboim.me/eeguana/reference/channel_dbl.md)
  : Builds a channel.

- [`component_dbl()`](https://bruno.nicenboim.me/eeguana/reference/component_dbl.md)
  : Builds a component.

## Others

Some other useful functions.

- [`eeg_interpolate_tbl()`](https://bruno.nicenboim.me/eeguana/reference/eeg_interpolate_tbl.md)
  : Create a table with interpolated signals of an eeg_lst object.

- [`change_coord()`](https://bruno.nicenboim.me/eeguana/reference/change_coord.md)
  : Change coordinate system from 3D to 2D

- [`rpink()`](https://bruno.nicenboim.me/eeguana/reference/rpink.md) :
  Generates pink noise

- [`adapt_fast_ICA()`](https://bruno.nicenboim.me/eeguana/reference/fICA.md)
  [`fast_ICA2()`](https://bruno.nicenboim.me/eeguana/reference/fICA.md)
  : Wrapper for FastICA methods for Independent Component Analysis from
  fICA package

- [`fast_ICA()`](https://bruno.nicenboim.me/eeguana/reference/fast_ICA.md)
  : Wrapper for FastICA method for Independent Component Analysis from
  fastICA package

- [`ica_matrix_lst()`](https://bruno.nicenboim.me/eeguana/reference/ica_matrix_lst.md)
  : Function to get the list of mixing and unmixing matrices of an
  eeg_ica_lst object.

- [`between`](https://bruno.nicenboim.me/eeguana/reference/between.md) :
  Convenience function for range subsets

- [`install_py_eeguana()`](https://bruno.nicenboim.me/eeguana/reference/install_py_eeguana.md)
  :

  Install the Python packages needed for `eeguana`

## Example data sets

Example data sets in eeg_lst format.

- [`data_faces_10_trials`](https://bruno.nicenboim.me/eeguana/reference/data_faces_10_trials.md)
  : EEGs elicited when one subject saw faces and non-faces objects.
- [`data_faces_ERPs`](https://bruno.nicenboim.me/eeguana/reference/data_faces_ERPs.md)
  : ERPs elicited when one subject saw faces and non-faces objects.

## Layouts

Layout of caps

- [`layout_32_1020`](https://bruno.nicenboim.me/eeguana/reference/layout_32_1020.md)
  : Layout for a 32 electrodes cap at the standard 10-20 system

## Signal processing

Miscellanous signal processing functions (mostly adapted from python and
matlab)

- [`sig_fft()`](https://bruno.nicenboim.me/eeguana/reference/sig_fft.md)
  : Compute the one-dimensional discrete Fourier Transform.
- [`sig_ifft()`](https://bruno.nicenboim.me/eeguana/reference/sig_ifft.md)
  : Compute the one-dimensional inverse discrete Fourier Transform.
