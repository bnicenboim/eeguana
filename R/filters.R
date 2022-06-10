#' Apply a zero-phase low-pass, high-pass, band-pass, or band-stop FIR or IIR filter.
#'
#' Apply a low-pass, high-pass, band-pass, or band-stop filter to every segment of an `eeg_lst`. These filters are adapted from the FIR and IIR filters in [MNE package](https://mne-tools.github.io) (v 0.0.17.1)  of [python](https://www.python.org/). For background information about the FIR vs IIR filters, see [here](https://martinos.org/mne/dev/auto_tutorials/plot_background_filtering.html#sphx-glr-auto-tutorials-plot-background-filtering-py). **IIR filters are still on an experimental phase**.
#'
#' * `eeg_filt_low_pass()` Low-pass or high-cut filter.
#' * `eeg_filt_high_pass()` High-pass or low-cut filter.
#' * `eeg_filt_band_pass()` Band-pass filter.
#' * `eeg_filt_band_stop()` Band-stop filter.
#'
#' After setting a filter method. The filters can be configured passing a list to the argument `.config`.
#'
#' Using `method ="fir"`, we define a zero-phase  filter of the FIR (finite impulse response) class (MNE v 0.0.17.1 default); these are the options:
#'
#' * `l_trans_bandwidth = "auto"` by default. This is  `min(max(l_freq * 0.25, 2), l_freq)`, where `l_freq` is the `freq` of the high pass filter, or `freq[1]` of a band pass/stop filter.
#' * `h_trans_bandwidth = "auto"` by default. This is `min(max(0.25 * h_freq, 2.), sampling_rate / 2. - h_freq)` where `h_freq` is the `freq` of the low pass filter, or `freq[2]` of a band pass/stop filter.
#'
#' Using `method = "iir"`, we define a zero-phase (two-pass forward and reverse) non-causal IIR filter.  Filter type is Butterworth by default, and either order (default) or maximum loss and attenuation (gpass and gstop) should be specified:
#'
#' * `type = "butter"` for Butterworth by default, other options are `"cheby1"`, or `"cheby2"` for Chebyshev type I or type II, or `"ellip"` for Elliptic.
#' * `order` = 6 by default for low and high pass, and 4 by default for band pass and stop filters (this follows the defaults of Fieldtrip matlab package). Notice that the effective order after forward-backward pass is multiplied by two.
#' * gpass : The maximum loss in the passband (dB).
#' * gstop : The minimum attenuation in the stopband (dB).

#'
#' @param .data A channel or an eeg_lst.
#' @param ... Channels to apply the filters to. All the channels by default.
#' @param .freq A single cut frequency for `eeg_filt_low_pass` and `eeg_filt_high_pass`, two edges for `eeg_filt_band_pass` and `eeg_filt_band_stop`.
#' @param .config Other parameters passed in a list to configure the filters. See details for options.
#' @param na.rm =TRUE will set to NA the entire segment that contains an NA, otherwise the filter will stop with an error.
#' @return A channel or an eeg_lst.
#' @family preprocessing functions
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data("data_faces_ERPs")
#' data_ERPs_filtered <- data_faces_ERPs %>%
#'   eeg_filt_low_pass(.freq = 1)
#' # Compare the ERPs
#' data_faces_ERPs %>%
#'   eeg_select(O1, O2, P7, P8) %>%
#'   plot() +
#'   facet_wrap(~.key)
#' data_ERPs_filtered %>%
#'   eeg_select(O1, O2, P7, P8) %>%
#'   plot() +
#'   facet_wrap(~.key)
#' @name filt
NULL
# > NULL

#' @rdname filt
#' @export
eeg_filt_low_pass <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  UseMethod("eeg_filt_low_pass")
}

#' @rdname filt
#' @export
eeg_filt_high_pass <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  UseMethod("eeg_filt_high_pass")
}
#' @rdname filt
#' @export
eeg_filt_band_pass <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  UseMethod("eeg_filt_band_pass")
}
#' @rdname filt
#' @export
eeg_filt_band_stop <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  UseMethod("eeg_filt_band_stop")
}

#' @export
eeg_filt_low_pass.eeg_lst <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  h <- create_filter(
    l_freq = NULL,
    h_freq = .freq,
    sampling_rate = sampling_rate(.data),
    config = .config
  )

  .data$.signal <- filt_eeg_lst(.data$.signal, ..., h = h, na.rm = na.rm)
  # if(.by_reference & options()$eeguana.verbose) changed_objects(.data)
  #  if(.by_reference) invisible(.data) else
  .data
}
#' @export
eeg_filt_high_pass.eeg_lst <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  h <- create_filter(
    l_freq = .freq,
    h_freq = NULL,
    sampling_rate = sampling_rate(.data),
    config = .config
  )
  .data$.signal <- filt_eeg_lst(.data$.signal, ..., h = h, na.rm = na.rm)
  # if(.by_reference & options()$eeguana.verbose) changed_objects(.data)
  #  if(.by_reference) invisible(.data) else
  .data
}
#' @export
eeg_filt_band_stop.eeg_lst <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  if (length(.freq) != 2) stop(".freq should contain two frequencies.")
  if (.freq[1] <= .freq[2]) {
    stop("The first argument of .freq should be larger than the second one.")
  }

  h <- create_filter(
    l_freq = .freq[1],
    h_freq = .freq[2],
    sampling_rate = sampling_rate(.data),
    config = .config
  )

  .data$.signal <- filt_eeg_lst(.signal = .data$.signal, ..., h = h, na.rm = na.rm)
  # if(.by_reference & options()$eeguana.verbose) changed_objects(.data)
  #  if(.by_reference) invisible(.data) else
  .data
}
#' @export
eeg_filt_band_pass.eeg_lst <- function(.data, ..., .freq = NULL, .config = list(), na.rm = FALSE) {
  if (length(.freq) != 2) stop(".freq should contain two frequencies.")
  if (.freq[1] >= .freq[2]) {
    stop("The first argument of .freq should be smaller than the second one.")
  }

  h <- create_filter(
    l_freq = .freq[1],
    h_freq = .freq[2],
    sampling_rate = sampling_rate(.data),
    config = .config
  )

  .data$.signal <- filt_eeg_lst(.data$.signal, ..., h = h, na.rm = na.rm)

  # if(.by_reference & options()$eeguana.verbose) changed_objects(.data)
  #  if(.by_reference) invisible(.data) else
  .data
}

#' @noRd
filt_eeg_lst <- function(.signal, ..., h, na.rm = FALSE, .by_ref = FALSE) {
  ch_sel <- sel_ch(.signal, ...)

  if (na.rm == FALSE) {
    NA_channels <- ch_sel[.signal[, purrr::map_lgl(.SD, anyNA), .SDcols = (ch_sel)]]
    if (length(NA_channels) > 0) {
      stop("Missing values in the following channels: ", paste(NA_channels, sep = ","), "; use na.rm =TRUE, to proceed setting to NA the entire segment that contains an NA", call. = FALSE)
    }
  }
  # fir filter
  if(is.null(names(h))) {
    .signal <- mutate.(.signal, across.(tidyselect::all_of(ch_sel),
                                        overlap_add_filter, h),
                       .by = ".id")
  } else {
    ## IIR filter
    attrs <- lapply(.signal, attributes)
    signal_non_sel <- select.(.signal, -tidyselect::all_of(ch_sel))
    signal_sel <- select.(.signal, tidyselect::all_of(ch_sel))
    if ("sos" %in% names(h)) {
      .signal <-  bind_cols.(signal_non_sel,
                             split(signal_sel,f =  .signal$.id)  %>%
                               map_dtr( function(ss){
                                 sig_sosfiltfilt(x = as.matrix(ss),
                                              sos = h$sos,
                                              padlen = min(h[["padlen"]],nrow(ss)-1)) %>%
                                   data.table::as.data.table()
                               }
                               )) 
    }
    if (all(c("b", "a") %in% names(h))) {
      # ba output
      #apply one by one
      # .signal <- mutate.(.signal, across(tidyselect::all_of(ch_sel),
      #                              sig_filtfilt,
      #                              b = h[["b"]], a = h[["a"]],
      #                           padlen = min(h[["padlen"]],n()-1)), .by = ".id")
      .signal <-  bind_cols.(signal_non_sel,
                       split(signal_sel,f =  .signal$.id)  %>%
                         map_dtr( function(ss){
                           sig_filtfilt(x = as.matrix(ss),
                                        b = h$b, a = h$a,
                                        padlen = min(h[["padlen"]],nrow(ss)-1)) %>%
                             data.table::as.data.table()
                         }
                         )) 
     
      
    } 
    #for iir filters
    .signal <- map2_dtc(.signal,attrs, function(c,a) {mostattributes(c)<-a
    c}) %>% as_signal_tbl()
  } 
  
  .signal 
  
}
