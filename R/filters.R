#' Apply a zero-phase low-pass, high-pass, band-pass, or band-stop filter.
#'
#' Apply a zero-phase low-pass, high-pass, band-pass, or band-stop filter of the  FIR (finite impulse response) class to every segment of an `eeg_lst`. These filters are adapted from the default filters in [MNE package](https://mne-tools.github.io) (v 0.0.17.1)  of [python](https://www.python.org/). For background information about the FIR vs IIR filters, see (here)[https://martinos.org/mne/dev/auto_tutorials/plot_background_filtering.html#sphx-glr-auto-tutorials-plot-background-filtering-py],.
#'
#'   * `eeg_filt_low_pass()` Low-pass or high-cut filter.
#'   * `eeg_filt_high_pass()` High-pass or low-cut filter.
#'   * `eeg_filt_band_pass()` Band-pass filter.
#'   * `eeg_filt_stop_pass()` Stop-pass filter.
#' 
#'
#' @param .data A channel or an eeg_lst.
#' @param freq A single cut frequency for `eeg_filt_low_pass` and `eeg_filt_high_pass`, two edges for  `eeg_filt_band_pass` and `eeg_filt_stop_pass`.
#' @param ... Channels to apply the filters to. All the channels by default.
#' @param config Other parameters passed in a list to the ICA method. (Not implemented)
#'
#' @return A channel or an eeg_lst.
#'
#'
#' @examples
#' data("data_faces_ERPs")
#' data_ERPs_filtered <- data_faces_ERPs %>% eeg_filt_low_pass(freq = 5)
#' # Compare the ERPs
#' data_faces_ERPs %>% select(O1, O2, P7, P8) %>% plot() + facet_wrap(~.source)
#' data_ERPs_filtered %>% select(O1, O2, P7, P8) %>% plot() + facet_wrap(~.source)
#' @name filt
NULL
#> NULL 

#' @rdname filt
#' @export
eeg_filt_low_pass <- function(.data, ..., freq = NULL, config = list()) {
  UseMethod("eeg_filt_low_pass")
}

#' @rdname filt
#' @export
eeg_filt_high_pass <- function(.data, ..., freq = NULL, config = list()) {
  UseMethod("eeg_filt_high_pass")
}
#' @rdname filt
#' @export
eeg_filt_band_pass <- function(.data, ..., freq = NULL, config = list()) {
  UseMethod("eeg_filt_band_pass")
}
#' @rdname filt
#' @export
eeg_filt_band_stop <- function(.data, ..., freq = NULL, config = list()) {
  UseMethod("eeg_filt_band_stop")
}




#' @rdname filt
#' @export
eeg_filt_low_pass.eeg_lst <- function(.data, ..., freq = NULL, config = list()) {
    h <- create_filter(l_freq = NULL,
                       h_freq = freq,
                       sampling_rate = sampling_rate(.data),config = config)
    .data$signal <- filt_eeg_lst(.data$signal,...,h=h)
    .data
}

#' @rdname filt
#' @export
eeg_filt_high_pass.eeg_lst <- function(.data, ..., freq = NULL, config = list()) {
    h <- create_filter(l_freq = freq,
                       h_freq = NULL,
                       sampling_rate = sampling_rate(.data),config = config)
    .data$signal <- filt_eeg_lst(.data$signal,...,h=h)
    .data
}
#' @rdname filt
#' @export
eeg_filt_band_stop.eeg_lst <- function(.data, ..., freq = NULL, config = list()) {
    if(length(freq) != 2) stop("freq should contain two frequencies.")
    if(freq[1] <= freq[2]) {
        stop("The first argument of freq should be larger than the second one.")  }

    h <- create_filter(l_freq = freq[1],
                       h_freq= freq[2],
                       sampling_rate = sampling_rate(.data),config = config)
    .data$signal <- filt_eeg_lst(.data$signal,...,h=h)
    .data
}
#' @rdname filt
#' @export
eeg_filt_band_pass.eeg_lst <- function(.data,..., freq = NULL, config = list()) {
    if(length(freq) != 2) stop("freq should contain two frequencies.")
    if(freq[1] >= freq[2]) {
        stop("The first argument of freq should be smaller than the second one.")
    }

    h <- create_filter(l_freq = freq[1],
                       h_freq= freq[2],
                       sampling_rate = sampling_rate(.data),config = config)
    .data$signal <- filt_eeg_lst(.data$signal,...,h=h)
    .data
}


filt_eeg_lst <- function(.signal,..., h){
    .signal <- data.table::copy(.signal)
    dots <- rlang::enquos(...)
    if(rlang::is_empty(dots)) {
        ch_sel <- channel_names(.signal)
    } else {
        ch_sel <- tidyselect::vars_select(channel_names(.signal), !!!dots)
    }
    .signal[, (ch_sel) := lapply(.SD, overlap_add_filter,h), 
                    .SDcols = (ch_sel), by = ".id"]
    .signal
}


