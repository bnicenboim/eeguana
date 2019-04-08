#' Apply a zero-phase low-pass, high-pass, band-pass, or band-stop filter.
#'
#' Apply a zero-phase low-pass, high-pass, band-pass, or band-stop filter of the  FIR (finite impulse response) class. These filters are adapted from the default filters in [MNE package](https://mne-tools.github.io) (v 0.0.17.1)  of [python](https://www.python.org/). For background information about the FIR vs IIR filters, see (here)[https://martinos.org/mne/dev/auto_tutorials/plot_background_filtering.html#sphx-glr-auto-tutorials-plot-background-filtering-py],.
#'
#' \itemize{
#' \item \code{ch_filt_low_pass()} Low-pass or high-cut filter.
#' \item \code{ch_filt_high_pass()} High-pass or low-cut filter.
#' \item \code{ch_filt_band_pass()} Band-pass filter.
#' \item \code{ch_filt_stop_pass()} Stop-pass filter.
#' }
#'
#' @param x A channel or an eeg_lst.
#' @param freq A single cut frequency for \code{ch_filt_low_pass} and \code{filt_high_pass_ch}, two edges for  \code{filt_band_pass_ch} and \code{filt_stop_pass_ch}.
#' @param ... The sample rate can be included as `sampling_rate`, when the function is called outside mutate/summarize. 
#'
#' @return A channel or an eeg_lst.
#'
#'
#' @examples
#' \dontrun{
#'
#' faces_segs %>% filt_low_pass_ch(freq = 100, order = 4)
#' }
#' @name filt
NULL
#> NULL 

#' @rdname filt
#' @export
ch_filt_low_pass <- function(x, freq = NULL,  ...) {
  UseMethod("ch_filt_low_pass")
}

#' @rdname filt
#' @export
ch_filt_high_pass <- function(x, freq = NULL,  ...) {
  UseMethod("ch_filt_high_pass")
}
#' @rdname filt
#' @export
ch_filt_band_pass <- function(x, freq = NULL,  ...) {
  UseMethod("ch_filt_band_pass")
}
#' @rdname filt
#' @export
ch_filt_band_stop <- function(x, freq = NULL,  ...) {
  UseMethod("ch_filt_band_stop")
}


#' @export
ch_filt_low_pass.channel_dbl <- function(x, freq = NULL,  ...) {
    if(length(freq)>1) stop("freq should contain only one frequency.", call.=FALSE)
      type <- "low"
    filt_custom(channel = channel, h_freq = freq,...)
}

#' @rdname filt
#' @export
ch_filt_high_pass.channel_dbl <- function(x, freq = NULL,  ...) {
  if(length(freq)>1) stop("freq should contain only one frequency.")

  filt_custom(channel = channel, l_freq = freq,...)
}

#' @rdname filt
#' @export
ch_filt_band_pass.channel_dbl <- function(x, freq = NULL,  ...) {
  if(length(freq) != 2) stop("freq should contain two frequencies.")
  if(freq[1] >= freq[2]) {
    stop("The first argument of freq should be smaller than the second one.")
  }
  filt_custom(channel = channel, l_freq = freq[1], h_freq = freq[2],...)
}

#' @rdname filt
#' @export
ch_filt_band_stop.channel_dbl <- function(x, freq = NULL,  ...) {
  if(length(freq) != 2) stop("freq should contain two frequencies.")
  if(freq[1] <= freq[2]) {
    stop("The first argument of freq should be larger than the second one.")  }
  type <- "stop"
  filt_custom(channel = channel, l_freq = freq[1], h_freq = freq[2],...)
}



#' @rdname filt
#' @export
ch_filt_low_pass.eeg_lst <- function(x, freq = NULL,  ...) {
  x$signal <- data.table::copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_low_pass.channel_dbl,
                                          freq= freq,
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                          
}

#' @rdname filt
#' @export
ch_filt_high_pass.eeg_lst <- function(x, freq = NULL,  ...) {
  x$signal <- data.table::copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_high_pass.channel_dbl,
                                         freq= freq,
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                         
}
#' @rdname filt
#' @export
ch_filt_band_stop.eeg_lst <- function(x, freq = NULL,  ...) {
   x$signal <- data.table::copy(x$signal)
   x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_band_stop.channel_dbl,
                                           freq = freq,
                           sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                       
}
#' @rdname filt
#' @export
ch_filt_band_pass.eeg_lst <- function(x, freq, ...) {
   x$signal <- data.table::copy(x$signal)
   x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_band_pass.channel_dbl,
                                           l_freq = freq[1],
                                           h_freq= freq[2],
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                        
}


filt_custom <- function(x, freq = NULL, type, ... ){
  args <- list(...)
  if(!"sampling_rate" %in% names(args)){ #if it's empty it will be taken from the attributes of sample
      ## Black magic to extract sample columns and its attribute sampling_rate that has the sampling_rate of the eeg_lst object:  
      ## signal_env <- rlang::env_get(env = parent.frame(2), '.top_env', inherit = TRUE)
      ## .sample_id <- rlang::env_get(signal_env, ".sample_id")
      ## sampling_rate <- attributes(.sample_id)$sampling_rate
      # print(sampling_rate)
      sampling_rate <- attributes(extended_signal$.sample_id)$sampling_rate
    } else {
      sampling_rate <- args$sampling_rate
    }

  h <-  create_filter(sampling_rate = sampling_rate, ...)
  overlap_add_filter(x, h)
}


# filtfilt in matlab
# https://github.com/fieldtrip/fieldtrip/blob/f67c00431828af4777ac27b464d6adfd9d4ac884/external/signal/filtfilt.m
# butter:
# https://github.com/fieldtrip/fieldtrip/blob/f67c00431828af4777ac27b464d6adfd9d4ac884/external/signal/butter.m

# maybe from here:
## https://github.com/mne-tools/mne-python/blob/master/mne/filter.py#L708-L851
## https://martinos.org/mne/dev/auto_tutorials/plot_background_filtering.html#sphx-glr-auto-tutorials-plot-background-filtering-py
## https://martinos.org/mne/dev/auto_tutorials/plot_artifacts_correction_filtering.html#sphx-glr-auto-tutorials-plot-artifacts-correction-filtering-py
