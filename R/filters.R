#' Apply a butterworth IIR filter.
#'
#' Apply a Butterworth IIR filter using \code{signal::filt_filt}. Based on
#' Matt Craddock's code of \code{eegUtils} \email{matt@mattcraddock.com}.
#'
#' \itemize{
#' \item \code{filt_low_pass_ch()} Low-pass or high-cut filter.
#' \item \code{filt_high_pass_ch()} High-pass or low-cut filter.
#' \item \code{filt_band_pass_ch()} Band-pass filter.
#' \item \code{filt_stop_pass_ch()} Stop-pass filter.
#' }
#'
#' @param x A channel or an eeg_lst.
#' @param band_edge A single cut frequency for \code{filt_low_pass_ch} and \code{filt_high_pass_ch}, two edges for  \code{filt_band_pass_ch} and \code{filt_stop_pass_ch}.
#' @param order Filter order (default is 6).
#' @param direction "twopass" (default) filters the signal twice (once forwards, then again backwards).
#' @param ... The sample rate can be included as \code{sampling_rate}, when the function is called outside mutate/summarize. 
#'
#' @return A channel or an eeg_lst.
#'
#'
#' @examples
#' \dontrun{
#'
#' faces_segs %>% filt_low_pass_ch(band_edge = 100, order = 4)
#' }
#' @name filt
NULL
#> NULL

#' @rdname filt
#' @export
ch_filt_low_pass <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  UseMethod("ch_filt_low_pass")
}

#' @rdname filt
#' @export
ch_filt_high_pass <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  UseMethod("ch_filt_high_pass")
}
#' @rdname filt
#' @export
ch_filt_band_pass <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  UseMethod("ch_filt_band_pass")
}
#' @rdname filt
#' @export
ch_filt_stop_pass <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  UseMethod("ch_filt_stop_pass")
}


#' @export
ch_filt_low_pass.channel_dbl <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  if(length(band_edge)>1) stop("band_edge should contain only one frequency.")
  type <- "low"
  ch_filt(channel = x, band_edge = band_edge, order = order, type = type, direction = "twopass",...)
}

#' @rdname filt
#' @export
ch_filt_high_pass.channel_dbl <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  if(length(band_edge)>1) stop("band_edge should contain only one frequency.")
  type <- "high"
  centered_channel <- x - mean(x)
  ch_filt(channel = centered_channel, band_edge = band_edge, order = order, type = type, direction = "twopass",...)
}

#' @rdname filt
#' @export
ch_filt_band_pass.channel_dbl <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  if(length(band_edge) != 2) stop("band_edge should contain two frequencies.")
  if(band_edge[1] >= band_edge[2]) {
    stop("The first argument of band_edge should be larger than the second one.")
  }
  type <- "pass"
  centered_channel <- x - mean(x)
  ch_filt(channel = centered_channel, band_edge = band_edge, order = order, type = type, direction = "twopass",...)
}

#' @rdname filt
#' @export
ch_filt_stop_pass.channel_dbl <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  if(length(band_edge) != 2) stop("band_edge should contain two frequency.")
  if(band_edge[1] <= band_edge[2]) {
    stop("The second argument of band_edge should be larger than the first one.")  }
  type <- "stop"
  centered_channel <- x - mean(x)
  ch_filt(channel = centered_channel, band_edge = band_edge, order = order, type = type, direction = "twopass",...)
}



#' @rdname filt
#' @export
ch_filt_low_pass.eeg_lst <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  x$signal <- copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_low_pass.channel_dbl, band_edge = band_edge, 
                          order = order, direction = direction, 
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                          
}

#' @rdname filt
#' @export
ch_filt_high_pass.eeg_lst <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  x$signal <- copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_high_pass.channel_dbl, band_edge = band_edge, 
                          order = order, direction = direction, 
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                       
}
#' @rdname filt
#' @export
ch_filt_high_pass.eeg_lst <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
  x$signal <- copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_high_pass.channel_dbl, band_edge = band_edge, 
                          order = order, direction = direction, 
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                         
}
#' @rdname filt
#' @export
ch_filt_stop_pass.eeg_lst <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
   x$signal <- copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_stop_pass.channel_dbl, band_edge = band_edge, 
                          order = order, direction = direction, 
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                       
}
#' @rdname filt
#' @export
ch_filt_band_pass.eeg_lst <- function(x, band_edge = NULL, order = 6, direction = "twopass", ...) {
   x$signal <- copy(x$signal)
  x$signal[, (channel_names(x)) := lapply(.SD, ch_filt_band_pass.channel_dbl, band_edge = band_edge, 
                          order = order, direction = direction, 
                          sampling_rate = sampling_rate(x)), 
                          .SDcols = channel_names(x), by = ".id"]
  x                        
}


ch_filt <- function(channel, band_edge = NULL, order = 6, type, direction = "twopass", ...){
  args <- list(...)
  if(!"sampling_rate" %in% names(args)){ #if it's empty it will be taken from the attributes of sample
      # #Black magic to extract sample columns and its attribute sampling_rate that has the sampling_rate of the eeg_lst object:  
      # signal_env <- rlang::env_get(env = parent.frame(2), '.top_env', inherit = TRUE)
      # .sample_id <- rlang::env_get(signal_env, ".sample_id")

      # sampling_rate <- attributes(.sample_id)$sampling_rate
      # print(sampling_rate)
      sampling_rate <- attributes(extended_signal$.sample_id)$sampling_rate
    } else {
      sampling_rate <- args$sampling_rate
    }
  W <- band_edge / (sampling_rate / 2)

  # filtfilt filters twice, so effectively doubles filt_order - we half it here
  # so that it corresponds to the expectation of the user
  order <- round(order / 2)
  filt <- signal::butter(n = order, W = W, type = type)
  signal::filtfilt(filt, channel)
}

# filtfilt in matlab
# https://github.com/fieldtrip/fieldtrip/blob/f67c00431828af4777ac27b464d6adfd9d4ac884/external/signal/filtfilt.m
# butter:
# https://github.com/fieldtrip/fieldtrip/blob/f67c00431828af4777ac27b464d6adfd9d4ac884/external/signal/butter.m
