#' Downsample EEG data
#'
#' Downsample a signal_tbl by a factor `q`, using an FIR or IIR filter.
#' This is a wrapper for \link{decimate} from the
#' \link{signal_tbl} package, see its documentation for details. Notice that
#' the code of the \link{signal_tbl} package might be outdated.
#'
#' A factor q larger than 13 can result in NAs. To avoid this,
#' the downsampling can be done in steps. For example, instead of setting
#' `q = 20`, it is possible to set `q = c(2,10)`.
#'
#' @param x An eeg_lst object.
#' @param q integer factor(s) to downsample by.
#' @param max_sample Optionally, the (approximated) maximum sample number can be defined here, which is at least half of the total number of samples.
#' @param ... Not in use.
#' 
#' @family eeg
#' 
#' @export
downsample <- function(x, q = 2, max_sample = NULL, ...) {
  UseMethod("downsample")
}

#' @export
downsample.eeg_lst <- function(x, q = 2L, max_sample = NULL,
                               n = if (ftype == "iir") 8 else 30,
                               ftype = "iir", ...) {

  # if(stringr::str_to_lower(q) == "min") {
  #   q <- mindiv(sampling_rate(x), start = 2)
  #  message(paste0("Using q = ", q))
  # }

  if (any(q < 2)) {
    stop("# The factor q must be 2 or more.")
  }

  if (any(round(q) != q)) {
    q <- round(q)
    warning(paste0("# The factor q needs to be round number, using q = ", q))
  }

  if (!is.null(max_sample)) {
    len_samples <- max(nsamples(x))
    if (max_sample > len_samples / 2) {
      stop("The maximum value for max_sample allowed is half the number of samples.")
    }
    approx_q <- len_samples / max_sample
    q <- factors(round(approx_q))
  }

  q <- as.integer(q)
  factor <- prod(q)
  new_sampling_rate <- sampling_rate(x) / factor
  message(paste0(
    "# Downsampling from ", sampling_rate(x), "Hz to ",
    new_sampling_rate, "Hz."
  ))

  list_of_attr <- purrr::map(x$signal, ~attributes(.x))
  channels_info <- channels_tbl(x)

decimate_fun <- function(channel) {
  attrs <- attributes(channel)
  decimated_chan <- purrr::reduce(c(list(channel), as.list(q)), ~
            signal::decimate(x = .x, q = .y, n = n, ftype = ftype))
  mostattributes(decimated_chan) <- attrs 
  decimated_chan
}

  x$signal <- x$signal[,lapply(.SD, decimate_fun), .SDcols = c(channel_names(x)),by = c(".id")][
            ,.sample_id := sample_int(seq_len(.N), new_sampling_rate),by = c(".id")][]

  data.table::setkey(x$signal,.id,.sample_id)
  data.table::setcolorder(x$signal,c(".id",".sample_id"))

  # even table needs to be adapted, starts from 1,
  # and the size is divided by two with a min of 1
  x$events <- data.table::copy(x$events)[, .sample_0 := as.integer(round(.sample_0 / factor)) + 1L][, .size := round(.size / factor) %>%
                                       as.integer() %>%
                                       purrr::map_int(~max(.x, 1L)) ][]
    # dplyr::mutate(
    #   .sample_0 = as.integer(round(.sample_0 / factor)) + 1L,
    #   .size = round(.size / factor) %>%
    #     as.integer() %>%
    #     purrr::map_int(~max(.x, 1L))
    # )

  # just in case I update the .id from segments table
  x$segments <- dplyr::mutate(x$segments, .id = seq_len(dplyr::n()))

  message(say_size(x))

  x %>% #update_channels_tbl(channels_info) %>% 
      validate_eeg_lst()
}
