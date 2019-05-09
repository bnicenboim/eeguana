#' Convert a sample point into a time point.
#'
#'
#' @param x A `sample_id` object.
#' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
#'
#' @return A vector of times.
#'
#' @importFrom magrittr %>%
#'
#' @export
as_time <- function(x, unit = "second") {
  UseMethod("as_time")
}

#' @export
as_time.sample_int <- function(x, unit = "second") {
  time <- (x - 1) / scaling(sampling_rate = attributes(x)$sampling_rate, unit)
  attributes(time) <- NULL
  time
}
#' @export
as_time.default <- function(x, unit = "second") {
  stop("`as_time()` can only be used with samples. Tip: You should probably use it with `.sample`.")
}

#' Convert a time point  into a sample.
#'
#'
#'
#' @return A sample_int object.
#' @param ... Not in use.
#' @export
as_sample_int <- function(x,...) {
    UseMethod("as_sample_int")
}
#' @rdname as_sample_int
#' @param x A vector of numeric values.
#' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
#' @param sampling_rate Sampling rate in Hz
#' @export
as_sample_int.numeric <- function(x,  sampling_rate = NULL, unit = "s", ...) {
    if(is.null(sampling_rate)) stop("'sampling_rate' needs to be specified", call. = FALSE)
    samples <- (x * scaling(sampling_rate, unit = unit) + 1) %>% as.integer()

    sample_int(samples, sampling_rate)
}
#' @export
as_sample_int.sample_int <- function(x) {
    x
}
