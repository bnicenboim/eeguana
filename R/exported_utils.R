
#' @export
time_point <- function(x, ...) {
  UseMethod("time_point")
}

#' @export
time_point.sample_id <- function(x ) {
  # add unit

  time <- x / attributes(x)$sampling_rate 

  attributes(time) <- NULL
  time
}

