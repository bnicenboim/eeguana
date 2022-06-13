#' wrapper for signal::decimate that allows a vector in q, for decimating several times serially
#' When using IIR downsampling, it is recommended to call decimate multiple times for downsampling factors higher than 13. reference: https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.decimate.html
#' @noRd
decimate_ch <- function(.channel, q, n = if (ftype == "iir") 8 else 30, ftype = "iir") {
  attrs <- attributes(.channel)
  .channel <- as.numeric(.channel)
  
  if (anyNA(.channel)) {
    warning("There are NAs, downsampling might be inaccurate.")
    r <- .channel[seq(1, length(.channel), by = prod(q))]
  } else if (length(q) > 1) {
    r <- Reduce(function(x, q) gsignal::decimate(x = x, q = q, n = n, ftype = ftype), x = q, init = .channel)
  } else {
    r <- gsignal::decimate(x = .channel, q = q, n = n, ftype = ftype)
  }
  mostattributes(r) <- attrs
  r
}

#' @noRd
decimate_chs <- function(.channels, q, n = if (ftype == "iir") 8 else 30, ftype = "iir") {
  attrs <- lapply(.channels, attributes)
  .channels <- as.matrix(.channels)
  
  if (anyNA(.channels)) {
    warning("There are NAs, downsampling might be inaccurate.")
    r <- .channels[seq(1, nrow(.channels), by = prod(q)),]
  } else if (length(q) > 1) {
    r <- Reduce(function(x, q) gsignal::decimate(x = x, q = q, n = n, ftype = ftype), x = q, init = .channels)
  } else {
    r <- gsignal::decimate(x = .channels, q = q, n = n, ftype = ftype)
  }
  r <- data.table::as.data.table(r)
  map2_dtc(r,attrs, function(c,a) {mostattributes(c)<-a
    c})
  
}

# d <- tidytable::tidytable(a=1:100, b=1:100)
# tidytable::map_dfc.(d, function(c) {c})
# purrr::map_dfc(d, function(c) {c})


