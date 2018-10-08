#'  For the by sample (or by row) mean of the specified channels.
#'
#' Wrapper of rowMeans
#'
#' @export
channelMeans <- function(x, ..., na.rm = FALSE) {
  dots <- rlang::enquos(...)
  dplyr::select(x$signal, !!!dots) %>% rowMeans(na.rm = na.rm)
}