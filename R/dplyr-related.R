

#' @export
channelMeans <- function(x, ..., na.rm = FALSE) {
  dots <- rlang::enquos(...)
  dplyr::select(x$signal, !!!dots) %>% rowMeans(na.rm=na.rm)
}