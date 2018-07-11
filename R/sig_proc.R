
#' Rereference channel.
#' 
#' This function is meant to be used together with \code{mutate} or \code{mutate_all}. See the example
#' 
#' @param x A channel.
#' @param ... Channels that will be averaged as the reference.
#' @return A rereferenced channel.
#' @export
#' 
#' @examples
#' \dontrun{
#' # Rereference all channels used the linked mastoids (average of the two mastoids)
#' 
#' faces_segs %>% act_on(signal) %>%
#'                  mutate_all(funs(rereference(., M1, M2)))
#' }#' 
rereference <- function(x, ..., na.rm = FALSE) {
   x - vec_mean(..., na.rm = na.rm)
}            
