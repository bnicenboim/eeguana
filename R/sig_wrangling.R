
#' Mutate/transmute/select/rename channels or segments.
#' 
#' Manipulate the signal and the segments of an eegble.
#'
#' Wrappers for \link{dplyr}'s commands for eegble objects. These commands always return the entire eegble so that they can be
#' piped using \link{magrittr}'s pipe, %>%.
#' 
#' Manipulation type:
#' \itemize{
#' \item \code{mutate_*()} adds new variables and preserves existing..
#' \item \code{transmute_*()} drops existing variables.
#' \item \code{select_*()} keeps only the mentioned variables. 
#' \item \code{rename_*()}: keeps all variables.
#' }
#' Manipulation effect:
#' \itemize{
#' \item \code{*_chan()} affects the channels of the data and adapt the `chan_info` data frame.
#' \item \code{*_seg()} affect the segment information of the `seg_info` data frame..
#' }
#'
#' See also \link{dplyr-package}.
#' @param .data An eegbl.
#' @param ... Name-value pairs of expressions, for `_chan` variations they will include channel names, for `_seg` variations they will include segments descriptors.
#' @return An eegble object. 
#' 
#' @export
mutate_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::mutate(.data$data,!!!dots)
   update_chans(.data)
}            

#' @rdname mutate_chan
#' @export
transmute_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::transmute(.data$data, sample, !!!dots)
  update_chans(.data)
}            


#' @rdname mutate_chan
#' @export
select_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::select(.data$data, .id, sample, !!!dots)
  update_chans(.data)
}  

#' @rdname mutate_chan
#' @export
rename_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::rename(.data$data, !!!dots)
  update_chans(.data)
}  


#' @rdname mutate_chan
#' @export
mutate_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::mutate(.data$seg_info,!!!dots)
  .data
}   

#' @rdname mutate_chan
#' @export
transmute_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::mutate(.data$seg_info, .id, !!!dots)
  .data
}   

#' @rdname mutate_chan
#' @export
rename_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::rename(.data$seg_info,!!!dots)
  .data
}   

#' @rdname mutate_chan
#' @export
select_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::select(.data$seg_info, .id, !!!dots)
  .data
}   


