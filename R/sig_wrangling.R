
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
#' \item \code{filter_*()}: find segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' \item \code{left_join_*()}: Left-joins an external dataframe to one of the dataframes of the eegble.
#' }
#' Manipulation effect:
#' \itemize{
#' \item \code{*_chan()} affects the channels of the data and adapt the `chan_info` data frame.
#' \item \code{*_seg()} affect the segment information of the `seg_info` data frame.
#' \item \code{*_ev()} affect the event information of the `events` data frame.
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
   update_chans(.data) %>% validate_eegbl 
}            

#' @rdname mutate_chan
#' @export
transmute_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::transmute(.data$data, sample, !!!dots)
  update_chans(.data) %>% validate_eegbl 
}            


#' @rdname mutate_chan
#' @export
select_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::group_by(.data$data, .id, sample) %>%
                 dplyr::select(!!!dots) %>% ungroup()
  update_chans(.data) %>% validate_eegbl 
}  

#' @rdname mutate_chan
#' @export
rename_chan <- function(.data, ...){
  dots <-  validate_dots(...)
  .data$data <- dplyr::rename(.data$data, !!!dots)
  update_chans(.data) %>% validate_eegbl 
}  


#' @rdname mutate_chan
#' @export
mutate_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::mutate(.data$seg_info,!!!dots)
  validate_eegbl(.data)
}   

#' @rdname mutate_chan
#' @export
transmute_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::mutate(.data$seg_info, .id, !!!dots)
  validate_eegbl(.data)
}   

#' @rdname mutate_chan
#' @export
rename_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::rename(.data$seg_info,!!!dots)
  validate_eegbl(.data)
}   

#' @rdname mutate_chan
#' @export
select_seg <- function(.data, ...){
   dots <-  validate_dots(...)
  .data$seg_info <- dplyr::group_by(.data$seg_info, .id, sample) %>% 
                    dplyr::select(!!!dots) %>% ungroup()
  validate_eegbl(.data)
}   

#' @rdname mutate_chan
#' @export
filter_seg <- function(.data, ...){
   dots <-  rlang::enquos(...)
  .data$seg_info <- dplyr::filter(.data$seg_info, !!!dots)
  .data$data <- dplyr::semi_join(.data$data, .data$seg_info, by =".id")
  .data$events <- dplyr::semi_join(.data$events, .data$seg_info, by =".id")

  validate_eegbl(.data)
}   

#' @rdname mutate_chan
#' @export
left_join_seg <- function(x, y, by = NULL, suffix= c(".x", ".y"), ...){
  if(!is_eegble(x)){stop("x must be an eegble.")}
  dplyr::left_join(x$seg_info, y, by = NULL, suffix= c(".x", ".y"), ...)
}

#' @rdname mutate_chan
#' @export
left_join_ev <- function(x, y, by = NULL, suffix= c(".x", ".y"), ...){
  if(!is_eegble(x)){stop("x must be an eegble.")}
  dplyr::left_join(x$events, y, by = NULL, suffix= c(".x", ".y"), ...)
}



##' @rdname mutate_chan
# not yet exported
filter_s <- function(.data, ...){
   dots <-  rlang::enquos(...)
   #should edit the dots to transform time to samples
   #then it filters by $data
  .data$seg_info <- dplyr::filter(.data$seg_info, !!!dots)
  .data$data <- dplyr::semi_join(.data$data, .data$seg_info, by =".id")
  .data$events <- dplyr::semi_join(.data$events, .data$seg_info, by =".id")

  validate_eegbl(.data)
}   


#it could look for the names and check if they appear in data or seg_info