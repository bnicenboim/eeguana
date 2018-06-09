decimals <- function(x) match(TRUE, round(x, 1:20) == x)
say_size <- function(eegble) paste("# Object size",capture.output(pryr::object_size(eegble)))


new_eegbl <- function(x) {
	stopifnot(is.list(x))

  structure(x,
    class = c("eegbl"))
}

validate_eegbl <- function(x) {
  if(names(x)[1] != "data"){
    stop("data is missing or has the wrong type",
      call. = FALSE)
  }
  if(names(x)[2] != "chan_info"){
    stop("chan_info is missing",
      call. = FALSE)
  }
  if(!is.data.frame(x$chan_info)){
    stop("chan_info is not a data frame",
      call. = FALSE)
  }
  if(names(x)[3] != "gral_info"){
    stop("gral_info is missing",
      call. = FALSE)
  }
  if(!is.numeric(x$gral_info$srate) | x$gral_info$srate < 0 ){
    stop("Incorrect sampling rate",
      call. = FALSE)
  }
  purrr::walk(x, ~ is.list)
  purrr::walk(x$data, ~ purrr::walk(.x$signal, 
          ~ if(!is.data.frame(.x)) 
          stop("some 'signal' doesn't have a data frame",
          call. = FALSE )))
x
}  

eegbl <- function(x){
  validate_eegbl(x)
}

#' Applies a function to all the segments and returns an eegble.
#'
#' @param x An eegbl.
#' @param f_seg Function to apply to the segments.
#' @param parallel If set to TRUE, it will use all cores. 
#'    (Default: parallel = FALSE).

segmap_eggbl <- function(x, f_seg, parallel = FALSE){
  if(parallel){
    # the progress bar are not very useful
    modify <- function(...) furrr::future_modify(..., .progress = FALSE)
  } else {
    modify <- purrr::modify
  }

    x$data <- purrr::modify(x$data, function(data_file) {
                segs <- modify(data_file$signals, f_seg)
                list(signals = segs, events = data_file$events)
            })
  x
   
}
