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


update_chans <- function(x){
    current_chans <- colnames(x$data)[!colnames(x$data) %in% c(".id","sample")]
    new_chans <- current_chans[!current_chans %in% x$chan_info$labels]
    #remove old channels
    x$chan_info <- dplyr::filter(x$chan_info, labels %in% current_chans) 
    # add new ones
    x$chan_info <- x$chan_info %>% dplyr::mutate(labels = as.character(labels)) %>% 
                    dplyr::bind_rows(tibble::tibble(labels = new_chans)) %>% 
                    dplyr::mutate(labels = as.factor(labels)) 
    x
  }

validate_dots <- function(...){
  dots <- rlang::enquos(...)  
  if(any(names(dots) %in% c(".id", "sample"))) {
      stop(".id and samples can't be manipulated")
    } else {
      dots
    }
}