decimals <- function(x) match(TRUE, round(x, 1:20) == x)
say_size <- function(eegble) paste("# Object size",capture.output(pryr::object_size(eegble)))


new_eegbl <- function(data = NULL, events = NULL, chan_info = NULL, eeg_info = NULL, seg_info = NULL) {
  x <- list(data =  data, events = events, 
                 chan_info = chan_info, eeg_info = eeg_info, seg_info = seg_info)
  x <- unclass(x)
  x$chan_info$labels <- forcats::as_factor(x$chan_info$labels, levels = x$chan_info$labels)
  
  # Force the channel to be a character and to have the same levels as there are channels
  x$events$channel <- as.character(x$events$channel) %>% 
                              forcats::lvls_expand(new_levels = x$chan_info$labels)

  structure(x,
    class = c("eegbl"))
}

validate_eegbl <- function(x) {

   if(!all(names(x) %in% c("data", "events", "chan_info", "eeg_info",  "seg_info"))){
        stop("Incomplete eegble object.",
      call. = FALSE)
   }

  if(!is.numeric(x$eeg_info$srate) | x$eeg_info$srate < 0 ){
    stop("Incorrect sampling rate",
      call. = FALSE)
  }
  if(!all(c("sample","size", "channel") %in% names(x$events))){
    stop("Missing fields in events",
      call. = FALSE)
  }

  if(!all(
  names(x$data)[1] == ".id",
  names(x$data)[2] == "sample",
  names(x$seg_info)[1] == ".id",
  names(x$events)[1] == ".id")){
 
    stop("Missing .id or sample fields",
      call. = FALSE)
  }
  if(!all(
   is.integer(x$data$.id),
   is.integer(x$data$sample),
   is.integer(x$seg_info$.id),
   is.integer(x$events$.id))){
    warning(".id or sample are not integers.")
   }

   if(!all(is.factor(x$chan_info$labels), is.factor(x$events$channel) )) {
     stop("Channel labels should be a factor",
      call. = FALSE)
  }
    if(!all(levels(x$chan_info$labels)== colnames(x$data)[c(-1,-2)] )){
           warning("Mismatch in label names",
      call. = FALSE)
    } 
    if(!all(levels(x$chan_info$labels)== levels(x$events$channel))){
           warning("Mismatch in channel names of events",
      call. = FALSE)
    }
  x
}  

update_chans <- function(x){
    current_chans <- colnames(x$data)[!colnames(x$data) %in% c(".id","sample")]
    new_chans <- current_chans[!current_chans %in% x$chan_info$labels]
    #remove old channels
    x$chan_info <- dplyr::filter(x$chan_info, labels %in% current_chans) 
    # add new ones
    x$chan_info <- x$chan_info %>% dplyr::mutate(labels = as.character(labels)) %>% 
                    dplyr::bind_rows(tibble::tibble(labels = new_chans)) %>% 
                    semi_join(tibble(labels = current_chans),by = "labels" ) %>%
                    dplyr::mutate(labels = forcats::as_factor(labels, levels = current_chans)) 
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


as_integer <- function(x){
  largest <- 2000000000
  x[x > largest] <- largest
  x[x < -largest] <- -largest
  as.integer(x) 
}



scaling <- function(x, unit){
    if(stringr::str_to_lower(unit) %in% c("s","sec","second","seconds","secs")) {
      scaling <- srate(x)
    } else if(stringr::str_to_lower(unit) %in% c("ms","msec", "millisecond",
                                                 "milli second", "milli-second", 
                                                 "milliseconds", "milli seconds", 
                                                 "msecs")) {
      scaling <- srate(x)/1000
    } else if(stringr::str_to_lower(unit) %in% c("sam","sample","samples")){
      scaling <- 1
    } else {
      stop("Incorrect unit. Please use 'ms', 's', or 'sample'")
    }
  }
