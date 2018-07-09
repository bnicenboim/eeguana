decimals <- function(x) match(TRUE, round(x, 1:20) == x)
say_size <- function(eegble) paste("# Object size",capture.output(pryr::object_size(eegble)))


new_eegbl <- function(signal = NULL, events = NULL, channels = NULL, info = NULL, segments = NULL) {
  x <- list(signal =  signal, events = events, 
                 channels = channels, info = info, segments = segments)
  x <- unclass(x)
  x$channels$labels <- forcats::as_factor(x$channels$labels, levels = x$channels$labels)
  
  # Force the channel to be a character and to have the same levels as there are channels
  x$events$channel <- as.character(x$events$channel) %>% 
                              forcats::lvls_expand(new_levels = x$channels$labels)

  x$signal <- dplyr::group_by(x$signal, .id)
  x$events <- dplyr::group_by(x$events, .id)
  x$segments <- dplyr::group_by(x$segments, .id)
  structure(x,
    class = c("eegbl"))
}

validate_eegbl <- function(x) {

   if(!all(names(x) %in% c("signal", "events", "channels", "info",  "segments"))){
        stop("Incomplete eegble object.",
      call. = FALSE)
   }

  if(!is.numeric(x$info$srate) | x$info$srate < 0 ){
    stop("Incorrect sampling rate",
      call. = FALSE)
  }
  if(!all(c("sample","size", "channel") %in% names(x$events))){
    stop("Missing fields in events",
      call. = FALSE)
  }

  #USE obligatory_cols to validate
  if(!all(
  names(x$signal)[1] == ".id",
  names(x$signal)[2] == "sample",
  names(x$segments)[1] == ".id",
  names(x$events)[1] == ".id")){
 
    stop("Missing .id or sample fields",
      call. = FALSE)
  }
  if(!all(
   is.integer(x$signal$.id),
   is.integer(x$signal$sample),
   is.integer(x$segments$.id),
   is.integer(x$events$.id))){
    warning(".id or sample are not integers.")
   }

   if(!all(is.factor(x$channels$labels), is.factor(x$events$channel) )) {
     stop("Channel labels should be a factor",
      call. = FALSE)
  }
    if(!all(levels(x$channels$labels)== colnames(x$signal)[c(-1,-2)] )){
           warning("Mismatch in label names",
      call. = FALSE)
    } 
    if(!all(levels(x$channels$labels)== levels(x$events$channel))){
           warning("Mismatch in channel names of events",
      call. = FALSE)
    }
  x
}  


obligatory_cols <- list(signal = c(".id","sample"),
                         events = c(".id", "sample","size","channel"),
                         channels = c("labels","x","y","z"),
                         segments = c(".id")
                         )

update_chans <- function(x){
    current_chans <- colnames(x$signal)[!colnames(x$signal) %in% c(".id","sample")]
    added_chans <- current_chans[!current_chans %in% x$channels$labels]
    #remove old channels
    x$channels <- dplyr::filter(x$channels, labels %in% current_chans) 
    x$events <- dplyr::filter(x$events, channel %in% current_chans)  
    
    # add new ones
    x$channels <- x$channels %>% dplyr::mutate(labels = as.character(labels)) %>% 
                    dplyr::bind_rows(tibble::tibble(labels = added_chans)) %>% 
                    dplyr::semi_join(tibble::tibble(labels = current_chans),by = "labels" ) %>%
                    dplyr::right_join(dplyr::tibble(labels=current_chans),by="labels") %>%
                  dplyr::mutate(labels = forcats::as_factor(labels)) 
    
    x$events <- x$events %>% mutate(channel =  forcats::fct_drop(channel) %>% 
               forcats::lvls_expand(new_levels = current_chans))
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
