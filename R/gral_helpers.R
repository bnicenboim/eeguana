redo_indices <- function(.eegbl){

   redo_indices_df <- function(df){
    orig_groups <- dplyr::groups(df)
    df <- df %>% 
                  dplyr::ungroup() %>% 
                  dplyr::mutate(.id = dplyr::group_indices(., .id) %>% as.integer()) %>%
                  dplyr::group_by(!!!orig_groups)
   }

  .eegbl$signal <- redo_indices_df(.eegbl$signal)
  .eegbl$segments <- redo_indices_df(.eegbl$segments)
  .eegbl
}

 #this function basically applies a dplyr function (dplyr_fun) to $signal based on groups of segments (ext_grouping_df) 
 do_based_on_grps <- function(.df, ext_grouping_df, dplyr_fun, dots){
  int_groups <- dplyr::groups(.df)
  ext_group_names <- dplyr::group_vars(ext_grouping_df)
  id <- .df$.id
  # list of groups from segments to create on the fly
  new_groups <- purrr::map(ext_group_names,
                              ~ rlang::expr(ext_grouping_df[id,][[!!.x]])) %>%
                  purrr::set_names(ext_group_names)

  # I need to ungroup first because if not, the other groups need ot be the size of the grouping that were already made and not the size of the entire signal df  
  .df <- dplyr::ungroup(.df) %>%   
                  #TODO: check the following
                  #maybe doing a left_join and then group would be not slower 
                  dplyr::group_by(!!!new_groups, !!!int_groups) %>%
                  dplyr_fun(!!!dots) %>%  # after summarizing I add the .id
                  dplyr::ungroup(.df) %>%
                  dplyr::select( -dplyr::one_of(ext_group_names) ) %>%
                  #in case .id was removed :
                  dplyr::mutate(.id = if(".id"  %in% tbl_vars(.))
                                              .id else NA_integer_,
                                sample = if("sample"  %in% tbl_vars(.))
                                              sample else NA_integer_) %>%
                  dplyr::select(.id, sample, dplyr::everything())

  }


# https://stackoverflow.com/questions/50563895/using-rlang-find-the-data-pronoun-in-a-set-of-quosures
getAST <- function( ee ) { as.list(ee) %>% purrr::map_if(is.call, getAST) }

dots_by_df <- function(dots, .eegbl){
  signal_cols <- c(channel_names(.eegbl), "sample", "channelMeans")

  signal_dots <- purrr::map_lgl(dots, function(dot)
    # get the AST of each call and unlist it
    getAST(dot) %>% unlist(.) %>% 
    # make it a vector of strings
    purrr::map_chr(~ rlang::quo_text(.x)) %>% 
    # check if it's some channel (might be problematic if a channel is named like function)
    {length(dplyr::intersect(., signal_cols)) > 0})

  #returns a vector of TRUE/FALSE indicating for each call whether it belongs to signals
  # if both signal and segments columns are there, it will say that the dots should apply
  # to a signal dataframe. 
  
 list(signal = dots[signal_dots], segments = dots[!signal_dots])
}

names_segments_col <- function(.eegbl, dots){
  segments_cols <- setdiff(colnames(.eegbl$segments), ".id")  #removes .id
  
  names_s <- c()
  for(n in seq_len(length(dots))){
    # get the AST of each call and unlist it
    names_s <- c(names_s, getAST(dots[[n]]) %>% unlist(.) %>% 
    # make it a vector of strings
    purrr::map_chr(~ rlang::quo_text(.x)) %>%
    dplyr::intersect(segments_cols))  
  }
 unique(names_s)
  
}

# add a column to an empty table
#https://community.rstudio.com/t/cannot-add-column-to-empty-tibble/1903/11
hd_add_column <- function(.data, ..., .before = NULL, .after = NULL) {
    if (nrow(.data) == 0L) {
        return(tibble::tibble(...))
    }
    return(tibble::add_column(.data, ..., .before = .before, .after = .after))
}


decimals <- function(x) match(TRUE, round(x, 1:20) == x)
say_size <- function(eegble) paste("# Object size in memory", 
                capture.output(pryr::object_size(eegble)))

#' Smallest divisor of x, starting by 2
mindiv <- function(x, start = 2) {
  div <- start
  while(round(x) %% div != 0){
    div <- div + 1
  }
  div
}


#' Get integers so that their prod is approx N
factors <- function(N){
  out <- c()
  while(N != 1){
    for(i in 10:1){
      if(i == 1) {
       N <- N - 1
      } else if(N %% i == 0) {
        out <- c(out, i) 
        N <- N/i
        break
      }
    }}
    out
}



new_eegbl <- function(signal = NULL, events = NULL, channels = NULL, info = NULL, segments = NULL) {
  x <- list(signal =  signal, events = events, 
                 channels = channels, info = info, segments = segments)
  x <- unclass(x)
  x$channels$labels <- forcats::as_factor(x$channels$labels, levels = x$channels$labels)
  
  # Force the channel to be a character and to have the same levels as there are channels
  x$events$channel <- as.character(x$events$channel) %>% 
                              forcats::lvls_expand(new_levels = x$channels$labels)

  # x$signal <- dplyr::group_by(x$signal, .id)
  # x$events <- dplyr::group_by(x$events, .id)
  # x$segments <- dplyr::group_by(x$segments, .id)
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
                         segments = c(".id","segment","recording")
                         )

update_chans <- function(x){
    current_chans <- colnames(x$signal)[!colnames(x$signal) %in% c(".id","sample")]
    added_chans <- current_chans[!current_chans %in% x$channels$labels]
    #remove old channels
    x$channels <- dplyr::filter(x$channels, labels %in% current_chans) 
    x$events <- dplyr::filter(x$events, channel %in% current_chans | is.na(channel) )  
    
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


vec_mean <- function(..., na.rm = FALSE){
  purrr::pmap_dbl(list(...), ~ mean(c(...), na.rm = FALSE))
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
