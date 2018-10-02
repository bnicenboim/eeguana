
#' Mutate/transmute/select/rename channels with signals, events or segments.
#' 
#' Manipulate the signal, events and the segments of an eegble.
#'
#' Wrappers for \link{dplyr}'s commands that act on different parts
#' \code{eggble} objects.  The part of the object that the wrapper should "act on" is set with  
#' \code{act_on(.data, ...)} where \code{...} should be \code{signal}, \code{events} or \code{segments}.
#'
#' The following wrappers have been implemented for \code{eegble} objects: 
#' \itemize{
#' \item \code{mutate()} adds new variables and preserves existing ones.
#' \item \code{mutate_all()} mutates all variables (except reserved ones: .id, samples, etc) .
#' \item \code{transmute()} drops existing variables.
#' \item \code{select()} keeps only the mentioned variables. 
#' \item \code{rename()}: keeps all variables.
#' \item \code{filter()}: finds segments/samples where conditions are true. Segments/samples where the condition evaluates to NA are dropped.
#' \item \code{left_join()}: Left-joins an external dataframe to one of the dataframes of the eegble.
#' \item \code{semi_join()}: Semi-joins an external dataframe to one of the dataframes of the eegble and updates the remaining dataframes correspondingly.
#' \item \code{anti_join()}: Anti-joins an external dataframe to one of the dataframes of the eegble and updates the remaining dataframes correspondingly.
#' \item  \code{group_by()}: allows that operations would be performed "by group".
#' \item  \code{ungroup()}: removes the grouping created by group_by.
#' }
#'
#' These commands always return the entire eegble so that
#' they can be ' piped using \link{magrittr}'s pipe, %>%.
#' 
#' @param .data An eegbl.
#' @param ... \code{signal}, \code{events} or \code{segments} for \code{act_on}, and see  \link{dplyr-package} for the different dplyr "verbs".
#' @return An eegble object. 
#'
#' @export
#' 
#' @examples
#' \dontrun{
#'
#' faces_segs %>% act_on(signal) %>%
#'                  select(O1, O2, P7, P8)
#' }
act_on <- function(.data, ...){
  UseMethod("act_on")
}

#' @export
act_on <- function(.data, ...) {
  dots <- rlang::enexprs(...)
  if(length(dots) > 1) {warning("Only the first argument of act_on will be used.")}
  based <- as.character(dots[[1]])
  dfs <-  c("signal", "events", "segments")
  if(!based %in% dfs) {stop(paste("act_on only permits",paste(dfs, collapse =  ", "), "as arguments." ))}
  attr(.data, "act_on") <- based
  validate_eegbl(.data)
}


#' @export
mutate_.eegbl<- function(.data, ...) {
  df <-  attr(.data, "act_on")
  .data[[df]] <- dplyr::mutate_(.data[[df]], ...)
  update_chans(.data) %>%   validate_eegbl 
}

#' @export
summarise_.eegbl<- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  segments_groups <- dplyr::groups(.data$segments)
  signal_groups <- dplyr::groups(.data$signal)


  .data$signal <- do_based_on_grps(.data$signal, 
                            ext_grouping_df = .data$segments, 
                            dplyr_fun = dplyr::summarize, dots) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(sample = if("sample"  %in% tbl_vars(.))
                                              sample else NA_integer_) %>%
                    dplyr::group_by(sample) %>%
                    dplyr::mutate(.id = seq(dplyr::n()) %>% as.integer) %>%
                    dplyr::group_by(!!!signal_groups)

  .data$segments <- dplyr::summarize(.data$segments) %>%
                    dplyr::ungroup() %>% 
                    dplyr::mutate(.id = seq(dplyr::n()) %>% as.integer,
                                recording = if("recording"  %in% tbl_vars(.))
                                              recording else NA_character_) %>%
                    dplyr::select(.id, dplyr::everything()) %>%
                    dplyr::group_by(recording) %>%
                    dplyr::mutate(segment = seq(dplyr::n())) %>%
                    dplyr::group_by(!!!segments_groups)
                    # dplyr::group_by_at(dplyr::vars(segments_groups)) #for chars

  update_chans(.data) %>%   validate_eegbl 
}

tbl_vars.eegbl <- function(x) {
  setdiff(tbl_vars(x$signal), c(".id", "sample"))
}

#' @export
groups.eegbl <- function(x) {
  groups(x$segments)
}

#' @export
 group_by_.eegbl<- function(.data, ..., .dots = list(), add = add) {
    # dots <- rlang::quos(segment)
    dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
    #divide dots according to if they belong to $signal or segments
    new_dots <- dots_by_df(dots, .data)

   .data$segments <- dplyr::group_by(.data$segments,!!!new_dots$segments, add = add)
   .data$signal <- dplyr::group_by(.data$signal,!!!new_dots$signal, add = add)
   update_chans(.data) %>%   validate_eegbl 
 }

#' @export
ungroup.eegbl<- function(.data, ..., add = add) {
  .data$segments <- dplyr::ungroup(.data$segments)
  update_chans(.data) %>%   validate_eegbl 
}

#' @export
mutate_all <- function(.tbl, .funs, ...) {
  UseMethod("mutate_all")
}

#' @export
mutate_all.default <- dplyr::mutate_all

#' @export
mutate_all.eegbl<- function(.tbl, .funs, ...) {
  df <-  attr(.tbl, "act_on")
  
  # excluding the obligatory_cols
  .tbl[[df]] <- dplyr::mutate_at(.tbl[[df]], 
                                 .vars = vars(-one_of(c(obligatory_cols[[df]]))), .funs, ...)
  update_chans(.tbl) %>%   validate_eegbl 
}



#' @export
transmute_.eegbl<- function(.data, ...) {
  df <-  attr(.data, "act_on")
  .data[[df]] <- dplyr::transmute_(.data[[df]], obligatory_cols[[df]], ...)
  update_chans(.data) %>%   validate_eegbl 
}


#' @export
rename_.eegbl <- function(.data, ...){
  df <-  attr(.data, "act_on")
  .data[[df]] <- dplyr::rename_(.data[[df]], ...)
  update_chans(.data) %>% validate_eegbl 
}  

#' @export
filter_.eegbl <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  #dots <- rlang::quo(recording == "0")
  new_dots <- dots_by_df(dots, .data)

  #filter the signal and update the segments, in case an entire id drops  
  if(length(new_dots$signal) > 0) { 

    .data$signal <- do_based_on_grps(.data$signal, 
                            ext_grouping_df = .data$segments, 
                            dplyr_fun = dplyr::filter, new_dots$signal)

    .data$segments <- dplyr::semi_join(.data$segments, .data$signal, by =".id")
  } 
  #filter the segments and update the signal
    if(length(new_dots$segments) > 0) { 
      .data$segments <- dplyr::filter(.data$segments, !!!new_dots$segments)
      .data$signal <- dplyr::semi_join(.data$signal, .data$segments, by =".id")
   }

  # Fix the indices in case some of them drop out
  redo_indices(.data) %>% 
  update_chans %>% validate_eegbl 
}  


#' @export
select.eegbl <- function(.data, ...){
    
   dots <- rlang::enquos(...)

   # here this is fine if there are no -, if there are I should treat it differently
   all_vars <- tidyselect::vars_select(c(names(.data$signal), names(.data$segments)), !!!dots)


   # Divide the variables into the relevant columns
   signal_col <- intersect(all_vars, colnames(.data$signal))  
                      
   segments_col <- intersect(all_vars, colnames(.data$segments)) 

   extra <- setdiff(union(signal_col, segments_col), all_vars)


   if(length(extra)>0){
    warning(paste("The following grouping variables were not found: ", 
      paste(extra, collapse = ", ") ))
   }

   # select if there is something to select:
   if(length(signal_col) != 0){
    .data$signal <- dplyr::select(.data$signal, obligatory_cols$signal,  one_of(signal_col))    
   }

   if(length(segments_col) != 0){
     .data$segments <- dplyr::select(.data$segments, obligatory_cols$segments, one_of(segments_col))
   }  


  # df <-  attr(.data, "act_on")
  # #this is to avoid removing important columns
  # dots <- rlang::enquos(...)
  # .data[[df]] <- dplyr::group_by_at(.data[[df]], obligatory_cols[[df]]) %>%
  #   select(!!!dots) %>% ungroup()
  update_chans(.data) %>% validate_eegbl 
} 


 

#' @export
left_join.eegbl <- function(x, y, by = NULL, suffix= c(".x", ".y"), ...){
  if(!is_eegble(x)) stop("x must be an eegble.")
  df <-  attr(x, "act_on")
  if(!df %in% c("segments","events")) stop("x must be act_on segments or events.")
  
  x[[df]] <-  dplyr::left_join(x[[df]], y, by = NULL, suffix= c(".x", ".y"), ...)
  
  validate_eegbl(x)
  
}



#' @export
semi_join.eegbl <- function(x, y, by = NULL, suffix= c(".x", ".y"), ...){
  if(!is_eegble(x)) stop("x must be an eegble.")
  df <-  attr(x, "act_on")
  if(!df %in% c("segments","events")) stop("x must be act_on segments or events.")
  
  x[[df]] <-  dplyr::semi_join(x[[df]], y, by = NULL, suffix= c(".x", ".y"), ...)
  x$signal <- dplyr::semi_join(x$signal, x[[df]], by = ".id")
  
  if(df %in% "events"){
    x$segments <- dplyr::semi_join(x$segments, x[[df]], by = ".id")
  }
  
  if(df %in% "segments"){
    x$events <- dplyr::semi_join(x$events, x[[df]], by = ".id")
  }
  
  validate_eegbl(x)
  
}


#' @export
anti_join.eegbl <- function(x, y, by = NULL, suffix= c(".x", ".y"), ...){
  if(!is_eegble(x)) stop("x must be an eegble.")
  df <-  attr(x, "act_on")
  if(!df %in% c("segments","events")) stop("x must be act_on segments or events.")
  
  x[[df]] <-  dplyr::anti_join(x[[df]], y, by = NULL, suffix= c(".x", ".y"), ...)
  x[["signal"]] <- dplyr::semi_join(x[["signal"]], x[[df]], by = ".id")
  
  if(df %in% "events"){
    x$segments <- dplyr::semi_join(x$segments, x[[df]], by = ".id")
  }
  
  if(df %in% "segments"){
    x$events <- dplyr::semi_join(x$events, x[[df]], by = ".id")
  }
  
  validate_eegbl(x)
  
}




##' @rdname mutate_signal
# not yet exported
filter_s <- function(.data, ...){
  dots <-  rlang::enquos(...)
  #should edit the dots to transform time to samples
  #then it filters by $signal
  .data$segments <- dplyr::filter(.data$segments, !!!dots)
  .data$signal <- dplyr::semi_join(.data$signal, .data$segments, by =".id")
  .data$events <- dplyr::semi_join(.data$events, .data$segments, by =".id")
  
  validate_eegbl(.data)
}   


#it could look for the names and check if they appear in data or segments