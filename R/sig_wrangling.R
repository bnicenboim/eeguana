
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

# #' @export
# group_by_.eegbl<- function(.data, ..., add = add) {
#   df <-  attr(.data, "act_on")
#   .data[[df]] <- dplyr::group_by_(.data[[df]], ..., add = add)
#   update_chans(.data) %>%   validate_eegbl 
# }

# #' @export
# ungroup.eegbl<- function(.data, ..., add = add) {
#   df <-  attr(.data, "act_on")
#   .data[[df]] <- dplyr::ungroup(.data[[df]])
#   update_chans(.data) %>%   validate_eegbl 
# }

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
filter_.eegbl <- function(.data, ...){
  df <-  attr(.data, "act_on")
  if(df %in% "segments") {
    .data[[df]] <- dplyr::filter_(.data[[df]], ...)
    .data$signal <- dplyr::semi_join(.data$signal, .data$segments, by =".id")
    .data$events <- dplyr::semi_join(.data$events, .data$segments, by =".id")
  } else {
    stop("Filter only defined for segments")
  }
  update_chans(.data) %>% validate_eegbl 
}  

#' @export
select.eegbl <- function(.data, ...){
  df <-  attr(.data, "act_on")
  #this is to avoid removing important columns
  dots <- rlang::enquos(...)
  .data[[df]] <- dplyr::group_by_at(.data[[df]], obligatory_cols[[df]]) %>%
    select(!!!dots) %>% group_by(.id)
  update_chans(.data) %>% validate_eegbl 
} 



# #' @export
# mutate.eegbl<- function(.data, ...) {
#   print("aa")
#   df <-  attr(.data, "act_on")
#   dots <- rlang::enquos(...) 
#   .data[[df]] <- dplyr::mutate(.data[[df]], !!!dots)
#    update_chans(.data) %>% validate_eegbl 
# }


# #' @export
# rename.eegbl <- function(.data, ...){
#    df <-  attr(.data, "act_on")
#    dots <- rlang::enquos(...) 
#   .data[[df]] <- dplyr::rename(.data[[df]], !!!dots)
#   update_chans(.data) %>% validate_eegbl 
# }  

# #' @export
# filter.eegbl <- function(.data, ...){
#    df <-  attr(.data, "act_on")
#    dots <- rlang::enquos(...) 
#    if(df %in% "segments") {
#     .data[[df]] <- dplyr::filter(.data[[df]], !!!dots)
#     .data$signal <- dplyr::semi_join(.data$signal, .data$segments, by =".id")
#     .data$events <- dplyr::semi_join(.data$events, .data$segments, by =".id")
#    } else {
#     stop("Filter only defined for segments")
#    }
#   update_chans(.data) %>% validate_eegbl 
# }  

# #' @export
# select.eegbl <- function(.data, ...){
#    df <-  attr(.data, "act_on")
#    dots <- rlang::enquos(...) 

#    #this is to avoid removing important columns
#   .data[[df]] <- dplyr::group_by_at(.data[[df]], obligatory_cols[[df]]) %>%
#                 select(!!!dots) %>% group_by(.id)
#   update_chans(.data) %>% validate_eegbl 
# } 



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