#' Get the by-sample (or by-row) mean of the specified channels.
#'
#' Wrapper of `rowMeans` that performs a by-sample mean of the specified channels.
#'
#' @param ... A channel or a group of channels, or an `eeg_lst` object.
#' @param na.rm 
#' @return A new channel or an `eeg_lst` object with a `mean` channel instead of the previous channels.
#' @family channel
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>%
#'                transmute(Occipital = chs_mean(O1, O2, Oz, na.rm = TRUE),
#'                          Parietal = chs_mean(P3, P4, P7,  P8, Pz, na.rm = TRUE))
#'
#' faces_segs_some %>%
#'                chs_mean(na.rm = TRUE)
#' }
#' @export
chs_mean <- function(x, ...,na.rm= FALSE) {
  UseMethod("chs_mean")
}


#' @export
chs_mean.channel_dbl <- function(..., na.rm = FALSE) {
  dots <- rlang::quos(...)

  # # signal_tbl <- signal_from_parent_frame(env = parent.frame())
  # # This is the environment where I can find the columns of signal_tbl
  # signal_env <- rlang::env_get(env = parent.frame(), ".top_env", inherit = TRUE)
  # signal_tbl <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))

 # https://stackoverflow.com/questions/17133522/invalid-internal-selfref-in-data-table

	 rowMeans_ch(data.table::data.table(...), na.rm = na.rm)  # throws a warning

  # rowMeans(copy(data.table::data.table(...)), na.rm = na.rm)  # throws a warning
  # rowMeans(.SD, na.rm = na.rm), .SDcols = cols # should be the way, but it's hard to implement it in my template

  # rowMeans(dplyr::select(signal_tbl, !!!dots), na.rm = na.rm)
}

#' @export
chs_mean.eeg_lst <- function(x, na.rm = FALSE) {
  #channels_info <- channels_tbl(x)
  signal <- data.table::copy(x$signal)
  signal[,mean := rowMeans_ch(.SD, na.rm = na.rm),.SDcols = channel_names(x)][,`:=`(channel_names(x), NULL)]
  x$signal <- signal
  update_events_channels(x) %>% #update_channels_tbl(channels_info) %>%
      validate_eeg_lst()
}


#' Rereference a channel or group of channels.
#'
#' Rereference a channel or group of channels.
#'
#' Notice that this function will update the channels one by one when used inside a mutate and not all at the same time.
#' 
#' @param x A channel to be referenced an eeg_lst where all the channels will be re-referenced (except for the ones in exclude).
#' @param ... Channels that will be averaged as the reference.
#' @param na.rm 
#' @param exclude  A character vector of channels to exclude from referencing.
#' @return A rereferenced channel or an eeg_lst with all channels re-referenced.
#' @export
#'
#' @family channel
#'
#' @examples
#' \dontrun{
#' # Rereference all channels used the linked mastoids (average of the two mastoids)
#'
#' faces_segs %>% ch_rereference(M1, M2)
#' }
#' @export
ch_rereference <- function(x, ...,na.rm= FALSE) {
  UseMethod("ch_rereference")
}

#' @export
ch_rereference.channel_dbl <- function(x, ..., na.rm = FALSE) {
   dots <- rlang::enquos(...)
   new_ref <-  purrr::map_chr(dots, rlang::quo_text) %>% paste0(collapse = ", ")

  {x - rowMeans(data.table::data.table(...), na.rm = na.rm)}  %>%
   { `attributes<-`(., c(attributes(.), 
                        list(.reference = new_ref))
                        ) }
  # {`attributes<-`(., list(.reference=2))}
 }

#' @export
ch_rereference.eeg_lst <- function(x,..., na.rm = FALSE, exclude = NULL) {
  #channels_info <- channels_tbl(x)
  signal <- data.table::copy(x$signal)
  dots <- rlang::enquos(...)
  cols <- rlang::quos_auto_name(dots) %>% names()
  ref <- rowMeans(x$signal[,..cols], na.rm = na.rm)
  reref <- function(x){
    x <- x - ref 
    attributes(x)$.reference <- purrr::map_chr(dots, rlang::quo_text) %>% paste0(collapse = ", ")
    # print(x)
    x
  }

  signal[, (channel_names(x)[!channel_names(x) %in% exclude]) := purrr::map(.SD, reref),.SDcols = channel_names(x)]
  x$signal <- signal
  update_events_channels(x) %>% #update_channels_tbl(channels_info) %>%
      validate_eeg_lst()

}



#' @export
chs_fun <- function(x, .funs, ...) {
  UseMethod("chs_fun")
}


#' @export
chs_fun.channel_dbl <- function(...,.funs, pars = list()) {
   row_fun_ch(data.table::data.table(...),.funs,  unlist(pars))  # throws a warning
}

#' @export
chs_fun.eeg_lst <- function(x,.funs, pars = list()) {

  signal <- data.table::copy(x$signal)
  funs <- dplyr:::as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env())
  fun_txt <- rlang::quo_text(funs[[1]]) %>% make.names()

  # TODO a more elegant way, but if pars is list(), then row_fun_ch thinks that ... is NULL, and the function gets an argument NULL
  if(length(pars) != 0){
    signal[,(fun_txt) := row_fun_ch(.SD, .funs,  unlist(pars)),.SDcols = channel_names(x)][,`:=`(channel_names(x), NULL)]
  } else {
    signal[,(fun_txt) := row_fun_ch(.SD, .funs),.SDcols = channel_names(x)][,`:=`(channel_names(x), NULL)]
  }
  x$signal <- signal
  update_events_channels(x) %>% #update_channels_tbl(channels_info) %>%
      validate_eeg_lst()
}
