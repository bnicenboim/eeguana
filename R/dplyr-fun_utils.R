#' @noRd
extended_signal <- function(.eeg_lst, cond_cols){
  relevant_cols <- c(obligatory_cols$segments, group_chr(.eeg_lst),cond_cols)
print(relevant_cols)
 if(length(relevant_cols)>1) {
  segments <-  dplyr::ungroup(.eeg_lst$segments) %>% 
               dplyr::select_if(names(.) %in% relevant_cols) %>%
               data.table::data.table()
  data.table::setkey(segments,.id)
  return(.eeg_lst$signal[segments])
 } else {
  return(.eeg_lst$signal)
 }
}

eval_signal <- function(.eeg_lst, eval_txt, cond_cols, out_cols = colnames(.eeg_lst$signal),remove_cols = NULL){
  extended_signal <- extended_signal(.eeg_lst, cond_cols) 
  out_cols
  by <- group_chr(.eeg_lst)
  # eval needs signal_cols and extended signal and by
  eval(parse(text= eval_txt)) 
}

#' @noRd
group_chr <- function(.eeg_lst){
 dplyr::group_vars(.eeg_lst) %>%  unlist()
} 

#' @noRd
update_events_channels <- function(x) {
  if(nrow(x$events)>0) {
    x$events <- x$events[is.na(.channel) | .channel %in% channel_names(x),]
  }
  x
}


# https://stackoverflow.com/questions/50563895/using-rlang-find-the-data-pronoun-in-a-set-of-quosures
#' @noRd
getAST <- function(ee) {
  as.list(ee) %>% purrr::map_if(is.call, getAST)
}

#' @noRd
dots_by_tbl_quos <- function(.eeg_lst, dots) {
# TODO: use str_* to make the signal_cols more general,
# it should ignore if there is a function that starts with ch_ (using is.function)
  signal_cols <- c(channel_names(.eeg_lst), ".id", ".sample_id", "chs_mean")

  signal_dots <- purrr::map_lgl(dots, function(dot)
  # get the AST of each call and unlist it
    getAST(dot) %>%
      unlist(.) %>%
      # make it a vector of strings
      purrr::map_chr(~rlang::quo_text(.x)) %>%
      # check if it's some channel (might be problematic if a channel is named like function)
      {
        length(dplyr::intersect(., signal_cols)) > 0
      })

  # signal_dots is a vector of TRUE/FALSE indicating for each call whether it belongs to signals
  # if both signal_tbl and segments columns are there, it will say that the dots should apply
  # to a signal_tbl dataframe.
  segments <- c(dots[!signal_dots], dots[signal_dots][rlang::quos(.id) %in% dots[signal_dots]])
  list(signal = dots[signal_dots], segments = segments)
}