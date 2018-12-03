#' @noRd
extended_signal <- function(.eeg_lst, cond_cols){
  relevant_cols <- c(obligatory_cols$segments, group_chr(.eeg_lst),cond_cols)

 if(length(group_chr_only_segments(.eeg_lst))>0) {
  segments <-  dplyr::ungroup(.eeg_lst$segments) %>% 
               dplyr::select_if(names(.) %in% relevant_cols) %>%
               data.table::data.table()
  data.table::setkey(segments,.id)
  return(.eeg_lst$signal[segments])
 } else {
  return(data.table::copy(.eeg_lst$signal))
 }
}

# eval_signal <- function(.eeg_lst, eval_txt, cond_cols, out_cols = colnames(.eeg_lst$signal),remove_cols = NULL){
#   extended_signal <- extended_signal(.eeg_lst, cond_cols) 
#   out_cols
#   by <- dplyr::group_vars(.eeg_lst)
#   # eval needs signal_cols and extended signal and by
#   eval(parse(text= eval_txt)) 
# }

#' @noRd
group_chr <- function(.eeg_lst){
 dplyr::group_vars(.eeg_lst) %>%  unlist()
} 

#' @noRd
group_chr_segments <- function(.eeg_lst){
 intersect(group_chr(.eeg_lst), colnames(.eeg_lst$segments))
} 

#' @noRd
group_chr_only_segments <- function(.eeg_lst){
 group_chr_segments(.eeg_lst) %>% {.[. != ".id"]}
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

  signal_cols <- c(channel_names(.eeg_lst),
                     paste0("`",channel_names(.eeg_lst),"`"), #In case channel name is used with ` in the function call
                     ".id", ".sample_id")

  signal_dots <- purrr::map_lgl(dots, function(dot)
  # get the AST of each call and unlist it
    getAST(dot) %>%
      unlist(.) %>%
      # make it a vector of strings
      purrr::map_lgl(function(element){  # check for every element if it's a channel or if it's a channel function
        txt_element <- rlang::quo_text(element) 
        if (txt_element %in% signal_cols) {
          return(TRUE)
        } else if(exists(txt_element) && is.function(eval(parse(text= txt_element)))){
          return(stringr::str_detect(txt_element, stringr::regex("^ch_|^chs_|^channel_dbl|^channel_names")))
        } else {
          return(FALSE)
        }
        }) %>% any())

# things might fail if there is a function named as a signal column. TODO, check for that in the validate.

  # signal_dots is a vector of TRUE/FALSE indicating for each call whether it belongs to signals
  # if both signal_tbl and segments columns are there, it will say that the dots should apply
  # to a signal_tbl dataframe.
  segments <- c(dots[!signal_dots], dots[signal_dots][rlang::quos(.id) %in% dots[signal_dots]])
  list(signal = dots[signal_dots], segments = segments)
}


# getAST(dot) %>%
#       unlist(.) %>% purrr::map(function (e) {
#         e <- rlang::quo_text(e) %>% rlang::sym() 
#         print(e)
#         is.function(!!e)})

# is.function(!!rlang::quo(mean))
# is.function(mean)
# is.function(eval(parse(text= "M1")) )
# exists("mean")
# is.function(parse(text= "M1"))

# getAST(dot)[[2]][[1]] %>% class()
# getAST(dot)[[2]][[2]] %>% class()

# dot2 <- rlang::quo(mean)
# getAST(dot2)[[2]]  %>% purrr::is_function()

# xx <- getAST(dot2)[[2]]
# is.function(!!rlang::as_quosure(xx, env = NULL))
# is.function(mean)

# is.function(rlang::expr("mean()"))

