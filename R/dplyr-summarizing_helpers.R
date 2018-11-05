#' @noRd
summarize_eval <- function(.dots){
  # https://community.rstudio.com/t/clarifying-question-when-is-manual-data-mask-needed-in-rlang-eval-tidy/11186
  #left joins then evaluates the summary by groups:
  # The following is very slow when grouping by ".sample_id"    "segment" 
  # col_expr <- rlang::get_expr(.dots)
  # new_signal <- rlang::quo(.eeg_lst$signal[segments, ..all_cols][
  #                 ,.(!!!col_expr), by = c(by)]) %>% 
  #   rlang::eval_tidy(data = .eeg_lst$signal)
  # .dots_expr <- rlang::get_expr(.dots)
 dots_txt <- purrr::imap(.dots, ~ if(.y!="") paste(.y, "=", rlang::quo_text(.x)) else rlang::quo_text(.x)) %>%
     paste0(., collapse = ", ")
  sprintf("new_signal[,.(%s), by = c(by)]", dots_txt)
} 

#' @noRd
summarize_at_eval <- function(.vars, .fun ){
  fun_txt <- rlang::quo_text(.fun[[1]])
  vars_txt <- paste0("'",.vars,"'",collapse =", ")
  sprintf("new_signal[,purrr::map(.SD,~ %s),.SDcols = c(%s) , by = c(by)][,..signal_cols]", fun_txt, vars_txt)
} 

#' @noRd
rollup_eval <- function(.dots, grouping){
  dots_txt <- purrr::imap(.dots, ~ if(.y!="") paste(.y, "=", rlang::quo_text(.x)) else rlang::quo_text(.x)) %>%
     paste0(., collapse = ", ")
  sprintf("rollup(new_signal,.(%s), by = c(by), id = TRUE)[grouping== %s][,..signal_cols]", dots_txt,grouping)
}

#' @noRd
rollup_at_eval <- function(.vars, .fun, grouping){
  fun_txt <- rlang::quo_text(.fun[[1]])
  vars_txt <- paste0("'",.vars,"'",collapse =", ")
  sprintf("rollup(new_signal,purrr::map(.SD,~ %s),.SDcols = c(%s), by = c(by), id = TRUE)[grouping== %s][,..signal_cols]", fun_txt, vars_txt, grouping)
}

#' @noRd
extended_signal <- function(signal, segments, all_cols){
  segments <- data.table::data.table(segments)
  data.table::setkey(segments,.id)
  signal[segments, ..all_cols]
}


#' @noRd
summarize_segments <-  function(segments, segments_groups, last_id){
    segments %>% 
          dplyr::group_by_at(vars(segments_groups)) %>%
          dplyr::summarize() %>%
          dplyr::ungroup() %>%
      {
       if (!".id" %in% dplyr::tbl_vars(.)) {
         hd_add_column(., .id = seq_len(last_id))
       } else {
         .
       }
      } %>%
    dplyr::select(.id, dplyr::everything())
}

summarize_eeg_lst <- function(.eeg_lst, dots){
   # # if there is something conditional on segments (O[condition == "faces"],
   # # I should add them to the signal_tbl df temporarily
                                      # cond_cols = cond_cols)

  cond_cols <- names_segments_col(.data, dots)
  segment_groups <- intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$segments))
  summarize_eval_eeg_lst(.eeg_lst, eval = summarize_eval(dots), cond_cols, segment_groups)
}

summarize_at_eeg_lst <- function(.eeg_lst, vars, funs){
  segment_groups <- intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$segments))
  cond_cols <- names_segments_col(.tbl, funs[[1]])
  summarize_eval_eeg_lst(.eeg_lst, eval = summarize_at_eval(vars, funs), cond_cols, segment_groups)
}

rollup_eeg_lst <- function(.eeg_lst, dots, level = c()){
  
  cond_cols <- names_segments_col(.data, dots)
  #if level is a vector of characters, this way I convert it to binary and then dec
  grouping <- level_to_grouping(level)
  segment_groups <- intersect(final_groups, colnames(.eeg_lst$segments)) 

  summarize_eval_eeg_lst(.eeg_lst, eval = rollup_eval(dots, grouping), cond_cols, segment_groups)
}

rollup_at_eeg_lst <- function(.eeg_lst, dots, level = c()){
  
  cond_cols <- names_segments_col(.tbl, funs[[1]])
  #if level is a vector of characters, this way I convert it to binary and then dec
  grouping <- level_to_grouping(level)
  segment_groups <- intersect(final_groups, colnames(.eeg_lst$segments)) 

  summarize_eval_eeg_lst(.eeg_lst, eval = rollup_at_eval(vars, funs, grouping), cond_cols, segment_groups)
}



summarize_eval_eeg_lst <- function(.eeg_lst, eval, cond_cols,segment_groups){
    
    # To update later
    channels_info <- channels_tbl(.eeg_lst)
    new_signal <- extended_signal(signal = .eeg_lst$signal, 
                                       segments = .eeg_lst$segments, 
                                       all_cols = c(dplyr::group_vars(.eeg_lst),cond_cols) %>% unique())
    
    # by, signal_cols and new_signal are required for the eval
    by <- group_vars(.eeg_lst)
    signal_cols <- colnames(.eeg_lst$signal)
    new_signal <- eval(parse(text= eval)) #uses `new_signal`  and `by` in the evaluation

    ## Restructure signal table
    # Recover lost attributes and columns of signal_id
    attr_sample_id <- attributes(.eeg_lst$signal$.sample_id)
    #Add obligatory cols (.id, .sample_id) in case they are missing  :
    if(!".sample_id" %in% colnames(new_signal)) {
     new_signal[,.sample_id := sample_int(NA_integer_, attr_sample_id$sampling_rate)]
    } else {
     attributes(new_signal$.sample_id) <- attr_sample_id 
    }

    # Add .id in case it was removed by a summary
    if(!".id" %in% colnames(new_signal)) {
     new_signal[,.id := seq_len(.N), by =  .sample_id]
    }

    data.table::setkey(new_signal,.id,.sample_id)
    data.table::setcolorder(new_signal,c(".id",".sample_id"))

    ## Restructure segments table to fit the new signal table
    if (nrow(.eeg_lst$signal) != 0) {
      last_id <- max(.eeg_lst$signal$.id)
    } else {
      last_id <- integer(0)
    }
   .eeg_lst$segments <- summarize_segments(.eeg_lst$segments, 
                                            segments_groups = segment_groups,
                                            last_id= last_id ) 

    ## Restructure events table
    # TODO maybe I can do some type of summary of the events table, instead
    .eeg_lst$events <- .eeg_lst$events %>% filter(FALSE)

    #update channels in the events and the meta data (summarize deletes the metadata of the channels)
    .eeg_lst <- update_events_channels(.eeg_lst) %>% update_channels_tbl(channels_info)
    
 }
