summarize_eeg_lst <- function(.eeg_lst, dots){
   # # if there is something conditional on segments (O[condition == "faces"],
   # # I should add them to the signal_tbl df temporarily
                                      # cond_cols = cond_cols)

  cond_cols <- names_segments_col(.eeg_lst, dots)
  segment_groups <- intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$segments))
   summarize_eval_eeg_lst(.eeg_lst, eval = summarize_eval(dots), cond_cols)
}

summarize_at_eeg_lst <- function(.eeg_lst, vars, funs){
  segment_groups <- intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$segments))
  cond_cols <- names_segments_col(.eeg_lst, funs[[1]])
  summarize_eval_eeg_lst(.eeg_lst, eval = summarize_at_eval(vars, funs), cond_cols)
}

rollup_eeg_lst <- function(.eeg_lst, dots, level = c()){
  
  cond_cols <- names_segments_col(.eeg_lst, dots)
  summarize_eval_eeg_lst(.eeg_lst, eval = rollup_eval(dots, grouping), cond_cols, level)
}

rollup_at_eeg_lst <- function(.eeg_lst, vars, funs, level = c()){
  
  cond_cols <- names_segments_col(.eeg_lst, funs[[1]])
  summarize_eval_eeg_lst(.eeg_lst, eval = rollup_at_eval(vars, funs, grouping), cond_cols, level)
}

summarize_eval_eeg_lst <- function(.eeg_lst, eval, cond_cols, level = NULL){
   channels_info <- channels_tbl(.eeg_lst)
  .eeg_lst$signal <- summarize_eval_signal(.eeg_lst, eval, cond_cols)
      ## Restructure segments table to fit the new signal table
  if (nrow(.eeg_lst$signal) != 0) {
    last_id <- max(.eeg_lst$signal$.id)
  } else {
    last_id <- integer(0)
  }

  if(!is.null(level)){

    #if level is a vector of characters, this way I convert it to binary and then dec
    grouping <- level_to_grouping(level)
    segment_groups <- intersect(final_groups, colnames(.eeg_lst$segments)) 
  } else {
    segment_groups <- group_chr_segments(.eeg_lst)
  }

  .eeg_lst$segments <- summarize_segments(.eeg_lst$segments, 
                                            segments_groups = segment_groups,
                                            last_id= last_id ) 
    ## Restructure events table
    # TODO maybe I can do some type of summary of the events table, instead
    .eeg_lst$events <- .eeg_lst$events %>% filter(FALSE)

    #update channels in the events and the meta data (summarize deletes the metadata of the channels)
    .eeg_lst <- update_events_channels(.eeg_lst) %>% update_channels_tbl(channels_info)
    validate_eeg_lst(.eeg_lst)
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


summarize_eval_signal <- function(.eeg_lst, eval, cond_cols){
    # To update later
    attr_sample_id <- attributes(.eeg_lst$signal$.sample_id)
    extended_signal <- eval_signal(.eeg_lst, eval_txt = eval, cond_cols = cond_cols) 
    ## Restructure signal table
    # Recover lost attributes and columns of signal_id
    #Add obligatory cols (.id, .sample_id) in case they are missing  :
    if(!".sample_id" %in% colnames(extended_signal)) {
     extended_signal[,.sample_id := sample_int(NA_integer_, attr_sample_id$sampling_rate)]
    } else {
     attributes(extended_signal$.sample_id) <- attr_sample_id 
    }

    # Add .id in case it was removed by a summary
    if(!".id" %in% colnames(extended_signal)) {
     extended_signal[,.id := seq_len(.N), by =  .sample_id]
    }
    
    if(length(group_chr_only_segments(.eeg_lst))>0){
     extended_signal[, (group_chr_only_segments(.eeg_lst)) := NULL] 
    }

    data.table::setkey(extended_signal,.id,.sample_id)
    data.table::setcolorder(extended_signal,c(".id",".sample_id"))
    extended_signal
  }

