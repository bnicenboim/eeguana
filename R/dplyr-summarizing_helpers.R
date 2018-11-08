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
  summarize_eval_eeg_lst(.eeg_lst, eval = rollup_eval(dots), cond_cols, level)
}

rollup_at_eeg_lst <- function(.eeg_lst, vars, funs, level = c()){
  cond_cols <- names_segments_col(.eeg_lst, funs[[1]])
  summarize_eval_eeg_lst(.eeg_lst, eval = rollup_at_eval(vars, funs), cond_cols, level)
}

summarize_eval_eeg_lst <- function(.eeg_lst, eval, cond_cols, level = dplyr::group_vars(.eeg_lst)){
   channels_info <- channels_tbl(.eeg_lst)
  .eeg_lst$signal <- summarize_eval_signal(.eeg_lst, eval, cond_cols, level)
      ## Restructure segments table to fit the new signal table
  if (nrow(.eeg_lst$signal) != 0) {
    last_id <- max(.eeg_lst$signal$.id)
  } else {
    last_id <- integer(0)
  }


  segment_groups <- intersect(level, colnames(.eeg_lst$segments)) 

    # should be  "group_chr_segments(.eeg_lst)" when  we don't remove any level

  .eeg_lst$segments <- summarize_segments(.eeg_lst$segments, 
                                            segments_groups = segment_groups,
                                            last_id= last_id ) 
    ## Restructure events table
    # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$events <- .eeg_lst$events %>% filter(FALSE)

   
  # updates groups in case rollup was used:
  
  if(is.null(level) || is.na(level) || length(level) == 0) {
    .eeg_lst <- dplyr::ungroup(.eeg_lst)
  } else {
    .eeg_lst <- dplyr::group_by(.eeg_lst, !!!rlang::syms(level)) 
  }
    #update channels in the events and the meta data (summarize deletes the metadata of the channels)
  .eeg_lst <- update_events_channels(.eeg_lst) %>% update_channels_tbl(channels_info)

  validate_eeg_lst(.eeg_lst)
}


#' @noRd
summarize_segments <-  function(segments, segments_groups, last_id){
    data.table::data.table(segments)[,unique(.SD),.SDcols=c(segments_groups)][] %>% dplyr::as_tibble() %>%
    # segments %>% 
          # dplyr::group_by_at(vars(segments_groups)) %>%
          # dplyr::summarize() %>%
          # dplyr::ungroup() %>%
      {
       if (!".id" %in% dplyr::tbl_vars(.)) {
         hd_add_column(., .id = seq_len(last_id))
       } else {
         .
       }
      } %>%
    dplyr::select(.id, dplyr::everything())
}


summarize_eval_signal <- function(.eeg_lst, eval, cond_cols, level ){
  # To update later
  attr_sample_id <- attributes(.eeg_lst$signal$.sample_id)
  extended_signal <- eval_signal(.eeg_lst, eval_txt = eval, cond_cols = cond_cols) 

  #TODO open bug to data.table, it looses the class
  if(!is_signal_tbl(extended_signal)) {
    class(extended_signal) <- c("signal_tbl", class(extended_signal))
  }

  #extract the right level in case rollup was used

    # we want to keep the grouping until the level, 
    # so the difference between the level and grouping cols should be NA.
  NAcols <- setdiff(dplyr::group_vars(.eeg_lst), level) 
  if(length(NAcols)>0) {
      extended_signal <- extended_signal[apply(extended_signal[,..NAcols],1,function(x) all(is.na(x)))]
 }
 extended_signal <- na.omit(extended_signal, level)


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
  extended_signal[]
}

