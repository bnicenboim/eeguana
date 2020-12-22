summarize_eeg_lst <- function(.eeg_lst, dots, .groups) {
  .eeg_lst$.signal <- summarize_eval_signal(.eeg_lst, dots)
  update_summarized_eeg_lst(.eeg_lst, .groups)
}

update_summarized_eeg_lst <- function(.eeg_lst,  .groups) {

  ## Restructure segments table to fit the new signal table
  if (nrow(.eeg_lst$.signal) != 0) {
    last_id <- max(.eeg_lst$.signal$.id)
  } else {
    last_id <- integer(0)
  }

  .eeg_lst$.segments <- summarize_segments(
    segments = .eeg_lst$.segments,
    segments_groups = group_vars_segments(.eeg_lst),
    last_id = last_id,
    .groups =  .groups
  )
  ## Restructure events table
  # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$.events <- new_events_tbl(sampling_rate = sampling_rate(.eeg_lst))
  # update channels in the events and the meta data (summarize deletes the metadata of the channels)
  .eeg_lst <- update_events_channels(.eeg_lst) #
  validate_eeg_lst(.eeg_lst)
}


#' @noRd
summarize_segments <- function(segments, segments_groups, last_id, .groups) {

  if (length(segments_groups) != 0) {
    # doesn't remove columns:
    grouped_seg <- unique(segments, by = segments_groups)
    # check if recording didn't change, if so leave it
    grouped_seg_rec <- unique(segments, by = c(segments_groups, ".recording"))
    if(identical(grouped_seg, grouped_seg_rec)){
      segments_groups <- unique(c(".recording", segments_groups))
    }
    grouped_seg <- grouped_seg[, segments_groups, with = FALSE]


    if (!".id" %in% segments_groups) {
      grouped_seg[, .id := seq_len(last_id)]
      data.table::setkey(grouped_seg, .id)
    }
    if (!".recording" %in% segments_groups) {
      grouped_seg[, .recording := NA]
    }
    data.table::setcolorder(grouped_seg, obligatory_cols[[".segments"]])
    grouped_seg
  } else {
    grouped_seg <-  data.table::data.table(.id = seq_len(last_id), .recording = NA)
    data.table::setkey(grouped_seg, .id)
    grouped_seg
  }
}

same_grouping <-  function(x,y) {
  identical(attributes(x)$groups$.rows, attributes(y)$groups$.rows)
}

summarize_eval_signal <- function(.eeg_lst, dots) {

  cond_cols <- names_other_col(.eeg_lst, dots, ".segments")
  extended_signal_dt <- extended_signal(.eeg_lst, cond_cols)
  by <- dplyr::group_vars(.eeg_lst)
  dots <- rlang::quos_auto_name(dots)
  add_names <- rlang::quos_auto_name(dots) %>% names()
  old_attributes <- purrr::map(
    add_names %>% strsplit("_"),
    ~ attributes(.eeg_lst$.signal[[.x[[1]]]])
  )
  old_attributes <- stats::setNames(old_attributes, add_names)
  extended_signal_dt <- summarize_dt(extended_signal_dt, !!!dots, group_by_ = by)

     #add class to the columns that lost their class
  extended_signal_dt[, (add_names) := purrr::map2(.SD, old_attributes, ~
  if (is_channel_dbl(.x) | is_component_dbl(.x)) .x else `attributes<-`(.x, .y)), .SDcols = add_names]


  update_summarized_signal(extended_signal_dt, .eeg_lst)
}




update_summarized_signal <- function(extended_signal, .eeg_lst) {
  attr_sample_id <- attributes(.eeg_lst$.signal$.sample)

  # TODO open bug to data.table, it looses the class
  if (!is_signal_tbl(extended_signal)) {
    class(extended_signal) <- c("signal_tbl", class(extended_signal))
  }

  ## Restructure signal table
  # Recover lost attributes and columns of signal_id
  # Add obligatory cols (.id, .sample) in case they are missing  :
  if (!".sample" %in% colnames(extended_signal)) {
    extended_signal[, .sample := sample_int(NA_integer_, attr_sample_id$sampling_rate)]
  } else {
    attributes(extended_signal$.sample) <- attr_sample_id
  }

  # Add .id in case it was removed by a summary
  if (!".id" %in% colnames(extended_signal)) {
    extended_signal[, .id := seq_len(.N), by = .sample]
  }

  if (length(group_vars_only_segments(.eeg_lst)) > 0) {
    extended_signal[, (group_vars_only_segments(.eeg_lst)) := NULL]
  }

  data.table::setkey(extended_signal, .id, .sample)
  data.table::setcolorder(extended_signal, c(".id", ".sample"))
  extended_signal[]
}
