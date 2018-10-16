#' @noRd
update_events_channels <- function(x) {
  x$events <- dplyr::filter(x$events, is.na(.channel) | .channel %in% channel_names(x))
  x  
}


# https://stackoverflow.com/questions/50563895/using-rlang-find-the-data-pronoun-in-a-set-of-quosures
#' @noRd
getAST <- function(ee) {
  as.list(ee) %>% purrr::map_if(is.call, getAST)
}

#' @noRd

#TODO: use str_* to make the signal_cols more general, 
#it should ignore if there is a function that starts with ch_ (using is.function)
dots_by_df <- function(dots, .eegbl) {
  signal_cols <- c(channel_names(.eegbl),".id", ".sample_id","chs_mean")

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
  # if both signal and segments columns are there, it will say that the dots should apply
  # to a signal dataframe.

  list(signal = dots[signal_dots], segments = dots[!signal_dots])
}


#' @importFrom rlang :=
# this function basically applies a dplyr function (dplyr_fun) to $signal based on groups of segments (ext_grouping_df)
#' @noRd
do_based_on_grps <- function(.df, ext_grouping_df, dplyr_fun, dots) {
  int_groups <- dplyr::groups(.df)
  ext_group_names <- dplyr::group_vars(ext_grouping_df)
  sampling_rate <- attributes(.df$.sample_id)$sampling_rate

  id <- .df$.id
  # list of groups from segments to create on the fly
  new_groups <- purrr::map(
    ext_group_names,
    ~rlang::expr(ext_grouping_df[id, ][[!!.x]])
  ) %>%
    purrr::set_names(ext_group_names)

  # I need to ungroup first because if not, the other groups need ot be the size of the grouping that were already made and not the size of the entire signal df
  .df <- dplyr::ungroup(.df) %>%
    # TODO: check the following
    # maybe doing a left_join and then group would be not slower
    dplyr::group_by(!!!new_groups, !!!int_groups) %>%
    dplyr_fun(!!!dots) %>% # after summarizing I add the .id
    dplyr::ungroup(.df) %>%
    dplyr::select(-dplyr::one_of(ext_group_names))



    # in case obligatory cols are gone was removed :
    if(nrow(.df)>0){
      if(!".id" %in% dplyr::tbl_vars(.df)){
        .df <- dplyr::mutate(.df, .id = NA_integer_)
      }
      if(!".sample_id" %in% dplyr::tbl_vars(.df)){
        # This just creates a channel, because of the reclass
        # .df <- dplyr::mutate(.df, .sample_id = new_sample_id(NA_integer_, 
        #   sampling_rate = sampling_rate) )
        .df$.sample_id <- new_sample_id(rep(NA_integer_,nrow(.df)), sampling_rate = sampling_rate)
      }
    }
      if(nrow(.df)==0){
      if(!".id" %in% dplyr::tbl_vars(.df)){
        .df <- dplyr::mutate(.df, .id = integer(0))
      }
      if(!".sample_id" %in% dplyr::tbl_vars(.df)){
        # .df <- dplyr::mutate(.df, .sample_id = sample_id(integer(0), 
        #   sampling_rate = sampling_rate) )
        .df$.sample_id = new_sample_id(integer(0), sampling_rate = sampling_rate)
      }
    }

    # for(col in obligatory_cols$signal){
    #   if(!col %in% dplyr::tbl_vars(.df)){
    #     .df <- dplyr::mutate(.df, !!rlang::sym(col) := NA_integer_)
    #   }
    # }

    dplyr::select(.df, obligatory_cols$signal, dplyr::everything())
}


#' @noRd
scaling <- function(sampling_rate, unit) {
  if (stringr::str_to_lower(unit) %in% c("s", "sec", "second", "seconds", "secs")) {
    scaling <- sampling_rate
  } else if (stringr::str_to_lower(unit) %in% c(
    "ms", "msec", "millisecond",
    "milli second", "milli-second",
    "milliseconds", "milli seconds",
    "msecs"
  )) {
    scaling <- sampling_rate / 1000
  } else if (stringr::str_to_lower(unit) %in% c("sam", "sample", "samples")) {
    scaling <- 1
  } else {
    stop("Incorrect unit. Please use 'ms', 's', or 'sample'")
  }
}


#' @noRd
redo_indices <- function(.eeg_lst) {
  redo_indices_df <- function(df) {
    orig_groups <- dplyr::groups(df)
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(.id = dplyr::group_indices(., .id) %>% as.integer()) %>%
      dplyr::group_by(!!!orig_groups)
  }

  .eeg_lst$signal <- redo_indices_df(.eeg_lst$signal)
  .eeg_lst$segments <- redo_indices_df(.eeg_lst$segments)
  .eeg_lst
}

#' @noRd
names_segments_col <- function(.eeg_lst, dots) {
  segments_cols <- setdiff(colnames(.eeg_lst$segments), ".id") # removes .id

  names_s <- c()
  for (n in seq_len(length(dots))) {
    # get the AST of each call and unlist it
    names_s <- c(names_s, getAST(dots[[n]]) %>%
      unlist(.) %>%
      # make it a vector of strings
      purrr::map_chr(~rlang::quo_text(.x)) %>%
      dplyr::intersect(segments_cols))
  }
  unique(names_s)
}


#' Add a column to (an empty) table
#' Taken from https://community.rstudio.com/t/cannot-add-column-to-empty-tibble/1903/11
#' @noRd
hd_add_column <- function(.data, ..., .before = NULL, .after = NULL) {
  if (nrow(.data) == 0L) {
    return(tibble::tibble(...))
  }
  return(tibble::add_column(.data, ..., .before = .before, .after = .after))
}


#' @noRd
validate_segments <- function(segments) {
  # Validates .id
  if(all(unique(segments$.id) != seq_len(max(segments$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }

}


#' @noRd
group_vars_int <- function(eeg_lst) {
  list(signal = group_vars(eeg_lst$signal), segments = group_vars(eeg_lst$segments))
}

#' @noRd
groups_int <- function(eeg_lst) {
  list(signal = groups(eeg_lst$signal), segments = groups(eeg_lst$segments))
}


#' @noRd
group_by_id <- function(eeg_lst) {
  orig_groups <- dplyr::group_vars(eeg_lst)
   #if there are many groupings
  if(length(orig_groups) > 1 | 
    #or if the only one is not .id
     (length(orig_groups) == 1 & orig_groups[1] != ".id")) { 
    message("# Grouping by .id.")
  } 

  dplyr::group_by(eeg_lst, .id) 
}  

#' @noRd
signal_from_parent_frame <- function(env = parent.frame()){
  # This is the environment where I can find the columns of signal
  signal_env <- rlang::env_get(env = env, '.top_env', inherit = TRUE)
  signal <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))
}
