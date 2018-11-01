#' @noRd
mutate_transmute <- function(.eeg_lst, mutate = TRUE, .dots) {
  if (mutate) {
    dplyr_fun <- dplyr::mutate
  } else {
    dplyr_fun <- dplyr::transmute
  }
  # For testing:
  # .dots <- rlang::quos(Occipital = (O1 + O2 + Oz)/3)
  new_dots <- dots_by_df(.dots, .eeg_lst)

  if (length(new_dots$signal) > 0) {

    # I add the missing variables in case one uses transmute,
    # it doesn't hurt to mutate. This prevents from deleting sample or .id
    missing_vars <- dplyr::setdiff(
      obligatory_cols$signal,
      dplyr::group_vars(.eeg_lst$signal)
    )
    .dots <- c(rlang::syms(missing_vars), new_dots$signal)
    .eeg_lst$signal <- do_based_on_grps(
      .df = .eeg_lst$signal,
      ext_grouping_df = .eeg_lst$segments,
      dplyr_fun = dplyr_fun,
      dots = .dots
    )
  }

  if (length(new_dots$segments) > 0) {
    missing_vars <- dplyr::setdiff(
      obligatory_cols$segments,
      dplyr::group_vars(.eeg_lst$segments)
    ) %>%
      rlang::syms(.)

    .eeg_lst$segments <- dplyr_fun(
      .eeg_lst$segments, !!!missing_vars,
      !!!new_dots$segments
    )
  }
  update_events_channels(.eeg_lst) %>% validate_eeg_lst()
}

#' @noRd
summarize_eeg_lst <- function(.eeg_lst, .dots){
  segments_groups <- attributes(.eeg_lst)$vars$segments
  signal_groups <- attributes(.eeg_lst)$vars$signal


  # if there is something conditional on segments (O[condition == "faces"],
  # I should add them to the signal_tbl df temporarily
  add_cols <- names_segments_col(.eeg_lst, .dots)



  .dots_expr <- rlang::get_expr(.dots)

  segments <- data.table::data.table(.eeg_lst$segments)
  data.table::setkey(segments,.id)
  all_cols <- c(colnames(.eeg_lst$signal),segments_groups, add_cols) %>% unique()
  
  unique_segments_groups <- segments_groups[segments_groups!=".id"]
  
  col_expr <- rlang::get_expr(.dots)

  # https://community.rstudio.com/t/clarifying-question-when-is-manual-data-mask-needed-in-rlang-eval-tidy/11186
  #left joins then evaluates the summary by groups:
  new_signal <- rlang::quo(.eeg_lst$signal[segments, ..all_cols][
                  ,.(!!!col_expr), by = c(signal_groups, unique_segments_groups)]) %>% 
    rlang::eval_tidy(data = .eeg_lst$signal)

  #Add obligatory cols (.id, .sample_id) in case they are missing  :
   if(!".sample_id" %in% colnames(new_signal)) {
    new_signal[,.sample_id := sample_int(NA_integer_, sampling_rate(.eeg_lst))]
   }
   if(!".id" %in% colnames(new_signal)) {
    new_signal[,.id := seq.int(.N), by =  .sample_id]
   }
   
   #remove unnecesary columns
  if(length(unique_segments_groups)!=0){
    new_signal[, (unique_segments_groups) := NULL]
  }

  data.table::setkey(new_signal,.id,.sample_id)
  data.table::setcolorder(new_signal,c(".id",".sample_id"))
  .eeg_lst$signal <- new_signal

  
  if (nrow(new_signal) != 0) {
    last_id <- max(new_signal$.id)
  } else {
    last_id <- integer(0)
  }

  .eeg_lst$segments <- .eeg_lst$segments %>% 
                        dplyr::group_by_at(vars(segments_groups)) %>%
                        dplyr::summarize() %>%
    dplyr::ungroup() %>%
    {
      if (!".id" %in% dplyr::tbl_vars(.)) {
        hd_add_column(., .id = seq_len(last_id) %>% as.integer())
      } else {
        .
      }
    } %>%

    dplyr::select(.id, dplyr::everything())

  # TODO maybe I can do some type of summary of the events table, instead
  .eeg_lst$events <- .eeg_lst$events %>% filter(FALSE)

  update_events_channels(.eeg_lst) %>% validate_eeg_lst()
}

#' @noRd
group_by_eeg_lst <- function(.eeg_lst, .dots, .add = FALSE){

  # .dots <- rlang::quos(segment,.id,.sample_id)
  # .dots <- rlang::quos(.sample_id)
  # divide dots according to if they belong to $signal or segments
  new_dots <- dots_by_df(.dots, .eeg_lst)
  signal_groups <- purrr::map_chr(new_dots$signal, rlang::quo_text)
  segments_groups <- purrr::map_chr(new_dots$segments, rlang::quo_text)
 
  if(any(!segments_groups %in% colnames(.eeg_lst$segments))){
    stop("Incorrect grouping.",call. = FALSE)
  }
  
  attributes(.eeg_lst)$vars <- list(
                          signal = signal_groups,
                          segments = segments_groups)
  .eeg_lst
}

#' @noRd
filter_eeg_lst <- function(.eeg_lst, .dots){  

  # .dots <- rlang::quos(recording == "0")
    new_dots <- dots_by_df(.dots, .eeg_lst)
  
    # filter the signal_tbl and update the segments, in case an entire id drops
    if (length(new_dots$signal) > 0) {
      .eeg_lst$signal <- do_based_on_grps(.eeg_lst$signal,
        ext_grouping_df = .eeg_lst$segments,
        dplyr_fun = dplyr::filter, new_dots$signal
      )
  
      .eeg_lst$segments <- dplyr::semi_join(.eeg_lst$segments, .eeg_lst$signal, by = ".id")
      .eeg_lst$events <- dplyr::semi_join(.eeg_lst$events, .eeg_lst$segments, by = ".id")
    }
    # filter the segments and update the signal_tbl
    if (length(new_dots$segments) > 0) {
      .eeg_lst$segments <- dplyr::filter(.eeg_lst$segments, !!!new_dots$segments)
      .eeg_lst$signal <- dplyr::semi_join(.eeg_lst$signal, .eeg_lst$segments, by = ".id")
      .eeg_lst$events <- dplyr::semi_join(.eeg_lst$events, .eeg_lst$segments, by = ".id")
    }
  
    # Fix the indices in case some of them drop out
    redo_indices(.eeg_lst) %>% update_events_channels() %>% validate_eeg_lst()
  }  


#' @noRd
select_rename <- function(.eeg_lst, select = TRUE, ...) {
  if (select) {
    vars_fun <- tidyselect::vars_select
    # dplyr_fun <- dplyr::select
  } else {
    vars_fun <- tidyselect::vars_rename
    # dplyr_fun <- dplyr::rename
  }
  dots <- rlang::enquos(...)
  # dots <- rlang::quos(xx =Fp1, yy = Cz)
  # dots <- rlang::quos(O1, O2, P7, P8)
  # dots <- rlang::quos(-Fp1)
  all_vars <- vars_fun(unique(c(
    names(.eeg_lst$signal),
    names(.eeg_lst$segments)
  )), !!!dots)

  #TODO in a more elegant way:
  select_in_df <- c("signal", "segments")
  if (length(intersect(all_vars, names(.eeg_lst$segments))) == 0) {
    select_in_df <- select_in_df[select_in_df != "segments"]
  }
  if (length(intersect(all_vars, names(.eeg_lst$signal))) == 0) {
    select_in_df <- select_in_df[select_in_df != "signal"]
  }

  # Divide the variables into the relevant columns
  for (dfs in select_in_df) {
    vars_dfs <- all_vars[all_vars %in% colnames(.eeg_lst[[dfs]])]
    # by adding these groups, select won't remove the obligatory columns
    vars_dfs <- c(obligatory_cols[[dfs]], vars_dfs)

    if (length(vars_dfs) > 0) {
      .eeg_lst[[dfs]] <- .eeg_lst[[dfs]] %>%
        dplyr::select(vars_dfs) 
    }
  }
  
  data.table::setkey(.eeg_lst$signal, .id, .sample_id)
  update_events_channels(.eeg_lst) %>% validate_eeg_lst()
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
dots_by_df <- function(dots, .eeg_lst) {
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


# this function basically applies a dplyr function (dplyr_fun) to $signal based on groups of segments (ext_grouping_df)
#' @noRd
do_based_on_grps <- function(.df, ext_grouping_df, dplyr_fun, dots) {
  int_groups <- dplyr::groups(.df)
  ext_group_names <- dplyr::group_vars(ext_grouping_df)
  sampling_rate <- attributes(.df$.sample_id)$sampling_rate

  id <- .df$.id
  # # list of groups from segments to create on the fly
  # new_groups <- purrr::map(
  #   ext_group_names,
  #   ~rlang::expr(ext_grouping_df[id, ][[!!.x]])
  # ) %>%
  #   purrr::set_names(ext_group_names)

  # # I need to ungroup first because if not, the other groups need ot be the size of the grouping that were already made and not the size of the entire signal_tbl df
  # .df <- dplyr::ungroup(.df) %>%
  #   # TODO: check the following
  #   # maybe doing a left_join and then group would be not slower
  #   dplyr::group_by(!!!new_groups, !!!int_groups) %>%
  #   dplyr_fun(!!!dots) %>% # after summarizing I add the .id
  #   dplyr::ungroup(.df) %>%
  #   dplyr::select(-dplyr::one_of(ext_group_names))

  # group by elements of segments
  use_seg_groups <- length(ext_group_names) != 0 & any(ext_group_names != ".id")
  if (use_seg_groups) {
    .df <- ext_grouping_df %>%
      tidyr::unite("grouped", ext_group_names) %>%
      dplyr::select(.id, grouped) %>%
      dplyr::left_join(.df, ., by = ".id") %>%
      dplyr::group_by(grouped, !!!int_groups)
  }

  .df <- .df %>%
    dplyr_fun(!!!dots) %>% # after summarizing I add the .id
    dplyr::ungroup(.df) %>%
    {
      if (use_seg_groups) {
        dplyr::select(., -grouped)
      } else {
        .
      }
    }

  # in case obligatory cols are gone was removed :
  if (nrow(.df) > 0) {
    if (!".id" %in% dplyr::tbl_vars(.df)) {
      .df <- dplyr::mutate(.df, .id = NA_integer_)
    }
    if (!".sample_id" %in% dplyr::tbl_vars(.df)) {
      # This just creates a channel, because of the reclass
      # .df <- dplyr::mutate(.df, .sample_id = new_sample_int(NA_integer_,
      #   sampling_rate = sampling_rate) )
      .df$.sample_id <- new_sample_int(rep(NA_integer_, nrow(.df)), sampling_rate = sampling_rate)
    }
  }
  if (nrow(.df) == 0) {
    if (!".id" %in% dplyr::tbl_vars(.df)) {
      .df <- dplyr::mutate(.df, .id = integer(0))
    }
    if (!".sample_id" %in% dplyr::tbl_vars(.df)) {
      # .df <- dplyr::mutate(.df, .sample_id = sample_int(integer(0),
      #   sampling_rate = sampling_rate) )
      .df$.sample_id <- new_sample_int(integer(0), sampling_rate = sampling_rate)
    }
  }

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
    df$.id <- as.factor(df$.id) %>% as.integer(.)
    df
    # orig_groups <- dplyr::groups(df)
    # df <- df %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(.id = dplyr::group_indices(., .id) %>% as.integer()) %>%
    #   dplyr::group_by(!!!orig_groups)
  }

  .eeg_lst$signal <- redo_indices_df(.eeg_lst$signal)
  .eeg_lst$segments <- redo_indices_df(.eeg_lst$segments)
  .eeg_lst$events <- redo_indices_df(.eeg_lst$events)
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
  if (all(unique(segments$.id) != seq_len(max(segments$.id)))) {
    warning("Missing .ids, some functions might fail.",
      call. = FALSE
    )
  }
}


#' @noRd
group_vars_int <- function(eeg_lst) {
  list(signal_tbl = group_vars(eeg_lst$signal), segments = group_vars(eeg_lst$segments))
}

#' @noRd
groups_int <- function(eeg_lst) {
  list(signal_tbl = groups(eeg_lst$signal), segments = groups(eeg_lst$segments))
}


#' @noRd
group_by_id <- function(eeg_lst) {
  orig_groups <- dplyr::group_vars(eeg_lst)
  # if there are many groupings
  if (length(orig_groups) == 0 | length(orig_groups) > 1 |
    # or if the only one is not .id
    (length(orig_groups) == 1 & orig_groups[1] != ".id")) {
    message("# Grouping by .id.")
  }

  dplyr::group_by(eeg_lst, .id)
}

#' @noRd
signal_from_parent_frame <- function(env = parent.frame()) {
  # This is the environment where I can find the columns of signal_tbl
  signal_env <- rlang::env_get(env = env, ".top_env", inherit = TRUE)
  signal_tbl <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))
}
