#' @noRd
group_by_eeg_lst <- function(.eeg_lst, .dots, .add = FALSE){
  attributes(.eeg_lst)$vars <- purrr::map_chr(.dots, rlang::quo_text)
  allcols <- c(colnames(.eeg_lst$signal), colnames(.eeg_lst$segments))
  if(length(setdiff(attributes(.eeg_lst)$vars, allcols))>0) {
    stop("Incorrect grouping.",call. = FALSE)
  }
  .eeg_lst
}

# summarize_eeg_lst <- function(.eeg_lst, dots){
#    # # if there is something conditional on segments (O[condition == "faces"],
#    # # I should add them to the signal_tbl df temporarily
#                                       # cond_cols = cond_cols)

#   cond_cols <- names_segments_col(.eeg_lst, dots)
#   segment_groups <- intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$segments))
#    summarize_eval_eeg_lst(.eeg_lst, eval = summarize_eval(dots), cond_cols, segment_groups)
# }

filter_eeg_lst <- function(.eeg_lst, dots){  

    new_dots <- dots_by_tbl_quos(.eeg_lst, dots)

    if (length(new_dots$signal) > 0) {
      .eeg_lst$signal <- eval_signal(.eeg_lst, eval_txt = filter_eval(new_dots$signal), cond_cols = names_segments_col(.eeg_lst, dots))
      .eeg_lst$segments <- dplyr::semi_join(.eeg_lst$segments, .eeg_lst$signal, by = ".id")
     }
    
    # filter the segments and update the signal_tbl
    if (length(new_dots$segments) > 0) {
      .eeg_lst$segments <- dplyr::filter(.eeg_lst$segments, !!!new_dots$segments)
      .eeg_lst$signal <- semi_join_dt(.eeg_lst$signal, .eeg_lst$segments, by = ".id")
    }
      .eeg_lst$events <- semi_join_dt(.eeg_lst$events, .eeg_lst$segments, by = ".id")
  
    # Fix the indices in case some of them drop out
    .eeg_lst <- redo_indices(.eeg_lst) %>% update_events_channels() 
    data.table::setkey(.eeg_lst$signal,.id,.sample_id)
    .eeg_lst %>% validate_eeg_lst()
  }  



#' @noRd
mutate_eeg_lst <- function(.eeg_lst, dots, keep_cols = TRUE){  

  # .dots <- rlang::quos(recording == "0")
    new_dots <- dots_by_tbl_quos(.eeg_lst, dots)

    if (length(new_dots$signal) > 0) {
      channels_info <- channels_tbl(.eeg_lst)

      new_cols <-  rlang::quos_auto_name(new_dots$signal) %>%
                    names()

      signal_cols <- {if(keep_cols) {
                              colnames(.eeg_lst$signal)
                             } else {
                              obligatory_cols$signal
                             }}  %>% c(.,new_cols) %>%
                       unique()

      .eeg_lst$signal <- eval_signal(.eeg_lst, eval_txt = mutate_eval(new_dots$signal), cond_cols = names_segments_col(.eeg_lst, dots),out_cols = signal_cols) 

      #updates the events and the channels
      .eeg_lst <- .eeg_lst %>% update_events_channels()  %>% update_channels_tbl(channels_info)
      data.table::setkey(.eeg_lst$signal,.id,.sample_id)
     }
    
    # If relevant mutates segments as well
    if (length(new_dots$segments) > 0) {
    missing_vars <- dplyr::setdiff(
      obligatory_cols$segments,
      dplyr::group_vars(.eeg_lst$segments)
    ) %>%
      rlang::syms(.)
    
     if(keep_cols) {
      dplyr_fun <- dplyr::mutate
     } else {
      dplyr_fun <- dplyr::transmute
     }
    .eeg_lst$segments <- dplyr_fun(
      .eeg_lst$segments, !!!missing_vars,
      !!!new_dots$segments
    )
    }

    .eeg_lst %>% validate_eeg_lst()  
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


# #' @noRd
# group_vars_int <- function(eeg_lst) {
#   list(signal_tbl = group_vars(eeg_lst$signal), segments = group_vars(eeg_lst$segments))
# }

# #' @noRd
# groups_int <- function(eeg_lst) {
#   list(signal_tbl = groups(eeg_lst$signal), segments = groups(eeg_lst$segments))
# }


# #' @noRd
# group_by_id <- function(eeg_lst) {
#   orig_groups <- dplyr::group_vars(eeg_lst)
#   # if there are many groupings
#   if (length(orig_groups) == 0 | length(orig_groups) > 1 |
#     # or if the only one is not .id
#     (length(orig_groups) == 1 & orig_groups[1] != ".id")) {
#     message("# Grouping by .id.")
#   }

#   dplyr::group_by(eeg_lst, .id)
# }

#' @noRd
signal_from_parent_frame <- function(env = parent.frame()) {
  # This is the environment where I can find the columns of signal_tbl
  signal_env <- rlang::env_get(env = env, ".top_env", inherit = TRUE)
  signal_tbl <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))
}


update_channels_tbl <- function(.eeg_lst, channels_info){
  new_channels_names <- dplyr::tibble(.name = setdiff(channel_names(.eeg_lst), channels_info$.name), class = "channel_dbl")
  old_channels_tbl <- dplyr::filter(channels_info, .name %in% channel_names(.eeg_lst))
  new_channels_tbl <- dplyr::bind_rows(new_channels_names, old_channels_tbl) %>% 
                    left_join(dplyr::tibble(.name=channel_names(.eeg_lst)),.,by=".name")

   channels_tbl(.eeg_lst) <- new_channels_tbl
  .eeg_lst
}

level_to_grouping <- function(level){
  if(is.character(level)){
    final_groups <- levels
    grouping <- {!dplyr::group_vars(.eeg_lst) %in% level} %>% purrr::map_chr(as.integer) %>% 
                                                paste0(collapse = "") %>%
                                                strtoi( base = 2)
  } else {
    stop("Invalid level")
  }
  # grouping <-    case_when(level %in% c("last","ultimate") ~ "max(grouping)",
  #             level %in% c("penultimate") ~ "max(grouping[grouping!=max(grouping)])",
  #             level %in% c("first") ~ 0,
  #             is.numeric(level) ~ as.character(round(level,0)),
  #             TRUE ~ stop("Incorrect level for the rollup"))
 grouping    
}
