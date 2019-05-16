#' @noRd
group_by_eeg_lst <- function(.eeg_lst, dots, .add = FALSE) {
  if (length(dots) != 0) {
    new_groups <- purrr::map_chr(dots, rlang::quo_text)
  } else {
    new_groups <- character(0)
  }
  names(new_groups) <- NULL
  if (.add == FALSE) {
    attributes(.eeg_lst)$vars <- new_groups
  } else {
    attributes(.eeg_lst)$vars <- unique(c(attributes(.eeg_lst)$vars, new_groups))
  }
  allcols <- col_names_main(.eeg_lst)
  if (length(setdiff(attributes(.eeg_lst)$vars, allcols)) > 0) {
    notfound <- paste0(setdiff(attributes(.eeg_lst)$vars, allcols), collapse = ", ")
    stop(sprintf("Incorrect grouping. The groups %s were not found", notfound), call. = FALSE)
  }
  .eeg_lst
}

#' @noRd
filter_eeg_lst <- function(.eeg_lst, dots) {
  new_dots <- dots_by_tbl_quos(.eeg_lst, dots)

  if (length(new_dots$.signal) > 0) {
    cond_cols <- names_other_col(.eeg_lst, dots, ".segments")
    ## events_col <- names_other_col(.eeg_lst, dots,".events")
    extended_signal <- extended_signal(.eeg_lst, cond_cols) # , events_col = events_col)
    by <- as.character(dplyr::group_vars(.eeg_lst))

    cols_signal <- colnames(.eeg_lst$.signal)
    .eeg_lst$.signal <- filter_dt(extended_signal, !!!dots, group_by_ = by) %>%
      .[, ..cols_signal]
    if (nrow(.eeg_lst$.events) > 0) {
      range_s <- .eeg_lst$.signal[, .(.lower = min(.sample), .upper = max(.sample)), by = .id]
      .eeg_lst$.events <- update_events(.eeg_lst$.events, range_s)
    }
    .eeg_lst$.segments <- dplyr::semi_join(.eeg_lst$.segments, .eeg_lst$.signal, by = ".id")
  }
  # filter the segments and update the signal_tbl
  if (length(new_dots$.segments) > 0) {
    grouping <- dplyr::group_vars(.eeg_lst)[dplyr::group_vars(.eeg_lst) %in% colnames(.eeg_lst$.segments)]
    .eeg_lst$.segments <- .eeg_lst$.segments %>%
      dplyr::group_by_at(dplyr::vars(grouping)) %>%
      dplyr::filter(!!!new_dots$.segments) %>%
      dplyr::ungroup()
    .eeg_lst$.signal <- semi_join_dt(.eeg_lst$.signal, data.table::as.data.table(.eeg_lst$.segments), by = ".id")
  }
  .eeg_lst$.events <- semi_join_dt(.eeg_lst$.events, data.table::as.data.table(.eeg_lst$.segments), by = ".id")


  # Fix the indices in case some of them drop out
  .eeg_lst <- .eeg_lst %>% update_events_channels()
  data.table::setkey(.eeg_lst$.signal, .id, .sample)
  .eeg_lst %>% validate_eeg_lst()
}



#' @noRd
mutate_eeg_lst <- function(.eeg_lst, dots, keep_cols = TRUE) {

  # .dots <- rlang::quos(.recording == "0")
  new_dots <- dots_by_tbl_quos(.eeg_lst, dots)

  if (length(new_dots$.signal) > 0) {
    # channels_info <- channels_tbl(.eeg_lst)

    new_cols <- rlang::quos_auto_name(new_dots$.signal) %>%
      names()

    cols_signal <- {
      if (keep_cols) {
        colnames(.eeg_lst$.signal)
      } else {
        obligatory_cols$.signal
      }
    } %>%
      c(., new_cols) %>%
      unique()
    cond_cols <- names_other_col(.eeg_lst, dots, ".segments")


    extended_signal <- extended_signal(.eeg_lst, cond_cols)
    by <- dplyr::group_vars(.eeg_lst) %>% as.character()
    # eval needs cols_signal and extended signal and by


    new_dots$.signal <- rlang::quos_auto_name(new_dots$.signal)
    for (i in seq_len(length(new_dots$.signal))) {
      extended_signal[, `:=`(names(new_dots$.signal[i]), eval(parse(text = rlang::quo_text(new_dots$.signal[[i]])))), by = c(by)]
    }

    .eeg_lst$.signal <- extended_signal[, ..cols_signal][]


    # updates the events and the channels
    .eeg_lst <- .eeg_lst %>% update_events_channels()
    data.table::setkey(.eeg_lst$.signal, .id, .sample)
  }

  # If relevant mutates segments as well
  if (length(new_dots$.segments) > 0) {
    missing_vars <- dplyr::setdiff(
      obligatory_cols$.segments,
      dplyr::group_vars(.eeg_lst$.segments)
    ) %>%
      rlang::syms(.)

    if (keep_cols) {
      dplyr_fun <- dplyr::mutate
    } else {
      dplyr_fun <- dplyr::transmute
    }
    .eeg_lst$.segments <- dplyr_fun(
      .eeg_lst$.segments, !!!missing_vars,
      !!!new_dots$.segments
    )
  }

  .eeg_lst %>% validate_eeg_lst()
}



#' @noRd
select_rename <- function(.eeg_lst, select = TRUE, ...) {
  if (select) {
    vars_fun <- tidyselect::vars_select
  } else {
    vars_fun <- tidyselect::vars_rename
  }
  dots <- rlang::enquos(...)

  all_vars <- vars_fun(unique(c(
    names(.eeg_lst$.signal),
    names(.eeg_lst$.segments)
  )), !!!dots)

  new_groups <- dplyr::group_vars(.eeg_lst) %>%
    purrr::map_if(~ .x %in% all_vars, ~ all_vars[all_vars == .x] %>% names()) %>%
    rlang::syms()

  # TODO in a more elegant way:
  select_in_df <- c(".signal", ".segments")
  if (length(intersect(all_vars, names(.eeg_lst$.segments))) == 0) {
    select_in_df <- select_in_df[select_in_df != ".segments"]
  }
  if (length(intersect(all_vars, names(.eeg_lst$.signal))) == 0) {
    select_in_df <- select_in_df[select_in_df != ".signal"]
  }

  # Divide the variables into the relevant tables
  for (dfs in select_in_df) {
    vars_dfs <- all_vars[all_vars %in% colnames(.eeg_lst[[dfs]])]
    # add grouped vars if missing
    groups <- dplyr::group_vars(.eeg_lst)[dplyr::group_vars(.eeg_lst) %in% colnames(.eeg_lst[[dfs]])]
    missing_grouped_vars <- setdiff(groups, vars_dfs) %>%
        stats::setNames(., .)
    if(length(missing_grouped_vars)>0) {
        message("Adding missing grouping variables: ", paste0(missing_grouped_vars, collapse = ", "))
    }
    vars_dfs <- c(missing_grouped_vars, vars_dfs)

    # by adding these, select won't remove the obligatory columns
    vars_dfs <- c(obligatory_cols[[dfs]], vars_dfs)

    if (length(vars_dfs) > 0) {
      .eeg_lst[[dfs]] <- .eeg_lst[[dfs]] %>%
        dplyr::select(vars_dfs)
    }

    if (dfs == ".signal") { # if the signal tbl was modified, the events need to be updated:

      # TODO: reimplement all the following directly in data table, or maybe with purrr
      old_channels <- events_tbl(.eeg_lst)$.channel
      new_channels <- all_vars[all_vars %in% old_channels] %>% sort()
      # removes the old channels that do not exist anymore (needed for e.g., select(ZZ=X))
      old_channels[!old_channels %in% new_channels] <- NA
      events_tbl(.eeg_lst)$.channel <- old_channels %>%
        factor(labels = names(new_channels)) %>%
        as.character()
    }
  }

  data.table::setkey(.eeg_lst$.signal, .id, .sample)


  .eeg_lst %>%
    dplyr::group_by(!!!new_groups) %>%
    validate_eeg_lst()
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
  scaling
}


#' Gives the names of segment or events columns except for .id included in a quosure
#' @noRd
names_other_col <- function(.eeg_lst, dots, tbl = NULL) {
  if (is.null(tbl)) {
    cols <- setdiff(c(colnames(.eeg_lst[[".signal"]]), colnames(.eeg_lst[[".events"]])), ".id")
  } else {
    cols <- setdiff(colnames(.eeg_lst[[tbl]]), ".id") # removes .id
  }
  names_o <- c()
  for (n in seq_len(length(dots))) {
    # get the AST of each call and unlist it
    names_o <- c(names_o, getAST(dots[[n]]) %>%
      unlist(.) %>%
      # make it a vector of strings
      purrr::map_chr(~ rlang::quo_text(.x)) %>%
      dplyr::intersect(cols))
  }
  unique(names_o)
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
signal_from_parent_frame <- function(env = parent.frame()) {
  # This is the environment where I can find the columns of signal_tbl
  signal_env <- rlang::env_get(env = env, ".top_env", inherit = TRUE)
  signal_tbl <- dplyr::as_tibble(rlang::env_get_list(signal_env, rlang::env_names(signal_env)))
}
#' @noRd
extended_signal <- function(.eeg_lst, cond_cols = NULL, events_cols = NULL) {
  ## For NOTES:
  ..events_cols <- NULL

  extended_signal <- NULL
  relevant_cols <- c(obligatory_cols$.segments, dplyr::group_vars(.eeg_lst), cond_cols)
  if (length(relevant_cols) > 1) { # more than just .id
    segments <- dplyr::ungroup(.eeg_lst$.segments) %>%
      {
        .[names(.) %in% relevant_cols]
      } %>%
      data.table::data.table()
    data.table::setkey(segments, .id)
    extended_signal <- .eeg_lst$.signal[segments]
  }
  if (length(events_cols) > 0) {
    extended_signal <- events_tbl(.eeg_lst)[, ..events_cols][extended_signal]
  }
  if (is.null(extended_signal)) {
    extended_signal <- data.table::copy(.eeg_lst$.signal)
  }
  extended_signal
}

#' @noRd
group_vars_segments <- function(.eeg_lst) {
  intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$.segments))
}

#' @noRd
group_vars_only_segments <- function(.eeg_lst) {
  group_vars_segments(.eeg_lst) %>% {
    .[. != ".id"]
  }
}

#' @noRd
update_events_channels <- function(x) {
  if (nrow(x$.events) > 0) {
    x$.events <- x$.events[is.na(.channel) | .channel %in% channel_names(x), ]
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
  signal_cols <- c(
    colnames(.eeg_lst$.signal),
    paste0("`", channel_names(.eeg_lst), "`") # In case channel name is used with ` in the function call, NOT sure if needed anymore
  )

  signal_dots <- purrr::map_lgl(dots, function(dot)
  # get the AST of each call and unlist it
    getAST(dot) %>%
      unlist(.) %>%
      # make it a vector of strings
      purrr::map_lgl(function(element) { # check for every element if it's a channel or if it's a channel function
        txt_element <- rlang::quo_text(element)
        if (txt_element %in% signal_cols) {
          return(TRUE)
        } else if (exists(txt_element) && is.function(eval(parse(text = txt_element)))) {
          return(stringr::str_detect(txt_element, stringr::regex("^ch_|^chs_|^channel_dbl|^channel_names|^signal_tbl")))
        } else {
          return(FALSE)
        }
      }) %>%
      any())

  # things might fail if there is a function named as a signal column. TODO, check for that in the validate.

  # signal_dots is a vector of TRUE/FALSE indicating for each call whether it belongs to signals
  # if both signal_tbl and segments columns are there, it will say that the dots should apply
  # to a signal_tbl dataframe.
  .segments <- c(dots[!signal_dots], dots[signal_dots][rlang::quos(.id) %in% dots[signal_dots]])
  list(.signal = dots[signal_dots], .segments = .segments)
}


#' @noRd
rename_sel_comp <- function(mixing, sel) {
  mixing <- mixing[ .ICA %in% c("mean", sel), ]
  mixing[, .ICA := purrr::map_chr(.ICA, function(r) {
    new_name <- names(sel[sel == r])
    if (length(new_name) != 0) {
      return(new_name)
    } else {
      return(r)
    }
  })][]
}
