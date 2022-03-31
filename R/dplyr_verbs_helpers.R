#' @noRd
summarize_ext <- function(.data, dots, .groups) {
  cond_cols <- names_other_col(.data, dots, ".segments")
  extended_signal_dt <- extended_signal(.data, cond_cols)
  by <- eeg_group_vars(.data)
  dots_main <- prep_dots(
    dots = dots,
    data = extended_signal_dt,
    .by = !!by,
    j = TRUE
  )
  summarize.(extended_signal_dt, !!!dots_main, .by = by)
}

rebuild_segment_dt <- function(.data) {
  # rebuild the segment table
  if (length(group_vars_only_segments(.data)) > 0) {
    segment_dt <- distinct.(.data[[1]], ".id", group_vars_only_segments(.data))
    if (!".recording" %in% eeg_group_vars(.data)) {
      segment_dt <- mutate.(segment_dt, .recording = NA_character_)
    }
    data.table::setcolorder(segment_dt, obligatory_cols$.segments)
  } else {
    ## Restructure segments table to fit the new signal table
    if (nrow(.data[[1]]) != 0) {
      last_id <- max(.data[[1]]$.id)
    } else {
      last_id <- integer(0)
    }
    segment_dt <- data.table::data.table(.id = seq_len(last_id), .recording = NA_character_)
    data.table::setkey(segment_dt, .id)
  }
  segment_dt
}



#' @noRd
group_by_lst <- function(.data, dots, .add = FALSE) {
  if (length(dots) != 0) {
    new_groups <- purrr::map_chr(dots, rlang::quo_text)
  } else {
    new_groups <- character(0)
  }
  names(new_groups) <- NULL
  if (.add == FALSE) {
    attributes(.data)$vars <- new_groups
  } else {
    attributes(.data)$vars <- unique(c(attributes(.data)$vars, new_groups))
  }
  allcols <- col_names_main(.data)
  if (length(setdiff(attributes(.data)$vars, allcols)) > 0) {
    notfound <- paste0(setdiff(attributes(.data)$vars, allcols), collapse = ", ")
    stop(sprintf("Incorrect grouping. The groups %s were not found", notfound), call. = FALSE)
  }
  .data
}

#' @noRd
filter_lst <- function(.data, ...) {
  dots <- rlang::quos(...)
  new_dots <- dots_by_tbl_quos(.data, dots)
  # first one is .signal or .psd
  if (length(new_dots[[1]]) > 0) {
    cond_cols <- names_other_col(.data, dots, tbl = ".segments")
    ## events_col <- names_other_col(.data, dots,".events")
    extended_signal_dt <- extended_signal(.data, cond_cols) # , events_col = events_col)
    by <- as.character(eeg_group_vars(.data))
    cols_main <- colnames(.data[[1]])
    dots_main <- prep_dots(dots = new_dots[[1]], data = extended_signal_dt, .by = !!by, j = TRUE)
    .data[[1]] <- filter.(extended_signal_dt, !!!dots_main, .by = by) %>%
      .[, ..cols_main]

    if (!is.null(.data$.events) && nrow(.data$.events) > 0) {
      range_s <- .data$.signal[, .(.lower = min(.sample), .upper = max(.sample)), by = .id]
      .data$.events <- update_events(.data$.events, range_s)
    }
    .data$.segments <- semi_join.(.data$.segments, .data[[1]], by = ".id")
  }
  # filter the segments and update the signal_tbl/psd
  if (length(new_dots$.segments) > 0) {
    grouping <- eeg_group_vars(.data)[eeg_group_vars(.data) %in% colnames(.data$.segments)]
    dots_segments <- prep_dots(dots = new_dots$.segments, data = extended_signal_dt, .by = !!by, j = TRUE)
    .data$.segments <- filter.(.data$.segments, !!!dots_segments, .by = grouping)
    .data[[1]] <- semi_join.(.data[[1]], .data$.segments, by = ".id")
  }

  if (!is.null(.data$.events)) {
    .data$.events <- semi_join.(.data$.events, data.table::as.data.table(.data$.segments), by = ".id")
    .data <- .data %>% update_events_channels()
  }
  # Fix the indices in case some of them drop out
  data.table::setkey(.data$.segments, .id)

  .data
}


#' @noRd
mutate_lst <- function(.data, ..., keep_cols = TRUE, .by_reference = FALSE) {
  # What to do by table in the object:
  dots <- rlang::quos(...)
  new_dots <- dots_by_tbl_quos(.data, dots)
  obligatory_cols_main <- obligatory_cols[[names(.data)[1]]]
  non_ch <- NULL # for msg at the end
  if (length(new_dots[[1]]) > 0) {
    # is it mutate or transmute?
    cols_main <- colnames(.data[[1]])
    if (keep_cols) {
      keep <- "all"
    } else {
      keep <- "none"
    }
    # names of columns that are used to conditionalize channels: F1[.recording=="1"]
    cond_cols <- names_other_col(.data, dots, ".segments")

    # extended signal dt with the group_by col, and the conditional columns
    # TODO: group_by columns could be pasted together and converted to factor
    extended_main_dt <- extended_signal(.data, cond_cols = cond_cols)
    tmp_col <- setdiff(colnames(extended_main_dt), colnames(.data[[1]]))
    by <- eeg_group_vars(.data)
    dots_main <- prep_dots(dots = new_dots[[1]], data = extended_main_dt, .by = !!by, j = TRUE)

    # added cols
    aux_cols <- setdiff(colnames(extended_main_dt), cols_main)

    extended_main_dt <- mutate.(extended_main_dt,
      !!!dots_main,
      !!!(rlang::parse_exprs(obligatory_cols_main)),
      .by = by,
      .keep = keep
    )
    # removes the extended by columns
    aux_cols <- intersect(aux_cols, colnames(extended_main_dt))
    if (!length(aux_cols) == 0) {
        extended_main_dt <- extended_main_dt[, c(aux_cols) := NULL][]
    }
    ## Check that the user did not mess up the attributes of the column:
  
    non_obl <- extended_main_dt[0, -obligatory_cols_main, with = FALSE]
    # Remove below:
    non_ch <- names(non_obl)[!purrr::map_lgl(non_obl, is_channel_dbl)]
    non_comp <- names(non_ch)[!purrr::map_lgl(non_ch, is_component_dbl)]
    non_ch <- unique(c(non_ch, non_comp))
    if (length(non_ch) > 0 & options()$eeguana.verbose) {
      message_verbose(
        "The following columns of signal_tbl are not channels (or ICA components): ", paste(non_ch, sep = ", "), "\n",
        "* To build a channel use `channel_dbl()` function, e.g. channel_dbl(0) to populate the table with a channel containing 0 microvolts.\n",
        "* To copy the structure of an existing channel one can do `new_ch = existing_channel * 0 + ...`"
      )
    }

    .data[[1]] <- extended_main_dt
  }
  # If relevant mutates segments as well
  if (length(new_dots$.segments) > 0) {
    by <- intersect(eeg_group_vars(.data), colnames(.data$.segments))

    dots_segments <- prep_dots(new_dots$.segments, .data$.segments, !!by, j = TRUE)
    .data$.segments <- mutate.(
      .data$.segments, !!!dots_segments,
      .by = by
    )

    if (!keep_cols) {
      # transmute
      # use intersect in case some columns are gone
      cols_to_keep <- intersect(
        c(
          obligatory_cols[[".segments"]],
          names(new_dots$.segments)
        ),
        colnames(.data$.segments)
      )
      if (!.by_reference) {
        .data$.segments <- .data$.segments[, ..cols_to_keep]
      } else {
        remove_cols <- setdiff(colnames(.data$.segments), cols_to_keep)
        .data$.segments[, c(remove_cols) := NULL][]
      }
    }
  }
  .data
}



#' @noRd
select_rename <- function(.data, select = TRUE, ...) {
  if (select) {
    vars_fun <- tidyselect::vars_select
  } else {
    vars_fun <- tidyselect::vars_rename
  }
  dots <- rlang::enquos(...)

  all_vars <- vars_fun(unique(c(
    names(.data[[1]]), # .signal or .psd
    names(.data$.segments)
  )), !!!dots)

  new_groups <- eeg_group_vars(.data) %>%
    purrr::map_if(~ .x %in% all_vars, ~ all_vars[all_vars == .x] %>% names()) %>%
    rlang::syms()

  # TODO in a more elegant way:
  select_in_df <- c(".signal", ".psd", ".segments")
  if (length(intersect(all_vars, names(.data$.segments))) == 0) {
    select_in_df <- select_in_df[select_in_df != ".segments"]
  }
  if (length(intersect(all_vars, names(.data$.signal))) == 0) {
    select_in_df <- select_in_df[select_in_df != ".signal"]
  }
  if (length(intersect(all_vars, names(.data$.psd))) == 0) {
    select_in_df <- select_in_df[select_in_df != ".psd"]
  }

  # Divide the variables into the relevant tables
  for (dfs in select_in_df) {
    vars_dfs <- all_vars[all_vars %in% colnames(.data[[dfs]])]
    # add grouped vars if missing
    groups <- eeg_group_vars(.data)[eeg_group_vars(.data) %in% colnames(.data[[dfs]])]
    missing_grouped_vars <- setdiff(groups, vars_dfs) %>%
      stats::setNames(., .)
    if (length(missing_grouped_vars) > 0) {
      message_verbose("Adding missing grouping variables: ", paste0(missing_grouped_vars, collapse = ", "))
    }
    vars_dfs <- c(missing_grouped_vars, vars_dfs)

    renamed_obligatory <- vars_dfs[names(vars_dfs) != vars_dfs] %>%
      intersect(obligatory_cols[[dfs]])
    if (length(renamed_obligatory) > 0) {
      warning("Trying to rename obligatory column(s): ", renamed_obligatory,
        call. = FALSE
      )
    }
    # by adding these, select won't remove the obligatory columns
    vars_dfs <- c(obligatory_cols[[dfs]], vars_dfs)

    if (length(vars_dfs) > 0) {
      .data[[dfs]] <- .data[[dfs]] %>%
        select.(tidyselect::all_of(vars_dfs))
    }

    if (dfs == ".signal") { # if the signal tbl was modified, the events need to be updated:

      # TODO: reimplement all the following directly in data table, or maybe with purrr
      old_channels <- events_tbl(.data)$.channel
      new_channels <- all_vars[all_vars %in% old_channels] %>% sort()
      # removes the old channels that do not exist anymore
      ## (needed for e.g., select(ZZ=X))
      ##
      rem_pos <- which(!is.na(old_channels) & !old_channels %in% unname(new_channels))
      old_channels[!old_channels %in% new_channels] <- NA
      ## old_channels <- old_channels[old_channels %in% new_channels]

      events_tbl(.data)$.channel <- old_channels %>%
        factor(labels = names(new_channels)) %>%
        as.character()
      if (length(rem_pos) > 0) {
        events_tbl(.data) <- events_tbl(.data)[-rem_pos]
      }
    }
  }
  data.table::setkey(.data$.segments, .id)
  .data %>%
    eeg_group_by(!!!new_groups)
}

#' @noRd
scaling <- function(sampling_rate, unit) {
  if (tolower(unit) %in% c("s", "sec", "second", "seconds", "secs")) {
    scaling <- sampling_rate
  } else if (tolower(unit) %in% c(
    "ms", "msec", "millisecond",
    "milli second", "milli-second",
    "milliseconds", "milli seconds",
    "msecs"
  )) {
    scaling <- sampling_rate / 1000
  } else if (tolower(unit) %in% c("sam", "sample", "samples")) {
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
  # names_o <- c()

  # col name, checking that before or after there is no part of a word \\w or .
  cols_regex <- paste0(paste0("(?<![\\w.])", cols, "(?![\\w.])"), collapse = "|")
  names_o <- lapply(dots, function(dot) {
    chr_extract_all(rlang::quo_text(dot), cols_regex)
  }) %>%
    unlist() %>%
    unique()
  # for (n in seq_len(length(dots))) {
  #   # get the AST of each call and unlist it
  #   names_o <- c(names_o, getAST(dots[[n]]) %>%
  #     unlist(.) %>%
  #     # make it a vector of strings
  #     purrr::map_chr(~ rlang::quo_text(.x)) %>%
  #     dplyr::intersect(cols))
  # }
  names_o[!is.na(names_o)]
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
extended_signal <- function(.eeg_lst, cond_cols = NULL, events_cols = NULL, .by_reference = FALSE) {
  ## For NOTES:
  ..events_cols <- NULL
  if (.by_reference) {
    extended_signal_dt <- .eeg_lst[[1]] # signal or psd
  } else {
    extended_signal_dt <- shallow(.eeg_lst[[1]])
  }
  relevant_cols <- c(".id", eeg_group_vars(.eeg_lst), cond_cols)
  if (any(relevant_cols != ".id")) { # more than just .id
    segments <- .eeg_lst$.segments[extended_signal_dt, unique(relevant_cols), with = FALSE]
    extended_signal_dt[, `:=`(names(segments), segments)]
  }
  if (length(events_cols) > 0) {
    events_dt <- events_tbl(.eeg_lst)[extended_signal_dt, c(".id", events_col), with = FALSE]
    extended_signal_dt[, `:=`(names(events_dt), events_dt)]
  }
  extended_signal_dt[]
}

#' @noRd
group_vars_segments <- function(.eeg_lst) {
  intersect(dplyr::group_vars(.eeg_lst), colnames(.eeg_lst$.segments))
}

#' @noRd
group_vars_only_segments <- function(.eeg_lst) {
  group_vars_segments(.eeg_lst) %>%
    {
      .[. != ".id"]
    }
}

#' @noRd
update_events_channels <- function(x, .by_reference = FALSE) {
  if (nrow(x$.events) > 0) {
    redundant_channels <- setdiff(unique(x$.events$.channel), c(channel_names(x), NA))
  } else {
    redundant_channels <- NULL
  }

  if (length(redundant_channels) > 0) {
    if (.by_reference) {
      x$.events[.channel %in% redundant_channels, names(x$.events) := NA]
      message_verbose("Setting to NA events with the redundant channel(s): ", paste0(redundant_channels, collapse = ", "))
    } else {
      ## x$.events <- x$.events[is.na(.channel) | .channel %in% channel_names(x), ]
      x$.events <- x$.events[!.channel %in% redundant_channels, ]
      message_verbose("Removing events with the redundant channel(s): ", paste0(redundant_channels, collapse = ", "))
    }
  }
  x
}


# https://stackoverflow.com/questions/50563895/using-rlang-find-the-data-pronoun-in-a-set-of-quosures
#' @noRd
getAST <- function(ee) {
  as.list(ee) %>% purrr::map_if(is.call, getAST)
}

#' @noRd
dots_by_tbl_quos <- function(.data, dots) {
  main_cols <- c(
    colnames(.data[[1]]), # main table: either signal or psd
    paste0("`", channel_names(.data), "`") # In case channel name is used with ` in the function call, NOT sure if needed anymore
  )

  main_dots <- purrr::imap_lgl(dots, function(dot, name) {
    if (name %in% main_cols) {
      TRUE
    } else {
      # get the AST of each call and unlist it, removing the first item which is ~
      getAST(dot)[-1] %>%
        unlist(.) %>%
        # make it a vector of strings
        purrr::map_lgl(function(element) { # check for every element if it's a channel or if it's a channel function
          if (is.numeric(element)) {
            return(FALSE)
          }
          if (is.logical(element)) {
            return(FALSE)
          }

          txt_element <- rlang::as_name(element)

          if (txt_element == "") {
            return(FALSE)
          }

          if (txt_element %in% main_cols) {
            return(TRUE)
          } else if (exists(txt_element) &&
            is.function(eval(parse(
              text =
              # protects to avoid errors with
              # "function" or *
                paste0(
                  "`",
                  txt_element, "`"
                )
            )))
          ) {
            return(chr_detect(txt_element, "channel_dbl$|^ch_|^chs_|^channel_dbl|^channel_names|^signal_tbl|_ch$"))
          } else {
            return(FALSE)
          }
        }) %>%
        any()
    }
  })

  # things might fail if there is a function named as a signal column. TODO, check for that in the validate.

  # main_dots is a vector of TRUE/FALSE indicating for each call whether it belongs to signals/psd
  # if both signal_tbl and segments columns are there, it will say that the dots should apply
  # to a signal_tbl dataframe.
  .segments <- c(dots[!main_dots], dots[main_dots][rlang::quos(.id) %in% dots[main_dots]])
  out <- list(.main = dots[main_dots], .segments = .segments)
  names(out)[1] <- names(.data)[1]
  out
}


#' @noRd
rename_sel_comp <- function(mixing, sel) {
  mixing <- mixing[.ICA %in% c("mean", sel), ]
  mixing[, .ICA := purrr::map_chr(.ICA, function(r) {
    new_name <- names(sel[sel == r])
    if (length(new_name) != 0) {
      return(new_name)
    } else {
      return(r)
    }
  })][]
}
sel_comp <- function(data, ...) {
  dots <- rlang::enquos(...)
  if (rlang::is_empty(dots)) {
    ch_sel <- component_names(data)
  } else {
    ch_sel <- tidyselect::vars_select(component_names(data), !!!dots)
  }
  ch_sel
}
