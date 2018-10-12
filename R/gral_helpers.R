#' @noRd
redo_indices <- function(.eegbl) {
  redo_indices_df <- function(df) {
    orig_groups <- dplyr::groups(df)
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(.id = dplyr::group_indices(., .id) %>% as.integer()) %>%
      dplyr::group_by(!!!orig_groups)
  }

  .eegbl$signal <- redo_indices_df(.eegbl$signal)
  .eegbl$segments <- redo_indices_df(.eegbl$segments)
  .eegbl
}




names_segments_col <- function(.eegbl, dots) {
  segments_cols <- setdiff(colnames(.eegbl$segments), ".id") # removes .id

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

# add a column to an empty table
# https://community.rstudio.com/t/cannot-add-column-to-empty-tibble/1903/11
hd_add_column <- function(.data, ..., .before = NULL, .after = NULL) {
  if (nrow(.data) == 0L) {
    return(tibble::tibble(...))
  }
  return(tibble::add_column(.data, ..., .before = .before, .after = .after))
}


decimals <- function(x) match(TRUE, round(x, 1:20) == x)
say_size <- function(eegble) paste(
    "# Object size in memory",
    capture.output(pryr::object_size(eegble))
  )

# Smallest divisor of x, starting by 2
mindiv <- function(x, start = 2) {
  div <- start
  while (round(x) %% div != 0) {
    div <- div + 1
  }
  div
}


# Get integers so that their prod is approx N
factors <- function(N) {
  out <- c()
  while (N != 1) {
    for (i in 10:1) {
      if (i == 1) {
        N <- N - 1
      } else if (N %% i == 0) {
        out <- c(out, i)
        N <- N / i
        break
      }
    }
  }
  out
}




obligatory_cols <- list(
  signal = c(".id", ".sample_id"),
  events = c(".id", ".sample_0", ".size", ".channel"),
  segments = c(".id")
)

update_chans <- function(x) {
  current_chans <- colnames(x$signal)[!colnames(x$signal) %in% c(".id", "sample")]
  added_chans <- current_chans[!current_chans %in% x$channels$labels]
  # remove old channels
  x$channels <- dplyr::filter(x$channels, labels %in% current_chans)
  x$events <- dplyr::filter(x$events, channel %in% current_chans | is.na(channel))

  # add new ones
  x$channels <- x$channels %>%
    dplyr::mutate(labels = as.character(labels)) %>%
    dplyr::bind_rows(tibble::tibble(labels = added_chans)) %>%
    dplyr::semi_join(tibble::tibble(labels = current_chans), by = "labels") %>%
    dplyr::right_join(dplyr::tibble(labels = current_chans), by = "labels") %>%
    dplyr::mutate(labels = forcats::as_factor(labels))

  x$events <- x$events %>% mutate(channel = forcats::fct_drop(channel) %>%
    forcats::lvls_expand(new_levels = current_chans))
  x
}

validate_dots <- function(...) {
  dots <- rlang::enquos(...)
  if (any(names(dots) %in% c(".id", "sample"))) {
    stop(".id and samples can't be manipulated")
  } else {
    dots
  }
}


as_integer <- function(x) {
  largest <- 2000000000
  x[x > largest] <- largest
  x[x < -largest] <- -largest
  as.integer(x)
}


vec_mean <- function(..., na.rm = FALSE) {
  purrr::pmap_dbl(list(...), ~mean(c(...), na.rm = FALSE))
}

scaling <- function(x, unit) {
  if (stringr::str_to_lower(unit) %in% c("s", "sec", "second", "seconds", "secs")) {
    scaling <- sampling_rate(x)
  } else if (stringr::str_to_lower(unit) %in% c(
    "ms", "msec", "millisecond",
    "milli second", "milli-second",
    "milliseconds", "milli seconds",
    "msecs"
  )) {
    scaling <- sampling_rate(x) / 1000
  } else if (stringr::str_to_lower(unit) %in% c("sam", "sample", "samples")) {
    scaling <- 1
  } else {
    stop("Incorrect unit. Please use 'ms', 's', or 'sample'")
  }
}
