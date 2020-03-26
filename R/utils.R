

# mu_raw <- list(charToRaw("μ"), charToRaw("µ"))


#' := operator
#'
#'
#' @name :=
#' @rdname set
#' @noRd
#' @keywords internal
#' @importFrom data.table :=
NULL

#' Unique columns of signal and segments tables.
#' @noRd
col_names_main <- function(.eeg_lst) {
  unique(c(colnames(.eeg_lst$.signal), colnames(.eeg_lst$.segments)))
}


#' @noRd
seq_len2 <- function(length.out) {
  if (length(length.out) == 0) {
    base::seq_len(0)
  } else if (length.out == -Inf) {
    warning("length.out is -Inf, using 0 instead.")
    base::seq_len(0)
  } else {
    base::seq_len(length.out)
  }
}



#' @noRd
say_size <- function(eeg_lst) paste(
    "# Object size in memory",
    utils::capture.output(print(utils::object.size(eeg_lst), units = "auto"))
  )

#' Get integers so that their prod is approx N
#' @noRd
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

## taken from dplyr
#' @noRd
cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

#' @noRd
make_names <- function(names) {
  make.names(names) %>% make.unique()
}

#' @noRd
as_integer <- function(x) {
  largest <- 2000000000
  x[x > largest] <- largest
  x[x < -largest] <- -largest
  as.integer(x)
}


#' @noRd
vec_mean <- function(..., na.rm = FALSE) {
  purrr::pmap_dbl(list(...), ~ mean(c(...), na.rm = FALSE))
}

#' @noRd
rowMeans_ch <- function(x, na.rm = FALSE, dims = 1L) {
  channel_dbl(rowMeans(x, na.rm, dims))
}

#' @noRd
row_fun_ch <- function(x, .f, pars = list()) {
  # TODO: faster options seem to be melting first (memory usage?):
  # https://stackoverflow.com/questions/7885147/efficient-row-wise-operations-on-a-data-table
  # or with for loop set:
  # https://stackoverflow.com/questions/37667335/row-operations-in-data-table-using-by-i?noredirect=1&lq=1


  # funs <- as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...)
  # fun_txt <- rlang::quo_text(funs[[1]])
  # channel_dbl(purrr::pmap(x, ~ eval(parse(text= fun_txt))))
  # .f <- purrr::possibly( match.fun, NULL )(.f)
  # if( is.null(.f) ){
  # .f <- purrr::as_mapper(.f)
  # y <- apply(x, 1, .f)
  # } else {
  y <- do.call(apply, c(list(x, 1, match.fun(.f)), pars))
  # }
  channel_dbl(y)
}

#' @noRd
repeated_group_col <- function(.eeg_lst) {
  group_cols <- dplyr::group_vars(.eeg_lst)
  segments <- .eeg_lst$.segments %>%
    {
      .[names(.) %in% c(obligatory_cols$.segments, group_cols)]
    } %>%
    data.table::data.table()
  data.table::setkey(segments, .id)
  dt <- .eeg_lst$.signal[segments, group_cols, with = FALSE]
  if (nrow(dt) == 0) {
    return(dt)
  } else {
    return(dt[, .group := do.call(paste0, .SD)][, (group_cols) := NULL][])
  }
}

#' @noRd
try_to_downsample <- function(.data, max_sample) {
    if (!anyNA(nsamples(.data)) && (is.numeric(max_sample) && max_sample != 0 &&
                                  # it will downsample if the samples are at least twice as large than the max_sample
                                  sum(nsamples(.data)) / 2 > max_sample)) {
    q <- round(sum(nsamples(.data)) / max_sample)
    .data <- eeg_downsample(.data, 
                            q=q)
  } else {
    .data
  }
}

#' @noRd
map_matr <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::map(.x, .f, ...)
  do.call("rbind", res)
}

#' Cat a message and then a printable object
#' @noRd
message_obj <- function(msg, obj) {
  outp <- paste(utils::capture.output({
    print(obj)
  }), collapse = "\n")
  paste0(msg, "\n", outp, "\n")
}
#' @noRd
is_wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol | is.infinite(x) | is.na(x)

}
#' @noRd
require_pkg <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package '",pkg,"'  needed for this function to work. Please install it."),
         call. = FALSE)
  }
}

#' @noRd
`%||%` <- function (x, y) {
  if (is.null(x) || length(x)==0)
  y
  else x
}

