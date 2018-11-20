  .datatable.aware = TRUE
  
#' @noRd
seq_len <- function(length.out) {
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
    capture.output(print(object.size(eeg_lst), units = "auto"))
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


#' @noRd
as_integer <- function(x) {
  largest <- 2000000000
  x[x > largest] <- largest
  x[x < -largest] <- -largest
  as.integer(x)
}


#' @noRd
vec_mean <- function(..., na.rm = FALSE) {
  purrr::pmap_dbl(list(...), ~mean(c(...), na.rm = FALSE))
}

#' @noRd
# https://github.com/mllg/batchtools/blob/master/R/Joins.R
semi_join_dt <- function(x, y, by = NULL) {
  w <- unique(x[y, on = by, nomatch = 0L, which = TRUE, allow.cartesian = TRUE])
  x[w]
}


rowMeans_ch <- function(x, na.rm = FALSE, dims = 1L) {
  channel_dbl(rowMeans(x, na.rm, dims))
}

row_fun_ch <- function(x, .funs, ...) {
 # TODO: faster options seem to be melting first (memory usage?):
  #https://stackoverflow.com/questions/7885147/efficient-row-wise-operations-on-a-data-table
# or with for loop set:
  # https://stackoverflow.com/questions/37667335/row-operations-in-data-table-using-by-i?noredirect=1&lq=1
  funs <- dplyr:::as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...)
  fun_txt <- rlang::quo_text(funs[[1]])
  # channel_dbl(purrr::pmap(x, ~ eval(parse(text= fun_txt))))
  channel_dbl(apply(x, 1, function(.) eval(parse(text= fun_txt))))
}



theme_eeguana <- ggplot2::theme_bw() + 
                ggplot2::theme(
                    strip.background = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
                    panel.spacing  = ggplot2::unit(.01, "points"),
                    panel.border = ggplot2::element_rect(colour = "transparent", fill = "transparent"))
