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
    capture.output(pryr::object_size(eeg_lst))
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
  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)

  w <- unique(x[y, on = by, nomatch = 0L, which = TRUE, allow.cartesian = TRUE])
  x[w]
}
