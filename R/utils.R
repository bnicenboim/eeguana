
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

rowMeans_ch <- function(x, na.rm = FALSE, dims = 1L) {
  channel_dbl(rowMeans(x, na.rm, dims))
}

row_fun_ch <- function(x, .funs, ...) {
 # TODO: faster options seem to be melting first (memory usage?):
  #https://stackoverflow.com/questions/7885147/efficient-row-wise-operations-on-a-data-table
# or with for loop set:
  # https://stackoverflow.com/questions/37667335/row-operations-in-data-table-using-by-i?noredirect=1&lq=1
  funs <- as_fun_list(.funs, rlang::enquo(.funs), rlang::caller_env(),...)
  fun_txt <- rlang::quo_text(funs[[1]])
  # channel_dbl(purrr::pmap(x, ~ eval(parse(text= fun_txt))))
  channel_dbl(apply(x, 1, function(.) eval(parse(text= fun_txt))))
}



theme_eeguana <- ggplot2::theme_bw() + 
                ggplot2::theme(
                    strip.background = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
                    panel.spacing  = ggplot2::unit(.01, "points"),
                    panel.border = ggplot2::element_rect(colour = "transparent", fill = "transparent"))
theme_eeguana_empty <- theme_eeguana +
                    theme(panel.grid = ggplot2::element_line(colour = "transparent"),
                      axis.ticks= ggplot2::element_line(colour = "transparent"),
                      axis.text= ggplot2::element_blank(),
                      axis.title= ggplot2::element_blank())
                    
#' Convenience function for range subsets 
#' 
#' between is a thin wrapper for the between function of [data.table]. It is equivalent to x >= lower & x <= upper when incbounds=TRUE, or x > lower & y < upper when FALSE.
#' 
#' @inheritParams  data.table::between
#' @export
between <- data.table::between


#' @noRd
repeated_group_col <- function(.eeg_lst){
    group_cols <- group_vars(.eeg_lst)
    segments <-   .eeg_lst$segments %>%
        {.[names(.) %in%  c(obligatory_cols$segments, group_cols)]} %>%
        data.table::data.table()
    data.table::setkey(segments,.id)
    dt <- .eeg_lst$signal[segments, group_cols, with = FALSE]  
    if(nrow(dt)==0){
      return(dt)
    } else {
        return(dt[, .group:=do.call(paste0,.SD)][,(group_cols):=NULL][])
    }
}

pink_noise <- function (N, alpha = 1) {
    ## adapted from tuneR, needs to be checked
    f <- seq(from = 0, to = pi, length.out = (N/2 + 1))[-c(1, 
    (N/2 + 1))]
    f_ <- 1/f^alpha
    RW <- sqrt(0.5 * f_) * rnorm(N/2 - 1)
    IW <- sqrt(0.5 * f_) * rnorm(N/2 - 1)
    fR <- complex(real = c(rnorm(1), RW, rnorm(1), RW[(N/2 - 
                                                       1):1]), imaginary = c(0, IW, 0, -IW[(N/2 - 1):1]), length.out = N)
    reihe <- fft(fR, inverse = TRUE)
    return(Re(reihe))
}
