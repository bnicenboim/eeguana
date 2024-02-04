

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

#' purrr alterative
#' @noRd
discard <- function(x, p) x[!sapply(x, p)]
keep <- function(x, p) x[sapply(x, p)]


#' Unique columns of signal and segments tables.
#' @noRd
col_names_main <- function(.eeg_lst) {
  unique(c(colnames(.eeg_lst$.signal), colnames(.eeg_lst$.segments)))
}

#' @noRd
message_verbose <- function(...) {
  if (options()$eeguana.verbose) message(...)
}

#' @noRd
say_size <- function(eeg_lst) {
  paste(
    "# Object size in memory",
    utils::capture.output(print(utils::object.size(eeg_lst), units = "auto"))
  )
}

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


#' #' @noRd
#' vec_mean <- function(..., na.rm = FALSE) {
#'   purrr::pmap_dbl(list(...), ~ mean(c(...), na.rm = FALSE))
#' }

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

#' #' @noRd
#' repeated_group_col <- function(.eeg_lst) {
#'   group_cols <- dplyr::group_vars(.eeg_lst)
#'   segments <- .eeg_lst$.segments %>%
#'     {
#'       .[names(.) %in% c(obligatory_cols$.segments, group_cols)]
#'     } %>%
#'     data.table::data.table()
#'   data.table::setkey(segments, .id)
#'   dt <- .eeg_lst$.signal[segments, group_cols, with = FALSE]
#'   if (nrow(dt) == 0) {
#'     return(dt)
#'   } else {
#'     return(dt[, .group := do.call(paste0, .SD)][, (group_cols) := NULL][])
#'   }
#' }

#' @noRd
try_to_downsample <- function(.data, max_sample) {
  if (!anyNA(nsamples(.data)) && (is.numeric(max_sample) && max_sample != 0 &&
    # it will downsample if the samples are at least twice as large than the max_sample
    sum(nsamples(.data)) / 2 > max_sample)) {
    q <- round(sum(nsamples(.data)) / max_sample)
    .data <- eeg_downsample(.data,
      q = q
    )
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
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol | is.infinite(x) | is.na(x)
}

#' @noRd
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "'  needed for this function to work. Please install it."),
      call. = FALSE
    )
  }
}

#' @noRd
require_python <- function(){
  require_pkg("reticulate")
  py_installed <- c(reticulate::py_module_available("pandas"),
                  reticulate::py_module_available("mne"))
  if(!all(py_installed)){
    stop("Python packages pandas and mne are needed for this function to work. Install them with `install_py_eeguana()`",
         call. = FALSE)
  }
}

#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}


#' @noRd
rep.channel_dbl <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}

#' @noRd
rep.sample_int <- function(x, ...) {
  y <- NextMethod()
  structure(y, class = class(x), sampling_rate = sampling_rate(x))
}

#' @noRd
match_arg <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
      envir = sys.frame(sysP)
    )
  }
  if (is.null(arg)) {
    return(choices[1L])
  } else if (!is.character(arg)) {
    stop("'arg' must be NULL or a character vector")
  }
  if (!several.ok) {
    if (identical(arg, choices)) {
      return(arg[1L])
    }
    if (length(arg) > 1L) {
      stop("'arg' must be of length 1")
    }
  } else if (length(arg) == 0L) {
    stop("'arg' must be of length >= 1")
  }

  arg <- trimws(tolower(arg))
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    stop(gettextf("'arg' should be one of %s", paste(dQuote(choices),
      collapse = ", "
    )), domain = NA)
  }
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) {
    stop("there is more than one match in 'match_arg'")
  }
  choices[i]
}

#' Copied from rstan
#' @noRd
is_arg_recognizable <- function(x, y, pre_msg = "", post_msg = "", ...) {
  idx <- match(x, y)
  na_idx <- which(is.na(idx))
  if (length(na_idx) > 0) {
    stop(
      pre_msg, paste(x[na_idx], collapse = ", "), ".",
      post_msg, ...
    )
  }
  return(TRUE)
}


#' @title Simpson integration
#' 
#' @description Computes the integral using simpson or trapezoid rule integration.
#' 
#' @details 
#' Slightly adapted from the package fda.usc, https://github.com/moviedo5/fda.usc/blob/master/R/int.simpson.R
#' Possible values for \code{method} are: \itemize{ \item \code{"TRAPZ"}:
#' Trapezoid rule integration. \item \code{"CSR"}: Composite Simpson's rule
#' integration.  \item \code{"ESR"}: Extended Simpson's rule integration. } 
#' \code{method=CSR} (default).
#' 
#' @param x Sorted vector of x-axis values: \code{argvals}.
#' @param y Vector of y-axis values.
#' @param equi =TRUE, the observed points on each curve are equally spaced (by
#' default).
#' @param method Method for numerical integration, see details.
#' @author Manuel Febrero-Bande, Manuel Oviedo de la Fuente
#' \email{manuel.oviedo@@udc.es}
#' @noRd
int.simpson2 <- function(x, y, equi = TRUE,
                      method="CSR"){
  n=length(x);ny=length(y)
  
  if (n!=ny) stop("Different length in the input data")
  if( n==0) return(NA)
  if (n==2 || ny==2) method="TRAPZ"
  out <- switch(method,
                "TRAPZ" = {
                  if (!equi){
                    idx=2:n
                    value<-as.double((x[idx]-x[idx-1])%*%(y[idx]+y[idx-1]))/2
                  } else {
                    h=(max(x)-min(x))/(n-1)
                    y[c(1,n)]=y[c(1,n)]/2
                    value<-h*sum(y)
                  }
                  value
                },"CSR" = {
                  if (!equi){
                    n=2*n-1
                    app=approx(x,y,n=n);x=app$x;y=app$y}
                  h=(max(x)-min(x))/(n-1)
                  value=(h/3)*(y[n]+y[1]+2*sum(y[2*(1:((n-1)/2))+1])+4*sum(y[2*(1:((n-1)/2))]))
                },
                "ESR" = {
                  if (!equi){
                    n = 2*n-1
                    app = approx(x,y,n=n)
                    x=app$x
                    y=app$y
                  }
                  h = (max(x)-min(x))/(n-1)
                  if (n<=4) stop("This method needs n>4")
                  value=17*(y[1]+y[n])+59*(y[2]+y[n-1])+43*(y[3]+y[n-2])+49*(y[4]+y[n-3])
                  value=value+48*sum(y[5:(n-4)])
                  value=(h/48)*value
                }
  )
  return(out)
}

#' @noRd
get_frac <- function(x) {
  frac <- MASS::fractions(x)
  splits <- strsplit(attr(frac,"fracs"), "/")[[1]]
  splits[2] <- ifelse(is.na(splits[2]),1, splits[2])
  list(num=as.numeric(splits[1]),denom=as.numeric(splits[2]))
}
