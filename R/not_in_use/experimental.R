#' Resample EEG data
#'
#' Resample a signal by setting new sampling rate or a maximum number of samples.
#' This is a wrapper for \link{interp1} from the
#' \link{signal} package, see its documentation for details. Notice that
#' the code of the \link{signal} package might be outdated.
#'
#'
#' @param x An eegble object.
#' @param sampling_rate New sampling rate.
#' @param max_sample Optionally, the (approximated) maximum sample number can be defined here.
#' @param method Method passed to interp1, "pchip" by default.
#' @param ... Other arguments passed to interp1.
#'
#'
#'
#'
#'
#'
resample <- function(x, sampling_rate = NULL, max_sample = NULL, method = "pchip", ...) {
  UseMethod("resample")
}

#'
resample.eegble <- function(x, sampling_rate = NULL, max_sample = NULL, method = "", ...) {
 
  warning("# Use with caution, resampling is based on decimate from the signal package, which might be outdated")
  message(paste0(
    "# Resampling from ", sampling_rate(x), "Hz to ",
    new_sampling_rate, "Hz."
  ))
}




#' Get the time based on the sample.
#'
#' @param x An eegble.
#' @param unit
#'
#' @examples
#' @return A vector of times in the specified unit
#'
#' @importFrom magrittr %>%
#'

time.eegbl <- function(x = x, unit = "seconds") {
  scaling <- scaling(x, unit)
  t <- rlang::quo(sample / scaling)
  t
}




baseline_ <- function(x, from_sample = -Inf) {
  sample <- get("sample", envir = parent.frame(), inherit = TRUE)
  # sample <- rlang::env(parent.frame())$sample
  x - mean(x[dplyr::between(sample, from_sample, 0)])
}







# library(R.matlab)

# mat <- readMat("/home/bruno/Documents/Working Papers/eegble/data/data_h.mat")


#     Find(function(x) length(x) == 1, events$type)[[1]] %>% class

#   events$type %>% as.character() %>% length
#   events$value %>% as.character() %>% length
#   events$sample %>% as.integer()

#   events$value %>% unlist(.) %>% c() %>% length

#   purrr::map_chr(events$value[1:5], ~ dplyr::if_else(length(c(.x))==0,NA_character_,c(.x) ))
#   purrr::map(events$value[1:5], ~ c(.x) %>% as.character %>% dplyr::if_else(length(.) ==0, NA_character_, .)) %>% unlist

#   dplyr::if_else(length(test$value[[2]]) == 0, NA_character_,test$value[[2]][[1]] )
# test$value[[2]][[1]]
# test$value[[1]][[1]]
# test$value %>% purrr::map_chr(~ dplyr::if_else(length(.x)==0, NA_character_, .x[[1]]))

# unlist(test$value[[1]]) %>% length()


#   purrr::array_branch(mat[[1]][,,1]$cfg[,,1]$event,2) %>% str()
#   purrr::array_branch(mat[[1]][,,1]$cfg[,,1]$event,3) %>% purr::map_dfr()
#   mat[[1]][,,1]$cfg[,,1]$event[,,1]
#   mat[[1]][,,1]$cfg[,,1]$event[,,1] %>% unlist() %>%  t(.) %>% dplyr::as_tibble()




# df <- tibble(n = seq(-10,10), y = rnorm(21), z = runif(21))

# mutate(df, a = y - mean(y[n < 0]))

# # baseline1 <- function(.x) {
# #   .x - mean(.x[n < 0])
# # }

# # baseline2 <- function(.x) {
# # n <- quo(n)
# #   .x - mean(.x[!!n < 0])
# # }

# # baseline3 <- function(.x) {
# #   # n <- enexpr(n)
# #   eval_bare(n, caller_env())
# # }

# baseline1 <- function(.x) {
#   n <- get('n', envir = parent.frame())
#    .x - mean(.x[n < 0])
# }

# baseline2 <- function(.x) {
# n <-  rlang::env_get( parent.frame(), 'n', inherit = TRUE)
#    .x - mean(.x[n < 0])
# }

# # # mutate(df, a = !!baseline1(y))
# # # mutate(df, a = baseline2(y))
# # # mutate(df, a = baseline3(y))
# # # mutate(df, a = baseline4(y))
# #  mutate(df, a = baseline1(y))
# mutate(df, a = baseline2(y))


rereference_ <- function(x, new_ref) {
  dplyr::funs(x - new_ref)
}


# #maybe use row number instead of samples? and ask unit when reading, and save it somewhere in the eegble

# outerFunc <- function() {
#   obj <- "Outer Object"
#   innerFunc()
# }

# innerFunc <- function() {
#   # A local variable with same name is created
#   obj <- "Inner Object"

#   # would like to grab the value of obj from the outer environment
#   # cat( get('obj', envir=parent.frame()) )
#   cat( env_get(parent.frame(), 'obj') )

#    }

#  outerFunc()



xf <- seq(0, 11, length = 500)
yf <- sin(2 * pi * xf / 5)
# xp <- c(0:1,3:10)
# yp <- sin(2*pi*xp/5)
xp <- c(0:10)
yp <- sin(2 * pi * xp / 5)
extrap <- TRUE
lin <- interp1(xp, yp, xf, "linear", extrap = extrap)
spl <- interp1(xp, yp, xf, "spline", extrap = extrap)
pch <- interp1(xp, yp, xf, "pchip", extrap = extrap)
cub <- interp1(xp, yp, xf, "cubic", extrap = extrap)
near <- interp1(xp, yp, xf, "nearest", extrap = extrap)
plot(xp, yp, xlim = c(0, 11))
lines(xf, lin, col = "red")
lines(xf, spl, col = "green")
lines(xf, pch, col = "orange")
lines(xf, cub, col = "blue")
lines(xf, near, col = "purple")
