#' @noRd
as_plain_df <- function(df) {
  df <- lapply(df, function(c) `attributes<-`(c, NULL))
  as.data.frame(df)
}

#' @noRd
expect_equal_plain_df <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  eval(bquote(expect_equal(as_plain_df(.(object)), as_plain_df(.(expected)))))
}

#' @noRd
remove_index <- function(.eeg_lst) {
  attributes(.eeg_lst$.signal)$index <- NULL
  attributes(.eeg_lst$.segments)$index <- NULL
  attributes(.eeg_lst$.events)$index <- NULL
  .eeg_lst
}

#' @noRd
expect_equal_eeg_lst <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  eval(bquote(expect_equal(remove_index(.(object)), remove_index(.(expected)))))
}

#' @noRd
expect_equal_but_cnt_sgl <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.signal <- object$.signal[0, ]
  expected$.signal <- expected$.signal[0, ]
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' @noRd
expect_equal_but_sgl <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.signal <- NULL
  expected$.signal <- NULL
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' @noRd
expect_equal_but_cnt_sgm <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.segments <- object$.segments[0, ]
  expected$.segments <- expected$.segments[0, ]
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' @noRd
expect_equal_but_sgm <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.segments <- NULL
  expected$.segments <- NULL
  eval(bquote(expect_equal_eeg_lst(.(object), .(expected))))
}

#' helper functions (borrowed from github.com/stan-dev/bayesplot/R/helpers-testthat.R)
#' @noRd
expect_gg <- function(x) {
  testthat::expect_s3_class(x, "ggplot")
  invisible(ggplot2::ggplot_build(x))
}

#' @noRd
skip_on_actions <- function() {
  if (!identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
    return(invisible(TRUE))
  }
  skip("On GitHub Actions")
}
