#' @noRd
as_plain_df <- function(df){
    df <- lapply(df, function(c)  `attributes<-`(c, NULL))
    as.data.frame(df)
}

#' @noRd
expect_equal_plain_df <-function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
    eval(bquote(expect_equal(as_plain_df(.(object)),as_plain_df(.(expected)))))
}


#' @noRd
expect_equal_but_cnt_sgl <-function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.signal <- object$.signal[0,]
  expected$.signal <- expected$.signal[0,]
    eval(bquote(expect_equal(.(object), .(expected))))
}

#' @noRd
expect_equal_but_sgl <-function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.signal <- NULL
  expected$.signal <- NULL
    eval(bquote(expect_equal(.(object), .(expected))))
}

#' @noRd
expect_equal_but_cnt_sgm <-function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.segments <- object$.segments[0,]
  expected$.segments <- expected$.segments[0,]
    eval(bquote(expect_equal(.(object), .(expected))))
}

#' @noRd
expect_equal_but_sgm <-function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  object$.segments <- NULL
  expected$.segments <- NULL
    eval(bquote(expect_equal(.(object), .(expected))))
}
