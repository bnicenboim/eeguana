#' noRd
as_plain_df <- function(df){
    df <- lapply(df, function(c)  `attributes<-`(c, NULL))
    as.data.frame(df)
}

#' noRd
expect_equal_plain_df <-function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
    eval(bquote(expect_equal(as_plain_df(.(object)),as_plain_df(.(expected)))))
}

