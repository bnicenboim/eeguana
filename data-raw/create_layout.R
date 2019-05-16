library(eeguana)

layout_32_1020 <- readr::read_csv("data-raw/layout_32_1020.csv")

usethis::use_data(layout_32_1020, overwrite = TRUE)
