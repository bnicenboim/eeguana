#  meta programming for using data.table
# https://stackoverflow.com/questions/15790743/data-table-meta-programming

# maybe export map as in tibble

#remove tibble from dependencies?
# remove dplyr from depends using this register S3 as in dtplyr

# #' @noRd
# `[.sample_id` <- function(x, ...) {
#   new_sample_int(NextMethod())
# }


# `[.channel` <- function(x, ...) {
#   new_channel_dbl(NextMethod())
# }
#



# #' Convert a time point into a sample point.
# #'
# #'
# #' @param x A vector of times.
# #' @param unit "seconds" (or "s"), "milliseconds" (or "ms")
# #' @param sampling_rate
# #'
# #' @return A `sample_id` object.
# #'
# #' @importFrom magrittr %>%
# #'
# #' @examples
# #' \dontrun{
# #'
# #' faces_segs_some %>% filter(between(.sample_id, as_sample_id(100, unit = "ms", sampling_rate = 500),
# #'                               as_sample_id(300, unit = "ms", sampling_rate = 500)))
# #' }
# #' @export
# as_sample_int <- function(x, unit = "seconds", sampling_rate) {
#   # TODO I could check if it's been called inside the eeg_lst and extract the sampling rate.
#   x * scaling(sampling_rate, unit)
# }