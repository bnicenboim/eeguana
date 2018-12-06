url_issues <- "https://github.com/bnicenboim/eeguana/issues"


obligatory_cols <- list(
  signal = c(.id = ".id", .sample_id = ".sample_id"),
  events = c(.id = ".id", .sample_0 = ".sample_0", .size = ".size", .channel = ".channel"),
  segments = c(.id = ".id")
)

# data.table needs this
.datatable.aware = TRUE
