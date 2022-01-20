url_issues <- "https://github.com/bnicenboim/eeguana/issues"


obligatory_cols <- list(
  .signal = c(.id = ".id", .sample = ".sample"),
  .events = c(.id = ".id", .type = ".type", .description = ".description", .initial = ".initial", .final = ".final", .channel = ".channel"),
  .segments = c(.id = ".id", .recording = ".recording")
)
