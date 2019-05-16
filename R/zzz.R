# CRAN Note avoidance
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", unlist(obligatory_cols),
      "..cols", "..cols_events", "..cols_events_temp", "..cols_signal",
      "..cols_signal_temp", ".GRP", ".I", ".N", ".SD", ".lower", ".new_id", ".sid", ".upper", ".x", ".y",
      "BinaryFormat", "DataFile", "DataFormat", "DataOrientation", "L", "MarkerFile",
      "Mk_number=Type", "SamplingInterval", "V1", "V2", "amplitude",
      "channel", "", "i..initial",
      "i..size", "lowerb", "mk", "n", "offset", "", ".recording", "resolution",
      "time", ".type", "value", "x..lower", "x..sample",
      "xmin", "xmax", "Event", ".key", ".value", ".time",
      ".first_sample"
    )
  )
}
## data.table needs this
.datatable.aware <- TRUE
