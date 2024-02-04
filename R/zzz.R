# CRAN Note avoidance
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", unlist(obligatory_cols),
      "..cols", "..cols_events", "..cols_events_temp", "..cols_signal",
      "..cols_signal_temp", ".GRP", ".I", ".N", ".SD", ".lower", ".new_id", ".sid", ".upper", ".x", ".y",
      "BinaryFormat", "DataFile", "DataFormat", "DataOrientation", "L", "MarkerFile",
      "Mk_number=Type", "SamplingInterval", "V1", "V2", "amplitude",
      "channel", "", "i..initial", "i..final", "x..upper",
      "i..size", "lowerb", "mk", "n", "offset", "", ".recording", "resolution",
      "time", ".type", "value", "x..lower", "x..sample",
      "xmin", "xmax", "Event", ".key", ".value", ".time",
      ".first_sample", ".ICA", ".group", ".BY", ".reference", "orig_names", "..orig_names", "where"
    )
  )
}
## data.table needs this
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {

  if(requireNamespace("reticulate", quietly = TRUE)){
    reticulate::use_condaenv("r-eeguana", required = FALSE)
  }

  #dplyr styff
  register_s3_method("dplyr", "group_by", "eeg_lst")
  register_s3_method("dplyr", "groups", "eeg_lst")
  register_s3_method("dplyr", "group_vars", "eeg_lst")
  register_s3_method("dplyr", "ungroup", "eeg_lst")
  register_s3_method("dplyr", "tbl_vars", "eeg_lst")

  register_s3_method("dplyr", "filter", "eeg_lst")
  register_s3_method("dplyr", "filter", "eeg_ica_lst")
  register_s3_method("dplyr", "summarise", "eeg_lst")
  register_s3_method("dplyr", "summarize", "eeg_lst")

  register_s3_method("dplyr", "mutate", "eeg_lst")
  register_s3_method("dplyr", "transmute", "eeg_lst")
  register_s3_method("dplyr", "select", "eeg_lst")
  register_s3_method("dplyr", "rename", "eeg_lst")
  register_s3_method("dplyr", "rename_with", "eeg_lst")

  register_s3_method("dplyr", "left_join", "eeg_lst")
  register_s3_method("dplyr", "semi_join", "eeg_lst")
  register_s3_method("dplyr", "anti_join", "eeg_lst")
  register_s3_method("dplyr", "as_tibble", "eeg_lst")

  register_s3_method("dplyr", "filter", "events_tbl")
  register_s3_method("dplyr", "mutate", "events_tbl")
  register_s3_method("dplyr", "transmute", "events_tbl")
  register_s3_method("dplyr", "summarise", "events_tbl")
  register_s3_method("dplyr", "as_data_frame", "eeg_lst")

  register_s3_method("dplyr", "pull", "eeg_lst")
  # register_s3_method("stats", "na.omit", "eeg_lst")

  register_s3_method("ggplot2", "ggplot", "eeg_lst")
  register_s3_method("ggplot2", "ggplot_add", "layer_events")
  register_s3_method("data.table", "as.data.table", "eeg_lst")

  op <- options()
  op.eeguana <- list(
    eeguana.verbose = TRUE
  )
  toset <- !(names(op.eeguana) %in% names(op))
  if (any(toset)) options(op.eeguana[toset])

  invisible()
}


.onAttach <- function(libname, pkgname) {
    packageStartupMessage(pkgname,
    " version ",
    utils::packageVersion(pkgname),
    "\nAn introduction to the package can be found in https://bruno.nicenboim.me/eeguana/articles/intro.html\n")
}
