#' Helper function to read the dat files directly,
#' samples doesn't do anything for now
#' @noRd
read_dat <- function(file, header_info = NULL, events_dt = NULL,
                     .recording, sep, zero, samples) {
  n_chan <- nrow(header_info$chan_info)
  common_info <- header_info$common_info

  if (chr_detect(common_info$orientation, "vector",
    ignore.case = TRUE
  )) {
    multiplexed <- FALSE
  } else if (chr_detect(common_info$orientation, "multipl",
    ignore.case = TRUE
  )) {
    multiplexed <- TRUE
  } else {
    stop("Orientation needs to be vectorized or multiplexed.", call. = FALSE)
  }

  ## multiplexed <- dplyr::case_when(
  ##   chr_detect(common_info$orientation, "vector",
  ##     ignore.case = TRUE
  ##   ) ~ FALSE,
  ##   chr_detect(common_info$orientation, "multipl",
  ##     ignore.case = TRUE
  ##   ) ~ TRUE,
  ##   TRUE ~ NA
  ## ) %>% {
  ##   if (is.na(.)) {
  ##     stop("Orientation needs to be vectorized or multiplexed.")
  ##   } else {
  ##     .
  ##   }
  ## }


  if (common_info$format == "BINARY") {
    type <- chr_extract(common_info$bits, "float|int", ignore.case = TRUE) %>%
      tolower() %>%
      {
        dplyr::case_when(
          . == "float" ~ "double",
          . == "int" ~ "integer",
          TRUE ~ .
        )
      }
    if (!type %in% c("double", "integer")) {
      stop(sprintf("Type '%s' is not recognized (it should be double (float) or integer (int)", type))
    }

    bytes <- as.numeric(chr_extract(common_info$bits, "\\d*$")) / 8

    raw_signal <- read_bin_signal(file, type = type, bytes = bytes, n_chan = n_chan, sample_x_channels = multiplexed)
  } else if (common_info$format == "ASCII") {
    raw_signal <- data.table::fread(file,
      skip = common_info$SkipLines,
      dec = common_info$DecimalSymbol,
      sep = " ",
      header = FALSE
    )
    if (common_info$SkipColumns > 0) {
      raw_signal <- raw_signal[, -common_info$SkipColumns, with = FALSE]
    }
    if (!multiplexed) {
      raw_signal <- data.table::transpose(raw_signal)
    }
  }

  # if there is a resolution use it. (This seems to be relevant only if the encoding is integer)
  if (!all(is.na(header_info$chan_info$resolution))) {
    raw_signal <- raw_signal[, purrr::map2(.SD, header_info$chan_info$resolution, ~ .x * .y)]
  }

  # TODO maybe convert to data.table directly
  # Adding the channel names to event table
  events_dt <- add_event_channel(events_dt, labels = header_info$chan_info$.channel) %>%
    data.table::as.data.table()


  # Initial samples as in Brainvision
  max_sample <- nrow(raw_signal)
  sample_id <- seq_len(max_sample)

  if (nrow(events_dt %>% dplyr::filter(!!sep)) == 0) {
    stop("Segment separation marker ", rlang::quo_text(sep), " not found in the events table.")
  }

  # the first event can't be the end of the segment
  # and the last segment ends at the end of the file
  .upper <- events_dt %>%
    dplyr::filter(!!sep) %>%
    dplyr::slice(-1) %>%
    {
      .$.initial - 1
    } %>%
    c(., max_sample)

  .lower <- events_dt %>%
    dplyr::filter(!!sep) %>%
    .$.initial

  .first_sample <- events_dt %>%
    dplyr::filter(!!zero) %>%
    .$.initial

  # In case the time zero is not defined
  if (length(.first_sample) == 0) .first_sample <- .lower

  # segmented id info and sample
  segmentation <- data.table::data.table(.lower, .first_sample, .upper)
  segmentation[, .id := seq_len(.N)]
  seg_sample_id <- data.table::data.table(.sample = sample_id) %>%
    .[segmentation,
      on = .(.sample >= .lower, .sample <= .upper),
      .(.id, .sample = x..sample, .first_sample)
    ]

  seg_sample_id[, .sample := .sample - .first_sample + 1L]

  signal_tbl <- new_signal_tbl(
    signal_matrix = raw_signal,
    .id = as.integer(seg_sample_id$.id),
    .sample = new_sample_int(seg_sample_id$.sample, sampling_rate = common_info$sampling_rate),
    channels_tbl = header_info$chan_info
  )
  events_dt[, .id := 1]
  segmentation[, .new_id := .id][, .id := 1]
  seg_events <- update_events(as_events_tbl(events_dt, common_info$sampling_rate), segmentation)

  segments <- build_segments_tbl(
    .id = seq(length(.lower)),
    .recording = .recording
  )

  eeg_lst <- eeg_lst(
    signal_tbl = signal_tbl,
    events_tbl = seg_events,
    segments_tbl = segments
  )

  message_verbose(paste0(
    "# Data from ", file,
    " was read."
  ))
  message_verbose(paste0(
    "# Data from ", nrow(eeg_lst$.segments),
    " segment(s) and ", nchannels(eeg_lst), " channels was loaded."
  ))
  message_verbose(say_size(eeg_lst))
  eeg_lst
}

build_segments_tbl <- function(.id, .recording) {
  data.table::data.table(.id = .id)[
    ,
    .recording := .recording
  ][
    , segment := seq_len(.N),
    by = ".recording"
  ]
}

# TODO check this function, it might be unnecesary
add_event_channel <- function(events, labels) {
  labels <- make_names(labels)

  if (!".channel" %in% names(events)) {
    events[, .channel := NA_character_]
  }
  # having a zero is problematic for labels[.channel]
  events[.channel == 0, .channel := NA]
  events[, .channel := labels[.channel]]
}

segment_events <- function(events, .lower, .initial, .upper) {
  segmentation <- data.table::data.table(.lower, .initial, .upper)
  segmentation[, .id := seq_len(.N)]

  cols_events_temp <- unique(c(colnames(events), colnames(segmentation), "i..initial", "i..final", "x..lower"))
  cols_events <- c(".id", colnames(events))
  new_events <- data.table::as.data.table(events)
  new_events[, lowerb := .final]

  # We want to capture events that span after the .lower bound ,that is .final over .lower
  # and events and that start before the .upper bound:
  new_events <- segmentation[new_events,
    on = .(.lower <= lowerb, .upper >= .initial),
    ..cols_events_temp, allow.cartesian = TRUE
  ][!is.na(.id)]

  # i..initial are the original.initial from the events file
  # .initial is the first sample of each segment
  # x..lower is the original .lower of segmentation
  new_events[, .initial := pmax(i..initial, x..lower), by = .id]
  new_events[, .final := pmin(i..final, x..upper), by = .id]
  out_events <- new_events[, ..cols_events]
  ## data.table::setattr(out_events, "class", c("events_tbl",class(out_events)))
  out_events
}


built_eeg_lst <- function(eeg_lst, file) {
  message_verbose(paste0(
    "# Data from ", file,
    " was read."
  ))
  message_verbose(paste0(
    "# Data from ", nrow(eeg_lst$.segments),
    " segment(s) and ", nchannels(eeg_lst), " channels was loaded."
  ))
  message_verbose(say_size(eeg_lst))
  validate_eeg_lst(eeg_lst)
}


read_vmrk <- function(file) {
  # Each entry looks like this in the vmrk file:
  # Mk<Marker number>=<Type>,<Description>,<Position in data
  # points>, <Size in data points>, <Channel number (0 = marker is related to
  # all channels)>, <Date (YYYYMMDDhhmmssuuuuuu)>
  # More information can be found in
  # http://pressrelease.brainproducts.com/markers/

  markers <- readChar(file, file.info(file)$size) %>%
    chr_extract("(?ms)^Mk[0-9]*?=.*^Mk[0-9]*?=.*?$") %>%
    chr_remove("\r$")

  col_names <- c(
    ".type", ".description", ".initial",
    ".final", ".channel", "date"
  )
  events <- data.table::fread(markers,
    fill = TRUE,
    header = FALSE,
    col.names = col_names
  )
  # splits Mk<Marker number>=<Type>, removes the Mk.., and <Date>
  events[, .type := strsplit(.type, "=") %>%
    purrr::map_chr(~ .x[[2]])][, date := NULL]
  events[, .final := .initial + .final - 1L]


  ## bug of BV1, first sample is supposed to be 1
  BV1 <- "BrainVision Data Exchange Marker File, Version 1.0"
  if (chr_detect(readChar(file, nchar(BV1)), BV1) &&
    events[.type == "New Segment", .initial][1] == 0) {
    events[.type == "New Segment" & .initial == 0, c(".initial", ".final") := list(1, 1)]
  }
  events
}

read_vhdr_metadata <- function(file) {
  vhdr <- ini::read.ini(file)

  channel_info <- vhdr[["Channel Infos"]] %>%
    imap_dtr(~ c(.y, strsplit(.x, ",")[[1]]) %>%
      t() %>%
      data.table::as.data.table())

  if (ncol(channel_info) == 4) {
    channel_info <- dplyr::mutate(channel_info, empty_col = NA_real_)
  }


  channel_info <- channel_info %>%
    stats::setNames(c("number", ".channel", ".reference", "resolution", "unit")) %>%
    dplyr::mutate(
      resolution = as.double(resolution),
      unit = "microvolt",
      .reference = data.table::fifelse(.reference == "", NA_character_, .reference)
    )
  # To avoid problems with the unicode characters, it seems that brainvision uses "mu" instead of "micro"
  # TODO: check if the unit could be different here


  if (is.null(vhdr$Coordinates)) {
    coordinates <- dplyr::tibble(number = channel_info$number, radius = NA_real_, theta = NA_real_, phi = NA_real_)
  } else {
    coordinates <- vhdr$Coordinates %>%
      imap_dtr(~ c(.y, strsplit(.x, ",")[[1]]) %>%
        t() %>%
        data.table::as.data.table(.name_repair = "unique")) %>%
      stats::setNames(c("number", "radius", "theta", "phi")) %>%
      dplyr::mutate_at(dplyr::vars(c("radius", "theta", "phi")), as.numeric)
  }

  # this is in case it can't find DataPoints and DataType in the header file
  DataPoints <- NA
  DataType <- "time"

  common_info <- vhdr[["Common Infos"]] %>%
    data.table::as.data.table() %>%
    dplyr::transmute(
      data_points = as.numeric(DataPoints),
      # seg_data_points = as.numeric(SegmentDataPoints),
      orientation = DataOrientation,
      format = DataFormat,
      domain = DataType,
      sampling_rate = 1000000 / as.double(SamplingInterval),
      data_file = DataFile,
      vmrk_file = MarkerFile
    )


  if (common_info$format == "ASCII") {
    common_info <- common_info %>%
      dplyr::mutate(
        DecimalSymbol = vhdr[["ASCII Infos"]][["DecimalSymbol"]],
        SkipColumns = vhdr[["ASCII Infos"]][["SkipColumns"]] %>% as.integer(),
        SkipLines = vhdr[["ASCII Infos"]][["SkipLines"]] %>% as.integer()
      )
  } else if (common_info$format == "BINARY") {
    common_info <- common_info %>%
      dplyr::mutate(bits = vhdr[["Binary Infos"]][["BinaryFormat"]])
  }

  if (substr(common_info$domain, start = 1, stop = nchar("time")) %>%
    tolower() != "time") {
    stop("DataType needs to be 'time'")
  }

  # TODO use the _dt version as in read_set
  chan_info <- dplyr::full_join(channel_info, coordinates, by = "number") %>%
    dplyr::bind_cols(purrr::pmap_dfr(
      list(.$radius, .$theta, .$phi),
      spherical_to_xyz
    ))

  out <- list()
  out$chan_info <- chan_info
  out$common_info <- common_info
  return(out)
}

## #' Spherical horiz. angle ("sph_theta")
## #' Spherical azimuth angle ("sph_phi")', ...
## #' Spherical radius ("sph_radius")'
## #' @noRd
## besa_loc_2_xyz <- function(azimuth, horiz_angle) {
##   spherical_to_xyz(radius = 1, theta = azimuth, phi = horiz_angle)
## }

#' Transform spherical coordinates to xyz
#'
#' Transform spherical coordinates, such as the ones provided by BrainVision or sph.....
#'
#' @noRd
spherical_to_xyz <- function(radius = 1, theta = NULL, phi = NULL) {
  radius <- ifelse((is.na(radius) | is.null(radius)) & is.numeric(theta) & is.numeric(phi), 1, radius)

  x <- ifelse(radius != 0, round(sin(theta * pi / 180) * cos(phi * pi / 180), 2), NA_real_)
  y <- ifelse(radius != 0, round(sin(theta * pi / 180) * sin(phi * pi / 180), 2), NA_real_)
  z <- ifelse(radius != 0, round(cos(theta * pi / 180), 2), NA_real_)
  dplyr::tibble(.x = x, .y = y, .z = z)
}

spherical_to_xyz_dt <- function(radius = 1, theta = NULL, phi = NULL) {
  radius <- ifelse((is.na(radius) | is.null(radius)) & is.numeric(theta) & is.numeric(phi), 1, radius)

  data.table::data.table(
    .x = ifelse(radius != 0, round(sin(theta * pi / 180) * cos(phi * pi / 180), 2), NA_real_),
    .y = ifelse(radius != 0, round(sin(theta * pi / 180) * sin(phi * pi / 180), 2), NA_real_),
    .z = ifelse(radius != 0, round(cos(theta * pi / 180), 2), NA_real_)
  )

  # x,y,z
}

read_bin_signal <- function(file, type = "double", bytes = 4, n_chan, n_trials = 1, sample_x_channels = TRUE) {
  if (!file.exists(file)) stop("Binary file ", file, " cannot be found.", call. = FALSE)
  amps <- readBin(file,
    what = type, n = file.info(file)$size / bytes,
    size = bytes
  )
  if (n_trials == 1) {
    matrix(amps, ncol = n_chan, byrow = sample_x_channels) %>%
      data.table::as.data.table()
  } else {
    samples <- length(amps) / (n_trials * n_chan)
    arraydata <- array(amps, dim = c(n_chan, n_trials, samples)) %>%
      apply(1, c)

    if (!sample_x_channels) {
      arraydata <- t(arraydata)
    }
    data.table::as.data.table(arraydata)
  }


  ## that one is faster than:
  ## raw_signal <- matrix(as.matrix(amps), ncol = n_chan, byrow = multiplexed) %>%
  ##   data.table::as.data.table(),
  ## raw_signal <- data.table::data.table(matrix(amps, ncol = n_chan, byrow = multiplexed)
  ## )
  ## )
}
