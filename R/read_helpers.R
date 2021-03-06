#' Helper function to read the dat files directly,
#' samples doesn't do anything for now
#' @noRd
read_dat <- function(file, header_info = NULL, events = NULL,
                     .recording, sep, zero, samples) {
  n_chan <- nrow(header_info$chan_info)
  common_info <- header_info$common_info

  multiplexed <- dplyr::case_when(
    stringr::str_detect(common_info$orientation, stringr::regex("vector",
      ignore_case = TRUE
    )) ~ FALSE,
    stringr::str_detect(common_info$orientation, stringr::regex("multipl",
      ignore_case = TRUE
    )) ~ TRUE,
    TRUE ~ NA
  ) %>% {
    if (is.na(.)) {
      stop("Orientiation needs to be vectorized or multiplexed.")
    } else {
      .
    }
  }


  if (common_info$format == "BINARY") {
    type <- stringr::str_extract(common_info$bits, stringr::regex("float|int", ignore_case = TRUE)) %>%
      stringr::str_to_lower() %>%
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

    bytes <- stringr::str_extract(common_info$bits, stringr::regex("\\d*$")) %>%
      as.numeric() %>%
      {
        . / 8
      }

    amps <- readBin(file,
      what = type, n = file.info(file)$size/bytes,
      size = bytes)

    # TODO: check to optimize the following line
    raw_signal <- matrix(as.matrix(amps), ncol = n_chan, byrow = multiplexed) %>%
      data.table::as.data.table()
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
  events <- add_event_channel(events, header_info$chan_info$.channel) %>%
    data.table::as.data.table() 


  # Initial samples as in Brainvision
  max_sample <- nrow(raw_signal)
  sample_id <- seq_len(max_sample)

  if (nrow(events %>% dplyr::filter(!!sep)) == 0) {
    stop("Segment separation marker ", rlang::quo_text(sep), " not found in the events table.")
  }

  # the first event can't be the end of the segment
  # and the last segment ends at the end of the file
  .upper <- events %>%
    dplyr::filter(!!sep) %>%
    dplyr::slice(-1) %>%
    {
      .$.initial - 1
    } %>%
    c(., max_sample)

  .lower <- events %>% dplyr::filter(!!sep) %>% .$.initial

  .first_sample <- events %>%
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
  events[, .id := 1]
  segmentation[, .new_id := .id][, .id := 1]
  seg_events <- update_events(as_events_tbl(events, common_info$sampling_rate), segmentation)

  segments <- build_segments_tbl(
    .id = seq(length(.lower)),
    .recording = .recording
  )

  eeg_lst <- eeg_lst(
    signal_tbl = signal_tbl,
    events_tbl = seg_events,
    segments_tbl = segments
  )

  message(paste0(
    "# Data from ", file,
    " was read."
  ))
  message(paste0(
    "# Data from ", nrow(eeg_lst$.segments),
    " segment(s) and ", nchannels(eeg_lst), " channels was loaded."
  ))
  message(say_size(eeg_lst))
  eeg_lst
}

build_segments_tbl <- function(.id, .recording) {
  dplyr::tibble(
    .id = .id,
    .recording = .recording
  ) %>%
    dplyr::group_by(.recording) %>%
    dplyr::mutate(segment = 1:dplyr::n()) %>%
    dplyr::ungroup()
}
add_event_channel <- function(events, labels) {
  labels <- make_names(labels)
  dplyr::mutate(events, .channel = if (".channel" %in% names(events)) {
    .channel
  } else {
    0L
  }) %>%
    dplyr::mutate(
      .channel = dplyr::if_else(
        .channel == 0, NA_integer_,
        .channel
      ) %>%
        labels[.]
    )
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



read_vmrk <- function(file) {
  # Each entry looks like this in the vmrk file:
  # Mk<Marker number>=<Type>,<Description>,<Position in data
  # points>, <Size in data points>, <Channel number (0 = marker is related to
  # all channels)>, <Date (YYYYMMDDhhmmssuuuuuu)>
  # More information can be found in
  # http://pressrelease.brainproducts.com/markers/

  markers <- readChar(file, file.info(file)$size) %>%
    stringr::str_extract(stringr::regex("^Mk[0-9]*?=.*^Mk[0-9]*?=.*?$", multiline = TRUE, dotall = TRUE))

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
  events[, .type := stringr::str_split(.type, "=") %>%
    purrr::map_chr(~ .x[[2]])][, date := NULL]
  events[, .final := .initial + .final - 1L]


  ## bug of BV1, first sample is supposed to be 1
  BV1 <- "BrainVision Data Exchange Marker File, Version 1.0"
  if (stringr::str_detect(readChar(file, nchar(BV1)), BV1) &&
    events[.type == "New Segment", .initial][1] == 0) {
    events[.type == "New Segment" & .initial == 0, c(".initial", ".final") := list(1, 1)]
  }
  events
}

read_vhdr_metadata <- function(file) {
    vhdr <- ini::read.ini(file)

  channel_info <- vhdr[["Channel Infos"]] %>%
    imap_dtr(~ c(.y, stringr::str_split(.x, ",")[[1]]) %>%
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
      .reference = data.table::fifelse(.reference=="", NA_character_,.reference)
    )
   # To avoid problems with the unicode characters, it seems that brainvision uses "mu" instead of "micro"
  # TODO: check if the unit could be different here


  if (is.null(vhdr$Coordinates)) {
    coordinates <- dplyr::tibble(number = channel_info$number, radius = NA_real_, theta = NA_real_, phi = NA_real_)
  } else {
    coordinates <- vhdr$Coordinates %>%
      imap_dtr(~ c(.y, stringr::str_split(.x, ",")[[1]]) %>%
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
  
  if (stringr::str_sub(common_info$domain, 1, nchar("time")) %>%
    stringr::str_to_lower() != "time") {
    stop("DataType needs to be 'time'")
  }

  chan_info <- dplyr::full_join(channel_info, coordinates, by = "number") %>%
    dplyr::bind_cols(purrr::pmap_dfr(
      list(.$radius, .$theta, .$phi),
      brainvision_loc_2_xyz
    ))

  out <- list()
  out$chan_info <- chan_info
  out$common_info <- common_info
  return(out)
}


besa_loc_2_xyz <- function(azimuth, horiz_angle) {
  brainvision_loc_2_xyz(radius = 1, theta = azimuth, phi = horiz_angle)
}

brainvision_loc_2_xyz <- function(radius = 1, theta = NULL, phi = NULL) {
  x <- dplyr::if_else(radius != 0, round(sin(theta * pi / 180) * cos(phi * pi / 180), 2), NA_real_)
  y <- dplyr::if_else(radius != 0, round(sin(theta * pi / 180) * sin(phi * pi / 180), 2), NA_real_)
  z <- dplyr::if_else(radius != 0, round(cos(theta * pi / 180), 2), NA_real_)
  dplyr::tibble(.x = x, .y = y, .z = z)
}
