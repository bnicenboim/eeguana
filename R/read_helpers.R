#' Helper function to read the dat files directly
#' @importFrom magrittr %>%

read_dat <- function(file, header_info = NULL, events = NULL,
                     recording, sep, zero) {
  n_chan <- nrow(header_info$chan_info)
  common_info <- header_info$common_info
  if (common_info$format == "BINARY") {
    samplesize <- dplyr::case_when(
      stringr::str_detect(common_info$bits, stringr::regex("float_32",
        ignore_case = TRUE
      )) ~ 4,
      stringr::str_detect(common_info$bits, stringr::regex("int_32",
        ignore_case = TRUE
      )) ~ 4,
      stringr::str_detect(common_info$bits, stringr::regex("int_16",
        ignore_case = TRUE
      )) ~ 2,
      TRUE ~ NA_real_
    )

    amps <- readBin(file,
      what = "double", n = file.info(file)$size,
      size = samplesize
    )
    byrow <- dplyr::case_when(
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

    raw_signal <- matrix(as.matrix(amps), ncol = n_chan, byrow = byrow) %>%
      tibble::as.tibble()

  } else if (common_info$format == "ASCII") {
    raw_signal <- readr::read_delim(file,
      delim = " ",
      col_types =
        readr::cols(.default = readr::col_double())
    )
  }

  colnames(raw_signal) <- header_info$chan_info$labels

  events <- add_event_channel(events, colnames(raw_signal))

  raw_signal_samples <- tibble::tibble(.sample_n = 
              1:nrow(raw_signal)) %>%
    dplyr::bind_cols(raw_signal)


  # the first event can't be the end of the segment
  # and the last segment ends at the end of the file
  end_segs <- events %>%
    dplyr::filter(!!sep) %>%
    dplyr::slice(-1) %>%
    {
      .$.sample_0 - 1
    } %>%
    c(., nrow(raw_signal_samples))

  beg_segs <- events %>% dplyr::filter(!!sep) %>% .$.sample_0
  # segs <- list(beg = beg_segs, t0 = t0, end = end_segs)

  s0 <- events %>%
    dplyr::filter(!!zero) %>%
    .$.sample_0

  # In case the time zero is not defined
  if (length(s0) == 0) s0 <- beg_segs

# bind_rows looses the attributes
# https://github.com/tidyverse/dplyr/issues/2457

  # TODO, make the following faster,probably in c++?, or try data.table
  signal <- purrr::pmap_dfr(list(beg_segs, s0, end_segs),
    .id = ".id",
    function(b, s0, e) raw_signal_samples %>%
        # filter the relevant samples
        dplyr::filter(
          .sample_n >= b,
          .sample_n <= e
        ) %>%
        # the first sample of a segment is 1
        dplyr::mutate(.sample_n = .sample_n - s0 + 1L) %>%
        # order the signal df:
        dplyr::select(.sample_n, dplyr::everything())
  ) %>%
    dplyr::mutate(.id = as.integer(.id),
      #needs to be added only after the bind_row implicit in purrr
                  .sample_n = new_sample_n(.sample_n, 
                            sampling_rate =  common_info$sampling_rate))

  # I add the channel info now
  signal[,colnames(raw_signal)] <- purrr::map2_dfc(
    purrr::transpose(header_info$chan_info), signal[,colnames(raw_signal)], 
    function(chan_info, sig) {
      channel = new_channel(value = sig, as.list(chan_info)) }
  )


  seg_events <- segment_events(events, beg_segs, s0, end_segs)

  segments <- tibble::tibble(
    .id = seq(length(beg_segs)),
    recording = recording, segment = .id
  )

  eegble <- new_eegble(
    signal = signal, 
    events =   seg_events, 
   segments = segments
  ) %>% validate_eegble()


  message(paste0(
    "# Data from ", file,
    " was read."
  ))
  message(paste0(
    "# Data from ", nrow(eegble$segments),
    " segment(s) and ", nchannels(eegble), " channels was loaded."
  ))
  message(say_size(eegble))
  eegble

}


add_event_channel <- function(events, labels) {
  dplyr::mutate(events, .channel = if (".channel" %in% names(events)) {
    .channel
  } else {
    0L
  }) %>%
    dplyr::mutate(.channel = dplyr::if_else(
      .channel == 0, NA_integer_,
      .channel
    ) %>%
      labels[.] 
      %>%
      forcats::lvls_expand(new_levels = labels))
}

segment_events <- function(events, beg_segs, s0, end_segs) {
  purrr::pmap_dfr(list(beg_segs, s0, end_segs),
    .id = ".id",
    function(b, s0, e) events %>%
        # filter the relevant events
        # started after the segment (b)
        # or span after the segment (b)
        dplyr::filter(
          .sample_0 >= b | .sample_0 + .size - 1 >= b,
          # start before the end
          .sample_0 <= e
        ) %>%
        dplyr::mutate(
          .size = dplyr::if_else(.sample_0 < b, b - .size, .size),
          sample = dplyr::case_when(
            .sample_0 >= b ~ .sample_0 - s0 + 1L,
            .sample_0 < b ~ b - s0 + 1L
          )
        )
  ) %>%
    dplyr::mutate(.id = as.integer(.id))
}




#' @importFrom magrittr %>%

read_vmrk <- function(file) {
  # Each entry looks like this in the vmrk file:
  # Mk<Marker number>=<Type>,<Description>,<Position in data
  # points>, <Size in data points>, <Channel number (0 = marker is related to
  # all channels)>, <Date (YYYYMMDDhhmmssuuuuuu)>
  # More information can be found in
  # http://pressrelease.brainproducts.com/markers/
  markers_info_lines <- readr::read_lines(file) %>%
    stringr::str_detect("Mk[0-9]*?=") %>%
    which()
  start <- markers_info_lines %>% min() - 1
  end <- markers_info_lines %>% max() - start

  col_names <- c(
    "Mk_number=Type", "description", ".sample_0",
    ".size", ".channel", "date"
  )

  events <- suppressWarnings(readr::read_csv(file,
    col_names = col_names,
    col_types = readr::cols(
      `Mk_number=Type` = readr::col_character(),
      description = readr::col_character(),
      sample = readr::col_integer(),
      size = readr::col_integer(),
      channel = readr::col_integer(),
      date = readr::col_double()
    ),
    skip = start, n_max = end,
    trim_ws = TRUE
  )) %>%
    tidyr::separate(`Mk_number=Type`,
      c("mk", "type"),
      sep = "="
    ) %>%
    dplyr::mutate(mk = as.numeric(stringr::str_remove(mk, "Mk"))) %>%
    dplyr::select(-mk, -date)


  # segs <- tibble_vmrk %>%  dplyr::transmute(
  #                           bounds = ifelse(type == "Stimulus", NA, type), sample) %>%
  #                 dplyr::filter(!is.na(bounds))

  return(events)
}


#' @importFrom magrittr %>%

read_vhdr_metadata <- function(file) {
  content_vhdr <- readr::read_file(file) %>%
    stringr::str_match_all(stringr::regex("([A-Za-z ]*?)\\](.*?)(\\[|\\Z)",
      dotall = TRUE, multiline = TRUE
    )) %>%
    .[[1]]

  read_metadata <- function(tag, vhdr = content_vhdr) {
    info <- vhdr[vhdr[, 2] == tag, 3]
    if (length(info) == 0) {
      return(tibble::tibble(type = character(), value = character()))
    } else {
      return(readr::read_delim(info,
        delim = "=", comment = ";", col_names = c("type", "value")
      ))
    }
  }


  out <- list()


  channel_info <- read_metadata("Channel Infos") %>%
    tidyr::separate(value, c("labels", "reference", "resolution", "unit"), sep = ",", fill = "right")
  coordinates <- read_metadata("Coordinates") %>%
    tidyr::separate(value, c("radius", "theta", "phi"), sep = ",", fill = "right") %>%
    readr::type_convert(
      col_types =
        readr::cols(
          radius = readr::col_integer(),
          theta = readr::col_integer(),
          phi = readr::col_integer()
        )
    )
  # this is in case it can't find DataPoints and DataType in the header file
  DataPoints <- NA
  DataType <- "time"
  common_info <- read_metadata("Common Infos") %>%
    tidyr::spread(type, value) %>%
    readr::type_convert(col_types = readr::cols(
      DataFile = readr::col_character(),
      DataFormat = readr::col_character(),
      DataOrientation = readr::col_character(),
      MarkerFile = readr::col_character(),
      NumberOfChannels = readr::col_integer(),
      SamplingInterval = readr::col_double()
    )) %>%
    dplyr::transmute(
      data_points = DataPoints,
      # seg_data_points = as.numeric(SegmentDataPoints),
      orientation = DataOrientation,
      format = DataFormat,
      domain = DataType,
      sampling_rate = 1000000 / SamplingInterval,
      data_file = DataFile,
      vmrk_file = MarkerFile
    )

  if (common_info$format == "ASCII") {
    format_info <- read_metadata("ASCII Infos") %>%
      tidyr::spread(type, value) %>%
      readr::type_convert(col_types = readr::cols(
        DecimalSymbol = readr::col_character(),
        SkipColumns = readr::col_integer(),
        SkipLines = readr::col_integer()
      ))
  } else if (common_info$format == "BINARY") {
    format_info <- read_metadata("Binary Infos") %>%
      tidyr::spread(type, value) %>%
      dplyr::rename(bits = BinaryFormat)
  }


  common_info <- dplyr::bind_cols(common_info, format_info)

  if (stringr::str_sub(common_info$domain, 1, nchar("time")) %>%
    stringr::str_to_lower() != "time") {
    stop("DataType needs to be 'time'")
  }

  chan_info <- dplyr::full_join(channel_info, coordinates, by = "type") %>%
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
  tibble::tibble(x = x, y = y, z = z)
}
