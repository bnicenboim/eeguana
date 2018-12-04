#' Read a BrainVision file into an eeg_lst object.
#'
#' @param file A vhdr file in a folder that contains a .vmrk and .dat files
#' @param sep Segment separation marker. By default: type == "New Segment"
#' @param zero Time zero marker. By default: type == "Time 0"
#' @param recording Recording name (file name, by default).
#' 
#' @return An `eeg_lst` object with signal_tbl and event from file_name.dat,
#' file_name.vhdr, and file_name.vmrk.
#' @family read
#' @importFrom magrittr %>%
#'
#' @export
read_vhdr <- function(file, sep = type == "New Segment", zero = type == "Time 0",
                      recording = file) {
  
  if (!file.exists(file)) stop(sprintf("File %s not found in %s",file, getwd()))

  sep <- rlang::enquo(sep)
  zero <- rlang::enquo(zero)
  # sep = rlang::quo(type == "New Segment")
  # zero = rlang::quo(type == "Time 0")

  # Takes the files from the header:
  file_path <- stringr::str_match(file, "(.*(/|\\\\)).")[, 2] %>% {
    if (is.na(.)) NULL else .
  }
  header_info <- tryCatch(read_vhdr_metadata(file),
        error=function(cond) {
            message(paste("Error in the metadata of:", file))
            message(paste(cond,"\n"))
            return(NA)
        },
        warning=function(cond) {
            message(paste("Warning in the metadata of:", file))
            message(paste(cond,"\n"))
            return(NULL)
        })

  data_file <- header_info$common_info$data_file
  data_ext <- tools::file_ext(data_file)
  # It only accepts .dat files (for now)
  vmrk_file <- header_info$common_info$vmrk_file

  file_vmrk <- paste0(file_path, vmrk_file)
  if (!file.exists(file_vmrk)) stop(sprintf("File %s not found in %s",file, getwd()))

  events <- 
   tryCatch(read_vmrk(file = file_vmrk),
        error=function(cond) {
            message(paste("Error in the events of:", paste0(file_path, vmrk_file)))
            message(paste(cond,"\n"))
            return(NA)
        },
        warning=function(cond) {
            message(paste("Warning in the events of:", paste0(file_path, vmrk_file)))
            message(paste(cond,"\n"))
            return(NULL)
        })

  if (data_ext == "dat" || data_ext == "eeg") {
    x <- read_dat(
      file = paste0(file_path, data_file),
      header_info = header_info,
      events = events,
      recording = recording,
      sep = sep,
      zero = zero
    )
  } else {
    warning(paste0(".", data_ext, " files are unsupported."))
  }
  x
}




#' Read a Fieldtrip file into an eeg_lst object (requires R.matlab).
#'
#' @param file A .mat file containing a fieldtrip struct.
#' @param recording Recording name, by default is the file name.
#' @param layout A .mat [layout from Fieldtrip](http://www.fieldtriptoolbox.org/template/layout)
#' @return An `eeg_lst` object with signal_tbl and event from a matlab file.
#'
#' @family read
#' 
#' @importFrom magrittr %>%
#'
#' @export
read_ft <- function(file, layout = NULL, recording = file) {
  # TODO: checks if R.matlab was installed first

  # It should be based on this:
  # http://www.fieldtriptoolbox.org/reference/ft_datatype_raw

  if (!file.exists(file)) stop(sprintf("File %s not found in %s",file, getwd()))

  mat <- R.matlab::readMat(file)

  channel_names <- mat[[1]][, , 1]$label %>% unlist()
  sampling_rate <- mat[[1]][, , 1]$fsample[[1]]

  ## signal_raw df:

  # segment lengths, initial, final, offset
  slengths <- mat[[1]][, , 1]$cfg[, , 1]$trl %>%
    apply(., c(1, 2), as.integer) %>%
    dplyr::as_tibble(.)

  sample <- purrr::pmap(slengths, ~
  seq(..3 + 1, length.out = ..2 - ..1 + 1)) %>%
    unlist() %>%
    new_sample_int(sampling_rate = sampling_rate)

  signal_raw <- purrr::map_dfr(mat[[1]][, , 1]$trial,
    function(lsegment) {
      lsegment[[1]] %>% t() %>% dplyr::as_tibble()
    },
    .id = ".id"
  ) %>% dplyr::mutate(.id = as.integer(.id))
  



  # channel info:
  channels <- dplyr::tibble(
    channel = make.unique(channel_names) %>% make.names()
  )

  if (!is.null(layout)) {
    chan_layout <- R.matlab::readMat(layout) %>%
      {
        dplyr::mutate(.$lay[, , 1]$pos %>% as.data.frame(),
          channel = unlist(.$lay[, , 1]$label)
        )
      } %>%
      dplyr::rename(.x = V1, .y = V2)
    not_layout <- setdiff(chan_layout$channel, channels$channel)
    not_channel <- setdiff(channels$channel, chan_layout$channel)
    warning(paste0(
      "The following channels are not in the layout file: ",
      paste(not_layout, collapse = ", "), "."
    ))
    warning(paste0(
      "The following channels are not in the data: ",
      paste(not_channel, collapse = ", "), "."
    ))
    channels <- dplyr::left_join(channels, dplyr::as_tibble(chan_layout), by = "channel") %>%
      dplyr::mutate(.z = NA_real_, .reference = NA)
  } else {
    channels <- channels %>%
      dplyr::mutate(.x = NA_real_, .y = NA_real_, .z = NA_real_, .reference = NA)
  }


  # colnames(signal_tbl) <- c(".id", channel_names)
  # signal_tbl <- dplyr::mutate(signal_tbl, .sample = sample, .id = as.integer(.id)) %>%
  #   dplyr::select(.id, .sample, dplyr::everything())
  signal_tbl <- new_signal_tbl(signal_matrix = dplyr::select(signal_raw, -.id),
    ids = dplyr::pull(signal_raw,.id), sample_ids = sample, channel_info = channels
  )


  as_first_non0 <- function(col) {
    # creates a function that converts to the class of the first element
    first_class <- Find(function(x) length(x) == 1, col)[[1]] %>% class()
    if (first_class == "character") {
      as.character(col)
    } else if (first_class == "numeric") {
      as.numeric(col)
    } else {
      stop("not numeric or character")
    }
  }

  ## events df:
  events <- mat[[1]][, , 1]$cfg[, , 1]$event[, 1, ] %>%
    t() %>%
    dplyr::as_tibble() %>%
    dplyr::select(-offset) %>%
    dplyr::mutate_all(as_first_non0) %>%
    dplyr::rename(.size = dplyr::matches("duration"), .sample_0 = sample) %>%
    dplyr::mutate(.sample_0 = as.integer(.sample_0), .size = as.integer(.size)) %>%
    add_event_channel(channel_names) %>%
    segment_events(.lower = slengths$V1, .sample_0 = slengths$V1 - slengths$V3, .upper= slengths$V2)

  segments <- tibble::tibble(
    .id = seq(nrow(slengths)),
    recording = recording, segment = .id
  )

  eeg_lst <- new_eeg_lst(
    signal = signal_tbl, events = events, segments = segments
  )


  message(paste0(
    "# Data from ", file,
    " was read."
  ))
  message(paste0(
    "# Data from ", nrow(eeg_lst$segments),
    " segment(s) and ", nchannels(eeg_lst), " channels was loaded."
  ))
  message(say_size(eeg_lst))
  validate_eeg_lst(eeg_lst)
}

#' Read a edf/bdf file into an eeg_lst object.
#'
#' @param file A edf/bdf file
#' @param recording Recording name (file name, by default). If set to NULL or NA, the patient name will be used.
#'
#' @return An `eeg_lst` object.
#' @family read
#'
#' @export
read_edf <- function(file, recording = file) {
  
  if (!file.exists(file)) stop(sprintf("File %s not found in %s",file, getwd()))
 
  
  header_edf <- edfReader::readEdfHeader(file)  
  if(is.null(recording) || is.na(recording)) {
    recording <- header_edf$patient 
    if(recording== "" ||  is.null(recording) || is.na(recording)) {
      stop("Patient information is missing.")
    }
  }
  signal_edf <- edfReader::readEdfSignals(header_edf)
  
  signal_dt <- purrr::map(signal_edf, ~.x$signal) %>% 
                data.table::as.data.table()
  sampling_rate <- purrr::map_dbl(signal_edf, ~.x$sRate) %>% 
                  unique() 
  if(length(sampling_rate)>1) {
    stop("Channels with different sampling rates are not supported.")
  }  
  
  if(header_edf$isContinuous) {
    s_id <- rep(1L,nrow(signal_dt))
    sample_id <- sample_int(seq_len(nrow(signal_dt)),sampling_rate = sampling_rate)
  } else {
    stop("Segmented edfs is not supported yet.")
  }
  channel_info <- dplyr::tibble(channel= header_edf$sHeaders$label, 
                                .x = NA_real_, .y = NA_real_, .z = NA_real_,
                                .reference = NA_character_)
  signal <- new_signal_tbl(signal_matrix = signal_dt,ids = s_id,
                      sample_ids = sample_id, 
                      channel_info = channel_info)
  events <- data.table::data.table(.sample_0 = integer(0), .size= integer(0), .channel= character(0))
  segments <- tibble::tibble(.id = seq_len(max(s_id)),
                            recording = recording, 
                            segment = .id)
  
  eeg_lst <- new_eeg_lst(
    signal = signal, events = events, segments = segments
  )

  message(paste0(
    "# Data from ", file,
    " was read."
  ))
  message(paste0(
    "# Data from ", nrow(eeg_lst$segments),
    " segment(s) and ", nchannels(eeg_lst), " channels was loaded."
  ))
  message(say_size(eeg_lst))
  validate_eeg_lst(eeg_lst)
}