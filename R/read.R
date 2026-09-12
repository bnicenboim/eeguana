#' Read a BrainVision file into R
#'
#' Creates an eeg_lst object from BrainVision exported files.The function reads
#' metadata from the .vhdr BrainVision file, which draws on
#' the .vmrk and .dat/.eeg files.  All three
#' files must be in the same directory.
#'
#'
#'
#' @param file A vhdr file in a folder that contains a .vmrk and .dat files
#' @param .sep Segment separation marker. By default: `.type == "New Segment"`
#' @param .zero Time zero marker. By default: `.type == "Time 0"`
#' @param .recording Recording name (`file` name, by default).
#'
#' @return An `eeg_lst` object with signal_tbl and event from file_name.dat,
#' file_name.vhdr, and file_name.vmrk.
#'
#' @examples
#' \dontrun{
#' # load a single subject
#' s1 <- read_vhdr("./faces.vhdr", .recording = "1")
#'
#' # load multiple subjects using purrr::map, extracting subject IDs from file names
#' faces_list <- purrr::map(list.files("./", "vhdr"), ~
#' read_vhdr(.x))
#' faces <- bind(faces_list)
#' }
#'
#' @family reading and writing functions
#'
#' @export
read_vhdr <- function(file, .sep = .type == "New Segment", .zero = .type == "Time 0",
                      .recording = file) {
  if (!file.exists(file)) stop(sprintf("File %s not found in %s", file, getwd()))
  message_verbose("# Reading file ", file, "...")
  .sep <- rlang::enquo(.sep)
  .zero <- rlang::enquo(.zero)
  # file <- "/home/bruno/dev/eeguana_bugs/files/40.vhdr"
  # .sep = rlang::quo(.type == "New Segment")
  # .zero = rlang::quo(.type == "Time 0")
  # .recording = file
  # Takes the files from the header:
  # file <- "faces.vhdr"
  file_path <- dirname(file)

  header_info <- read_vhdr_metadata(file)

  # header_info <- tryCatch(read_vhdr_metadata(file),
  #       error=function(cond) {
  #           message_verbose(paste("Error in the metadata of:", file))
  #           message_verbose(paste(cond,"\n"))
  #           return(NA)
  #       },
  #       warning=function(cond) {
  #           message_verbose(paste("Warning in the metadata of:", file))
  #           message_verbose(paste(cond,"\n"))
  #           return(NULL)
  #       })
  if (is.null(header_info)) {
    stop("Header info in ", file, " could not be read.", call. = FALSE)
  }
  data_file <- header_info$common_info$data_file
  data_ext <- tools::file_ext(data_file)
  # It only accepts .dat files (for now)
  vmrk_file <- header_info$common_info$vmrk_file

  file_vmrk <- file.path(file_path, vmrk_file)
  if (!file.exists(file_vmrk)) stop(sprintf("File %s not found in %s", file_vmrk, getwd()))
  events_dt <-
    tryCatch(read_vmrk(file = file_vmrk),
      error = function(cond) {
        message_verbose(paste("Error in the events of:", paste0(file_path, vmrk_file)))
        message_verbose(paste(cond, "\n"))
        return(NA)
      },
      warning = function(cond) {
        message_verbose(paste("Warning in the events of:", paste0(file_path, vmrk_file)))
        message_verbose(paste(cond, "\n"))
        return(NULL)
      }
    )
  if (data_ext == "dat" || data_ext == "eeg") {
    x <- read_dat(
      file = file.path(file_path, data_file),
      header_info = header_info,
      events_dt = events_dt,
      .recording = .recording,
      sep = .sep,
      zero = .zero
    )
  } else {
    warning(paste0(".", data_ext, " files are unsupported."))
  }
  x
}




#' Read a Fieldtrip file into R
#'
#' Creates an eeg_lst object from Matlab exported files.The function reads a .mat
#' file using `R.matlab`. If you do not already have
#' `R.matlab` installed in R, you will need to install it yourself. The .mat file
#' should have the structure described in
#' this [Fieldtrip reference article](http://www.fieldtriptoolbox.org/reference/ft_datatype_raw).
#'
#' @param file A .mat file containing a fieldtrip struct.
#' @param .recording Recording name, by default is the file name.
#' @param .layout A .mat [layout from Fieldtrip](http://www.fieldtriptoolbox.org/template/layout)
#' @return An `eeg_lst` object with signal_tbl and event from a Matlab file.
#'
#' @examples
#' \dontrun{
#' s1 <- read_ft("./subject1.mat", layout = "easycapM25.mat", .recording = 1)
#' }
#'
#' @family reading and writing functions
#'
#'
#' @export
read_ft <- function(file, .layout = NULL, .recording = file) {
  # to avoid no visible binding for global variable
  type <- NULL

  require_pkg("R.matlab")

  # It should be based on this:
  # http://www.fieldtriptoolbox.org/reference/ft_datatype_raw
  # file = system.file("testdata", "fieldtrip_matrix.mat", package = "eeguana")
  if (!file.exists(file)) stop(sprintf("File %s not found in %s", file, getwd()))
  message_verbose("# Reading file ", file, "...")

  mat <- R.matlab::readMat(file)[[1]][, , 1]
  channel_names_ <- mat$label %>% unlist()
  # fsample seems to be deprecated, but I can't find the sampling rate anywere
  if (!is.null(mat$fsample)) {
    srate <- mat$fsample[[1]]
  } else {
    # if fsample is not here, I reconstruct the sampling rate from the difference between time steps
    srate <- mean(1 / diff(mat$time[[1]][[1]][1, ]))
  }

  sample <-
    {
      unlist(lapply(mat$time, function(x) unlist(x))) * srate
    } %>%
    new_sample_int(.sampling_rate = srate)

  signal_raw <- map_dtr(mat$trial,
    function(lsegment) {
      lsegment[[1]] %>%
        t() %>%
        data.table::as.data.table()
    },
    .id = ".id"
  )
  signal_raw[, .id := as.integer(.id)]


  # channel info:
  channels <- dplyr::tibble(
    .channel = make_names(channel_names_)
  )

  if (!is.null(.layout)) {
    chan_layout <- R.matlab::readMat(.layout) %>%
      {
        dplyr::mutate(.$lay[, , 1]$pos %>% as.data.frame(),
          .channel = unlist(.$lay[, , 1]$label)
        )
      } %>%
      dplyr::rename(.x = V1, .y = V2)
    not_layout <- setdiff(chan_layout$channel, channels$.channel)
    not_channel <- setdiff(channels$channel, chan_layout$.channel)
    warning(paste0(
      "The following channels are not in the layout file: ",
      paste(not_layout, collapse = ", "), "."
    ))
    warning(paste0(
      "The following channels are not in the data: ",
      paste(not_channel, collapse = ", "), "."
    ))
    channels <- dplyr::left_join(channels, dplyr::as_tibble(chan_layout), by = ".channel") %>%
      dplyr::mutate(.z = NA_real_, .reference = NA)
  } else {
    channels <- channels %>%
      dplyr::mutate(.x = NA_real_, .y = NA_real_, .z = NA_real_, .reference = NA)
  }


  # colnames(signal_tbl) <- c(".id", channel_names_)
  # signal_tbl <- dplyr::mutate(signal_tbl, .sample = sample, .id = as.integer(.id)) %>%
  #   dplyr::select(.id, .sample, dplyr::everything())
  signal_tbl <- new_signal_tbl(
    signal_matrix = dplyr::select(signal_raw, -.id),
    .id = signal_raw$.id, .sample = sample, channels_tbl = channels
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

  # In case the configuration includes events
  if (!is.null(mat$cfg[, , 1]$event) && !is.null(mat$cfg[, , 1]$trl)) {
    # # segment lengths, initial, final, offset
    slengths <- mat$cfg[, , 1]$trl %>%
      apply(., c(1, 2), as.integer) %>%
      data.table::as.data.table()
    ## events df:

    events_dt <- struct_to_dt(mat$cfg[, , 1]$event)[, offset := NULL]
    data.table::setnames(
      events_dt, c("sample", "type", "value"),
      c(".initial", ".type", ".description")
    )
    events_dt[, `:=`(.final = .initial + duration - 1, .id = 1L, duration = NULL)]

    add_event_channel(events_dt, channel_names_)

    ##  events <- mat$cfg[, , 1]$event[, 1, ] %>%
    ##     t() %>%
    ##     dplyr::as_tibble(.name_repair = "unique") %>%
    ##     dplyr::select(-offset) %>%
    ##     dplyr::mutate_all(as_first_non0) %>%
    ##     dplyr::rename(duration = dplyr::matches("duration"), .initial = sample, .type = type, .description = value) %>%
    ##     dplyr::mutate(.final = .initial + duration -1, .id = 1L) %>%
    ##     dplyr::select(-duration) %>%
    ## #TODO check below, transform to dt
    ##   add_event_channel(channel_names_)


    segmentation <- data.table::data.table(
      .lower = slengths$V1,
      .first_sample = slengths$V1 - slengths$V3,
      .upper = slengths$V2
    )

    segmentation[, .new_id := seq_len(.N)][, .id := 1]
    events_dt <- update_events(as_events_tbl(events_dt, srate), segmentation)
  } else {
    events_dt <- NULL
  }

  segments <- build_segments_tbl(.id = seq_len(max(signal_tbl$.id)), .recording)




  if (!is.null(mat$trialinfo)) {
    segments <- segments %>% dplyr::bind_cols(dplyr::as_tibble(mat$trialinfo))
  }

  eeg_lst <- eeg_lst(
    signal_tbl = signal_tbl, events_tbl = events_dt, segments_tbl = segments
  )

  built_eeg_lst(eeg_lst, file)
}



#' Read an edf/edf+/bdf file into R
#'
#' Creates an eeg_lst object from edf, edf+, and bdf file export formats.
#'
#' When trigger information is stored in a "Status" or "Trigger" channel, the trigger
#' value is stored only when the value of the channel increases. This follows the
#' default behavior of [find_events in MNE 0.18](https://mne.tools/0.18/generated/mne.find_events.html?highlight=find_events#mne.find_events).
#' If you have a case where this assumption is incorrect, please open an issue in [https://github.com/bnicenboim/eeguana/issues](https://github.com/bnicenboim/eeguana/issues).
#'
#'
#' @param file A edf/bdf file
#' @param .recording Recording name (file name, by default). If set to NULL or NA, the patient name will be used.
#'
#' @return An `eeg_lst` object.
#'
#' @examples
#' \dontrun{
#' s1 <- read_edf("./faces.edf", .recording = 1)
#' }
#'
#' @family reading and writing functions
#'
#' @param .trigger_channel Name of a channel carrying triggers, or `"last"`
#'   for the last channel that is not an annotation or status channel. `NULL`,
#'   the default, leaves it alone.
#'
#'   Some recording systems write triggers as an ordinary analog channel with
#'   a label of their own, rather than as EDF+ annotations, and nothing in the
#'   header marks it as such. When `.trigger_channel` is given, that channel
#'   is turned into events and removed from the signal table: one event at the
#'   first sample and one wherever the value changes, with `.type` `"Trigger"`
#'   and `.description` holding the **digital code**. EDF stores a trigger as
#'   an integer and scales it into the channel's physical unit, so code 3072
#'   may arrive as 767.4961 uV; the code is what identifies the trigger.
#'
#'   Reading the same file in EEGLAB gives the same latencies but different
#'   values. `pop_biosig()` falls back to the last channel whenever the
#'   annotations are empty, reports the scaled physical value rather than the
#'   code, and passes it through `mod(x, 65536)`, so a baseline of -0.375
#'   appears as 65535.625.
#'
#'   A warning is issued when more than half the channel's samples hold distinct
#'   values, which is what continuous data looks like: real EEG has almost as
#'   many distinct values as samples, while a trigger channel repeats a
#'   handful of codes however many it uses. The channel is still read. The
#'   check does not separate triggers from a flat or disconnected electrode,
#'   because nothing in the file does, so name the channel deliberately rather
#'   than relying on it.
#' @export
read_edf <- function(file, .recording = file, .trigger_channel = NULL) {

  # samples Whether to subset the reading; by default starting from sample 1  until the end of the recording.
  # less samples doesn't speed up reading data, for now I'm hiding it:
  samples <- c(1, Inf)

  if (!file.exists(file)) stop(sprintf("File %s not found in %s", file, getwd()))
  message_verbose("# Reading file ", file, "...")

  header_edf <- edfReader::readEdfHeader(file)
  if (is.null(.recording) || is.na(.recording)) {
    .recording <- header_edf$patient
    if (.recording == "" || is.null(.recording) || is.na(.recording)) {
      stop("Patient information is missing.")
    }
  }
  sampling_rate <- header_edf$sHeaders$sRate %>%
    .[!is.na(.)] %>%
    unique()
  if (length(sampling_rate) > 1) {
    stop("Channels with different sampling rates are not supported.")
  }

  # so that includes the last sample as well
  times <- sample_int(c(samples[1], samples[2] + 1), .sampling_rate = sampling_rate) %>%
    as_time()

  signal_edf <- edfReader::readEdfSignals(header_edf, from = times[1], till = times[2], simplify = FALSE)

  non_signal <- purrr::map_lgl(signal_edf, ~ .x$isAnnotation |
    tolower(.x$label) %in% c("status", "trigger") |
    tolower(.x$name) %in% c("status", "trigger"))

  ## A channel named by the user carries the triggers. Some systems write them
  ## as an ordinary analog channel with a label of their own, so there is
  ## nothing in the header to recognise them by.
  trigger_pos <- NULL
  if (!is.null(.trigger_channel)) {
    labels <- purrr::map_chr(signal_edf, ~ .x$label %||% NA_character_)
    if (identical(.trigger_channel, "last")) {
      candidates <- which(!non_signal)
      if (!length(candidates)) stop("No channel left to use as `.trigger_channel`.", call. = FALSE)
      trigger_pos <- candidates[length(candidates)]
    } else {
      trigger_pos <- which(labels == .trigger_channel & !non_signal)
      if (length(trigger_pos) != 1) {
        stop("`.trigger_channel` ", encodeString(.trigger_channel, quote = "\""),
          " is not one of the channels: ",
          paste(labels[!non_signal], collapse = ", "),
          call. = FALSE
        )
      }
    }
    ## Continuous EEG has almost as many distinct values as samples, a ratio
    ## near 1; a trigger channel repeats a handful of codes, so its ratio is
    ## tiny however many codes it uses. Counting distinct values instead would
    ## reject a legitimate 8-bit trigger line, which carries up to 256 of them.
    ## This does not separate triggers from a flat or disconnected electrode:
    ## nothing in the file does, so naming the channel is the caller's job.
    trig <- signal_edf[[trigger_pos]]$signal
    distinct_ratio <- length(unique(trig)) / max(length(trig), 1L)
    if (distinct_ratio > 0.5) {
      warning("Channel ", encodeString(labels[trigger_pos], quote = "\""),
        " has ", length(unique(trig)), " distinct values across ",
        length(trig), " samples, so it looks like continuous data rather ",
        "than triggers. Reading it as triggers anyway.",
        call. = FALSE
      )
    }
  }

  if (sum(non_signal) >= 2) {
    warning("eeguana cannot deal with more than one annotation or status. It will use the first one.\n If you have a file like that, please open an issue in ", url_issues, " with a link to the offending file")
  }
  if (!is.null(trigger_pos)) non_signal[trigger_pos] <- TRUE

  event_channel <- signal_edf[non_signal]
  channel_names <- header_edf$sHeaders$label[!non_signal]
  trigger_edf <- if (is.null(trigger_pos)) NULL else signal_edf[[trigger_pos]]
  trigger_hdr <- if (is.null(trigger_pos)) {
    NULL
  } else {
    header_edf$sHeaders[trigger_pos, , drop = FALSE]
  }

  signal_edf[non_signal] <- NULL

  signal_dt <- lapply_dtc(signal_edf, function(x) x$signal)

  if (header_edf$isContinuous && all(purrr::map_lgl(signal_edf, ~ .x$isContinuous))) {
    s_id <- rep(1L, nrow(signal_dt))
    sample_id <- sample_int(seq_len(nrow(signal_dt)), .sampling_rate = sampling_rate)
  } else {
    stop("Non continuous edf/bdf files are not supported yet.")
  }

  ## Nothing in an EDF header marks a channel as triggers, so a system that
  ## writes them as an ordinary analog channel looks like any other. Say so
  ## when that is the likely situation, rather than returning an empty events
  ## table with no explanation. Only when no events were found, otherwise a
  ## quiet electrode at the end of a normal recording would nag on every read.
  hint_trigger_channel <- function(signal_edf, n_events) {
    if (n_events > 0 || !length(signal_edf)) {
      return(invisible(NULL))
    }
    last <- signal_edf[[length(signal_edf)]]
    label <- last$label %||% names(signal_edf)[length(signal_edf)] %||% "the last channel"
    v <- last$signal
    n_distinct <- length(unique(v))
    ## a handful of repeated codes across many samples; more than one, because
    ## a constant channel is dead rather than triggers
    if (n_distinct > 1 && n_distinct / max(length(v), 1L) < 0.01) {
      message_verbose(
        "No events found, but the last channel (",
        encodeString(label, quote = "\""), ") holds only ",
        n_distinct, " distinct values across ", length(v),
        " samples, so it may be a trigger channel.\n",
        "  Use read_edf(..., .trigger_channel = \"last\") to read it as events."
      )
    }
    invisible(NULL)
  }

  channel_info <- dplyr::tibble(
    .channel = channel_names,
    .x = NA_real_, .y = NA_real_, .z = NA_real_,
    .reference = NA_character_
  )
  signal <- new_signal_tbl(
    signal_matrix = signal_dt, .id = s_id,
    .sample = sample_id,
    channels_tbl = channel_info
  )
  if (!is.null(trigger_edf)) {
    ## Report the digital codes, not the physical values.
    ##
    ## EDF stores every sample as a 16 bit integer, the "digital" value, and
    ## the header gives each channel a linear map from that integer to a
    ## physical unit: physicalMin..physicalMax corresponds to
    ## digitalMin..digitalMax. edfReader precomputes the two constants of that
    ## map, so physical = gain * digital + offset, and returns the physical
    ## values.
    ##
    ## That map exists for amplitudes, where "-8191 to 8190 uV" is meaningful.
    ## A trigger channel abuses it: the recording system writes the trigger
    ## code itself as the digital value, and the map then scales it into
    ## whatever unit the channel claims. Here the codes 0, 3072 and 4096 come
    ## back as -0.375, 767.4961 and 1023.4531 uV, which are not microvolts of
    ## anything, just the codes multiplied by 0.2499.
    ##
    ## Inverting the map recovers the code, which is what identifies the
    ## trigger and what you would match on when segmenting.
    codes <- round((trigger_edf$signal - trigger_hdr$offset) / trigger_hdr$gain)
    init_events <- c(1L, which(diff(codes) != 0) + 1L) %>%
      sample_int(.sampling_rate = sampling_rate)
    events <- new_events_tbl(
      .id = 1L,
      .initial = init_events,
      .description = as.character(codes[as.integer(init_events)]),
      .type = "Trigger",
      .final = init_events,
      .channel = NA_character_
    )
  } else if (length(event_channel) == 0) {
    events <- new_events_tbl(.sampling_rate = sampling_rate)
  } else if (event_channel[[1]]$isAnnotation) {
    edf_events <- event_channel[[1]]$annotations
    init_events <- sample_int(round(edf_events$onset * sampling_rate) + 1L, .sampling_rate = sampling_rate)
    events <- new_events_tbl(
      .id = 1L,
      .initial = init_events,
      .type = NA_character_,
      .description = edf_events[["annotation"]],
      ## sample_int + integer keeps the class, but not when both are empty:
      ## an EDF+ file whose annotation channel holds only timekeeping TALs has
      ## no annotations, and `integer(0) + sample_int(integer(0))` comes back
      ## a plain integer, which then fails validation. Re-apply the class.
      .final = sample_int(
        (dplyr::case_when(
          !is.na(edf_events$duration) ~
            round(edf_events$duration * sampling_rate),
          !is.na(edf_events$end) ~
            round((edf_events$end - edf_events$onset + 1) * sampling_rate),
          TRUE ~ 0
        ) %>% as.integer()) + as.integer(init_events),
        .sampling_rate = sampling_rate
      ),
      .channel = NA_character_
    )
  } else {
    ## Biosemi devices trigger codes are encoded in 16-bit format, whereas system
    ## codes (CMS in/out-of range, battery low, etc.) are coded in bits 16-23 of
    ## the status channel (see http://www.biosemi.com/faq/trigger_signals.htm).
    ## To retrieve correct event values (bits 1-16), one could do:

    triggers <- bitwAnd(event_channel[[1]]$signal, (2^16 - 1))

    # for now it only reports growing changes
    init_events <- (which(diff(triggers) > 0) + 1) %>%
      sample_int(.sampling_rate = sampling_rate)


    events <- new_events_tbl(
      .id = 1L,
      .initial = init_events,
      .description = triggers[init_events],
      .type = NA_character_,
      .final = init_events,
      .channel = NA_character_
    )
  }

  segments <- build_segments_tbl(
    .id = seq_len(max(s_id)),
    .recording = .recording
  )

  if (is.null(.trigger_channel)) {
    hint_trigger_channel(signal_edf, nrow(events))
  }

  eeg_lst <- eeg_lst(
    signal = signal, events = events, segments = segments
  )


  built_eeg_lst(eeg_lst, file)
}

#' Read EEGlab set files (Matlab files) into R
#'
#' Creates an eeg_lst object from Matlab exported files. The function reads a .mat or .set file using `R.matlab`. If you do not already have `R.matlab` installed in R, you will need to install it yourself. The  file should have the structure described in this [Data structure article](https://sccn.ucsd.edu/wiki/A05:_Data_Structures). This function is experimental (there are many different formats of eeglab files). If your file cannot be opened please open an issue with a link to the file in [github](https://github.com/bnicenboim/eeguana/issues).
#'
#' @param file A .mat or .set file containing a fieldtrip struct.
#' @param .recording Recording name, by default is the file name.
#' @return An `eeg_lst` object with signal_tbl and event from a Matlab file.
#'
#' @examples
#' \dontrun{
#' s1 <- read_ft("./subject1.set", .recording = 1)
#' }
#'
#' @family reading and writing functions
#'
#'
#' @export
read_set <- function(file, .recording = file) {
  # https://sccn.ucsd.edu/wiki/A05:_Data_Structures
  # dataset in https://sccn.ucsd.edu/wiki/I.1:_Loading_Data_in_EEGLAB

  ## file = "/home/bruno/dev/eeguana/inst/testdata/bv_export_bv_txt_bin_multi_epoched_one.set"
  ## file = system.file("testdata", "EEG01.mat", package = "eeguana")
  ## file = system.file("testdata", "eeglab_data.set", package = "eeguana")
  ## file =  system.file("testdata", "bv_export_bv_txt_bin_multi_epoched_one.set", package = "eeguana")
  require_pkg("R.matlab")

  if (!file.exists(file)) stop(sprintf("File %s not found in %s", file, getwd()))
  message_verbose("# Reading file ", file, "...")

  ## file <- "/home/bruno/dev/eeguana/inst/testdata/eeglab_data.set"

  set <- R.matlab::readMat(file)
  if (length(set) == 1) {
    # nested file:
    set <- set$EEG[, , 1]
  }

  # Tries to set some meta data, but sometimes, some stuff is missing
  srate <- c(set$srate)
  n_trials <- set$trials[1, 1]
  n_chan <- set$nbchan[1, 1]
  ## n_samples <- length(set$times)
  n_samples <- set$pnts[1, 1]
  times <- c(set$times)
  # channels
  chan_set <- struct_to_dt(set$chanlocs)
  if (nrow(chan_set) > 0) {
    if (all(c("sph.radius", "sph.theta.besa", "sph.phi.besa") %in% colnames(chan_set)) &&
      !all(is.na(chan_set[, .(sph.theta.besa, sph.phi.besa)]))) {
      # probably brain vision files
      # eeglab gives the incorrect X, Y, Z for eeglab files
      chan_set[, `:=`(c(".x", ".y", ".z"), spherical_to_xyz_dt(sph.radius, sph.theta.besa, sph.phi.besa))]
    } else if (all(c("sph.radius", "sph.theta", "sph.phi") %in% colnames(chan_set)) &&
      !all(is.na(chan_set[, .(sph.theta, sph.phi)]))) {
      chan_set[, `:=`(c(".x", ".y", ".z"), spherical_to_xyz_dt(sph.radius, sph.theta, sph.phi))]
    } else if (all(c("X", "Y", "Z") %in% colnames(chan_set)) &&
      !all(is.na(chan_set[, .(X, Y)]))) {
      chan_set[, `:=`(c(".x", ".y", ".z"), list(X, Y, Z))]
    } else {
      chan_set[, `:=`(".x" = NA_real_, ".y" = NA_real_, ".z" = NA_real_)]
    }

    data.table::setnames(chan_set, "labels", ".channel")
    # if X,Y,Z are set and are differently from what eeguana found:
    #
    if (!all(is.na(chan_set[, .(X, Y, Z)])) &&
      !identical(
        round(matrix(chan_set[, c(.x, .y, .z)], ncol = 3), 2),
        round(matrix(chan_set[, c(X, Y, Z)], ncol = 3), 2)
      )) {
      warning('There is a mismatch between eeglab channel positions and the ones found by eeguana. To verify that the position of the channels is the correct one, one can plot them as follows:\n
   eeg_obj %>%
  filter(.sample == 1) %>%
  plot_topo() +
  annotate_head() +
  geom_contour() +
  annotate_electrodes(colour = "black")
', call. = TRUE)
    }
  } else {
    chan_set <- NULL
  }


  if (all(unique(dim(set$data)) == 1)) {
    binary <- file.path(dirname(file), set$data[1, 1])
    if (tools::file_ext(binary) == "fdt") {
      # One file that contains metadata (with extension .set, and is a type of MATLAB file), and one file containing raw data (float32 with .fdt extension). The raw data file is organized in samples x channels (so first all the data for one channel, then all the data for a second channel, etc.). In case, there are several trials, the raw data file is organized in samples x trials x channels.

      signal_raw <- read_bin_signal(file = binary, n_chan = n_chan, n_trials = n_trials, sample_x_channels = TRUE)
    } else if (tools::file_ext(binary) == "dat") {
      # transpose format
      signal_raw <- read_bin_signal(binary, n_chan = n_chan, n_trials = n_trials, sample_x_channels = FALSE)
    } else {
      stop(binary, " is an unrecognized file for eeglab format", call. = FALSE)
    }
  } else {
    # to be sure about the n_samples since set$time is sometimes empty
    n_samples <- ncol(set$data)
    if (n_trials == 1) {
      signal_raw <- t(set$data)
    } else {
      # flattening the array into a matrix
      signal_raw <- apply(set$data, 1, c)
    }
  }
  if (!is.null(times)) {
    samples <- rep(as_sample_int(times, .sampling_rate = srate, .unit = "ms"), times = n_trials)
  } else {
    # no times, then start from 0
    samples <- rep(sample_int(seq.int(set$pnts[1, 1]), .sampling_rate = srate), times = n_trials)
  }

  signal_ <- new_signal_tbl(
    .id = rep(seq.int(n_trials), each = n_samples),
    .sample = samples
    # new_sample_int(seq_len(n_samples), sampling_rate = srate)
    , signal_matrix = signal_raw,
    channels_tbl = chan_set
  )
  # events
  ##   In general, fields type, latency, and urevent are always present in the event structure:

  ##     type contains the event type
  ##     latency contains the event latency in data sample unit
  ##     urevent contains the index of the event in the original (= ‘ur’) urevent table (see below).

  ## Other fields like position are user defined and are specific to the experiment.

  ## The user may also define a field called duration (recognized by EEGLAB) for defining the duration of the event (if portions of the data have been deleted, the field duration is added automatically to store the duration of the break (i.e. boundary) event).

  ## If epochs have been extracted from the dataset, another field, epoch, is added to store the index of the data epoch(s) the event belongs to
  events_set <- struct_to_dt(set$event)[, event := 1:.N]
  data.table::setnames(events_set, c("type", "code", "channel"), c(".description", ".type", ".channel"), skip_absent = TRUE)

  if (n_trials == 1) {
    events_set[, `:=`(.id = 1L)]
  } else {
    epochs_set <- struct_to_dt(set$epoch, .id = "epoch")[, .(eventlatency, event, epoch)]
    events_set <- events_set[epochs_set, on = c("epoch", "event")][, latency := NULL]
    data.table::setnames(events_set, c("eventlatency", "epoch"), c("latency", ".id"))
    ## confusingly enough, here latencies are eventlatency which are in "ms" unlike the latency from the set$event which are in samples
    events_set[, latency := as_sample_int(round(latency), .sampling_rate = srate, .unit = "ms")]
  }
  events_set[, `:=`(
    .initial = sample_int(round(latency), .sampling_rate = srate),
    latency = NULL
  )]
  # remove cols with only NAs
  NAcols <- events_set[, names(.SD), .SDcols = sapply(events_set, function(x) all(is.na(x)))]
  if (length(NAcols) > 0) events_set[, (NAcols) := NULL]

  add_event_channel(events_set, chan_set$labels)


  if (!".type" %in% names(events_set)) events_set[, .type := NA_character_]

  if ("duration" %in% names(events_set)) {
    events_set[is.nan(duration), duration := 1]
    events_set[, .final := .initial + duration - 1][, duration := NULL]
  } else {
    events_set[, .final := .initial]
  }

  events_ <- as_events_tbl(events_set)

  segments_ <- build_segments_tbl(seq.int(n_trials), .recording)
  if (n_trials > 1) {
    # only the event that defines the "epoch", and without channel, intiial, final
    segments_ <- update_segments_tbl(segments_, events_[.initial == 1][, c(".channel", ".initial", ".final") := NULL])
  }
  eeg_lst_ <- eeg_lst(signal_tbl = signal_, events_tbl = events_, segments_tbl = segments_)

  built_eeg_lst(eeg_lst_, file)
}



#' Read a FIF file into R
#'
#' Creates an eeg_lst object from EEG/MEG data in FIF format. The function reads a .fif file using the `mne` Python package via `reticulate`.
#' If you do not already have `mne` installed in your Python environment, ensure to install it prior to using this function using [install_py_eeguana].
#' The .fif file is a standard format used by MNE and other neurophysiological data analysis software.
#'
#' @param file A .fif file containing EEG/MEG data.
#' @param .recording Recording name, by default is the file name. This parameter is currently not used in the function but reserved for future use or metadata purposes.
#' @return An `eeg_lst` object with signal_tbl and event information from a .fif file.
#'
#' @examples
#' \dontrun{
#' eeg_data <- read_fiff("./subject1_raw.fif")
#' }
#'
#' @family reading and writing functions
#'
#' @export
read_fif <- function(file, .recording = file) {
  message_verbose("Experimental function. Some properties of the fif files might be lost.")
  require_python()
  raw <- mne$io$read_raw_fif(file)
  as_eeg_lst(raw)
}
