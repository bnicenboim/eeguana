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
#' @param sep Segment separation marker. By default: .type == "New Segment"
#' @param zero Time zero marker. By default: .type == "Time 0"
#' @param .recording Recording name (file name, by default).
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
#' faces_list <- purrr::map(list.files("./","vhdr"), ~ 
#'     read_vhdr(.x)
#' )
#' faces <- bind(faces_list)
#' }
#'  
#' @family reading functions
#'
#' @export
read_vhdr <- function(file, sep = .type == "New Segment", zero = .type == "Time 0",
                      .recording = file) {
  message("Reading file ", file, "..." )  
  if (!file.exists(file)) stop(sprintf("File %s not found in %s",file, getwd()))
  sep <- rlang::enquo(sep)
  zero <- rlang::enquo(zero)
  # sep = rlang::quo(.type == "New Segment")
  # zero = rlang::quo(.type == "Time 0")

  # Takes the files from the header:
  file_path <- stringr::str_match(file, "(.*(/|\\\\)).")[, 2] %>% {
    if (is.na(.)) NULL else .
  }
  
  
  header_info <- read_vhdr_metadata(file)
  
  # header_info <- tryCatch(read_vhdr_metadata(file),
  #       error=function(cond) {
  #           message(paste("Error in the metadata of:", file))
  #           message(paste(cond,"\n"))
  #           return(NA)
  #       },
  #       warning=function(cond) {
  #           message(paste("Warning in the metadata of:", file))
  #           message(paste(cond,"\n"))
  #           return(NULL)
  #       })
   if(is.null(header_info)){
   stop("Header info in ", file, " could not be read.", call. = FALSE)
 }
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
      .recording = .recording,
      sep = sep,
      zero = zero
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
#' @param layout A .mat [layout from Fieldtrip](http://www.fieldtriptoolbox.org/template/layout)
#' @return An `eeg_lst` object with signal_tbl and event from a Matlab file.
#' 
#' @examples 
#' \dontrun{
#' s1 <- read_ft("./subject1.mat", layout = "easycapM25.mat", .recording = 1)
#' }
#' 
#' @family reading functions
#' 
#'
#' @export
read_ft <- function(file, layout = NULL, .recording = file) {
  # to avoid no visible binding for global variable
  type <- NULL
  
  require_pkg("R.matlab")

  # It should be based on this:
  # http://www.fieldtriptoolbox.org/reference/ft_datatype_raw

  if (!file.exists(file)) stop(sprintf("File %s not found in %s",file, getwd()))

  mat <- R.matlab::readMat(file)

  channel_names <- mat[[1]][, , 1]$label %>% unlist()
  # fsample seems to be deprecated, but I can't find the sampling rate anywere
  if(!is.null(mat[[1]][, , 1]$fsample)){
    sampling_rate <- mat[[1]][, , 1]$fsample[[1]]  
  } else {
    #if fsample is not here, I reconstruct the sampling rate from the difference between time steps
    sampling_rate <- mean(1/diff(mat[[1]][, , 1]$time[[1]][[1]][1,]))
  }
  
  ## signal_raw df:
  

  # sample <- purrr::pmap(slengths, ~
  # seq(..3 + 1, length.out = ..2 - ..1 + 1)) %>%
  #   unlist() %>%
  #   new_sample_int(sampling_rate = sampling_rate)

  sample <- mat[[1]][, , 1]$time %>% purrr::map(~ unlist(.x) * sampling_rate) %>% 
            unlist()  %>%
            new_sample_int(sampling_rate = sampling_rate)
  
  
  signal_raw <- map_dtr(mat[[1]][, , 1]$trial,
    function(lsegment) {
      lsegment[[1]] %>% t() %>% data.table::as.data.table()
    },
    .id = ".id"
  ) 
  signal_raw[,.id := as.integer(.id)]
  

  # channel info:
  channels <- dplyr::tibble(
    .channel = make_names(channel_names)
  )

  if (!is.null(layout)) {
    chan_layout <- R.matlab::readMat(layout) %>%
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


  # colnames(signal_tbl) <- c(".id", channel_names)
  # signal_tbl <- dplyr::mutate(signal_tbl, .sample = sample, .id = as.integer(.id)) %>%
  #   dplyr::select(.id, .sample, dplyr::everything())
  signal_tbl <- new_signal_tbl(signal_matrix = dplyr::select(signal_raw, -.id),
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
  if(!is.null(mat[[1]][, , 1]$cfg[, , 1]$event) && !is.null(mat[[1]][, , 1]$cfg[, , 1]$trl)){
    # # segment lengths, initial, final, offset
    slengths <- mat[[1]][, , 1]$cfg[, , 1]$trl %>%
      apply(., c(1, 2), as.integer) %>%
      data.table::as.data.table()
  ## events df:
  events <- mat[[1]][, , 1]$cfg[, , 1]$event[, 1, ] %>%
    t() %>%
    dplyr::as_tibble(.name_repair = "unique") %>%
    dplyr::select(-offset) %>%
    dplyr::mutate_all(as_first_non0) %>%
    dplyr::rename(duration = dplyr::matches("duration"), .initial = sample, .type = type, .description = value) %>%
    dplyr::mutate(.final = .initial + duration -1, .id = 1L) %>% 
    dplyr::select(-duration) %>%
    add_event_channel(channel_names)
    segmentation <- data.table::data.table(.lower = slengths$V1,
                                           .first_sample = slengths$V1 - slengths$V3,
                                           .upper= slengths$V2)

      segmentation[,.new_id := seq_len(.N)][, .id := 1]
     events <- update_events(as_events_tbl(events,sampling_rate), segmentation)
  } else {
    events <- NULL 
  }
  
  segments <- build_segments_tbl(.id= seq_len(max(signal_tbl$.id)),.recording)
  

  
  
  if(!is.null(mat[[1]][, , 1]$trialinfo)){
    segments <- segments %>% dplyr::bind_cols(dplyr::as_tibble(mat[[1]][, , 1]$trialinfo))
  }

  eeg_lst <- eeg_lst(
    signal = signal_tbl, events = events, segments = segments
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
  validate_eeg_lst(eeg_lst)
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
#' \dontrun{s1 <- read_edf("./faces.edf", .recording = 1)}
#' 
#' @family reading functions
#'
#' @export
read_edf <- function(file, .recording = file) {

# samples Whether to subset the reading; by default starting from sample 1  until the end of the recording.
#less samples doesn't speed up reading data, for now I'm hiding it:
  samples = c(1, Inf)

  if (!file.exists(file)) stop(sprintf("File %s not found in %s",file, getwd()))
  
  header_edf <- edfReader::readEdfHeader(file)  
  if(is.null(.recording) || is.na(.recording)) {
    .recording <- header_edf$patient 
    if(.recording== "" ||  is.null(.recording) || is.na(.recording)) {
      stop("Patient information is missing.")
    }
  }
  sampling_rate <-  header_edf$sHeaders$sRate %>%
    .[!is.na(.)] %>%
    unique()
  if(length(sampling_rate)>1) {
    stop("Channels with different sampling rates are not supported.")
  }

  # so that includes the last sample as well
  times <- sample_int(c(samples[1], samples[2] + 1), sampling_rate =sampling_rate) %>%
    as_time()

  signal_edf <- edfReader::readEdfSignals(header_edf, from = times[1], till = times[2], simplify = FALSE)

  non_signal <- purrr::map_lgl(signal_edf, ~ .x$isAnnotation |
                                           tolower(.x$label) %in% c("status", "trigger") |
                                           tolower(.x$name) %in% c("status", "trigger"))

  if(sum(non_signal)>=2){
    warning("eeguana cannot deal with more than one annotation or status. It will use the first one.\n If you have a file like that, please open an issue in ", url_issues, " with a link to the offending file")
  }
  event_channel <- signal_edf[non_signal]
  channel_names <- header_edf$sHeaders$label[!non_signal]

  signal_edf[non_signal] <- NULL

  signal_dt <- lapply_dtc(signal_edf, function(x) x$signal)

  if(header_edf$isContinuous && all(purrr::map_lgl(signal_edf, ~.x$isContinuous)) ) {
    s_id <- rep(1L,nrow(signal_dt))
    sample_id <- sample_int(seq_len(nrow(signal_dt)),sampling_rate = sampling_rate)
  } else {
    stop("Non continuous edf/bdf files are not supported yet.")
  }

  channel_info <- dplyr::tibble(.channel=   channel_names, 
                                .x = NA_real_, .y = NA_real_, .z = NA_real_,
                                .reference = NA_character_)
  signal <- new_signal_tbl(signal_matrix = signal_dt,.id = s_id,
                      .sample = sample_id, 
                      channels_tbl = channel_info)
  if(length(event_channel)==0){
      events <- new_events_tbl(sampling_rate = sampling_rate)
  } else if (event_channel[[1]]$isAnnotation){

    edf_events <- event_channel[[1]]$annotations
    desc <- data.table::data.table(.type = NA, .description = edf_events[["annotation"]] )
    init_events <- sample_int(round(edf_events$onset * sampling_rate) + 1L , sampling_rate = sampling_rate)
    events <- new_events_tbl(.id=1L, 
                             .initial = init_events,
                             descriptions_dt = desc, 
                             .final = ( dplyr::case_when(!is.na(edf_events$duration) ~
                                                           round(edf_events$duration* sampling_rate),
                                                         !is.na(edf_events$end) ~
                                                           round((edf_events$end - edf_events$onset + 1)* sampling_rate),
                                                         TRUE ~ 0) %>% as.integer() ) +init_events,
                             .channel= NA_character_)
  } else {
    ## Biosemi devices trigger codes are encoded in 16-bit format, whereas system
    ## codes (CMS in/out-of range, battery low, etc.) are coded in bits 16-23 of
    ## the status channel (see http://www.biosemi.com/faq/trigger_signals.htm).
    ## To retrieve correct event values (bits 1-16), one could do:

    triggers <- bitwAnd(event_channel[[1]]$signal, (2^16 -1))

    # for now it only reports growing changes
    init_events <- (which(diff(triggers) > 0) + 1) %>%
      sample_int(sampling_rate = sampling_rate)
    
    desc <- data.table::data.table(.type = NA, .description = triggers[init_events] )
    events <- new_events_tbl(.id=1L,
                             .initial = init_events,
                             descriptions_dt = desc,
                             .final = init_events,
                             .channel= NA_character_)



  }
   
    segments <- build_segments_tbl(
    .id = seq_len(max(s_id)),
    .recording = .recording
  )
   
  eeg_lst <- eeg_lst(
    signal = signal, events = events, segments = segments
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
