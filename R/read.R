#' Read a BrainVision file into an eegble object.
#'
#' @param file A vhdr file in a folder that contains a .vmrk and .dat files
#' @param ignore_segs Ignore BrainVision segmentation. By default, FALSE.
#' @param sep Segment separation marker. By default: type == "New Segment"
#' @param zero Time zero marker. By default: type == "Time 0"
#' @param recording Recording name, by default is the file name.
#' 
#' @return An \code{eegble} object with signals and event from file_name.dat, 
#' file_name.vhdr, and file_name.vmrk.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
read_vhdr <- function(file, sep= type == "New Segment", zero = type == "Time 0",
                      recording = file) {
  sep <- rlang::enquo(sep)
  zero <- rlang::enquo(zero)
  #zero = quo(type == "Time 0")
  #sep = quo(type == "New Segment")
  
  # Takes the files from the header:
  file_path <- stringr::str_match(file,"(.*(/|\\\\)).")[,2] %>%
  {if(is.na(.)) NULL else .} 
  header_info <- read_vhdr_metadata(file)
  data_file <- header_info$common_info$data_file
  data_ext <- tools::file_ext(data_file)
  # It only accepts .dat files (for now)
  if(data_ext == "dat" || data_ext == "eeg" ){
    vmrk_file <- header_info$common_info$vmrk_file
    events <- read_vmrk(file = paste0(file_path, vmrk_file))
    x <- read_dat(file = paste0(file_path, data_file), 
                     common_info = header_info$common_info,
                     chan_info = header_info$chan_info, 
                     events = events, 
                     recording = recording,
                     sep = sep,
                     zero = zero)
  } else {
    warning(paste0(".",data_ext, " files are unsupported."))
  }
  x
}



#' Helper function to read the dat files directly
#' @importFrom magrittr %>%

read_dat <- function(file, common_info = NULL, chan_info = NULL, events = NULL, 
                      recording, sep, zero) {
  
  n_chan <- nrow(chan_info)
  
  if(common_info$format == "BINARY") {
    samplesize <- dplyr::case_when(
     stringr::str_detect(common_info$bits,stringr::regex("float_32", 
                                                      ignore_case = TRUE)) ~ 4,
     stringr::str_detect(common_info$bits,stringr::regex("int_32", 
                                                      ignore_case = TRUE)) ~ 4,
     stringr::str_detect(common_info$bits,stringr::regex("int_16", 
                                                      ignore_case = TRUE)) ~ 2,
                                                                TRUE ~ NA_real_)
    
    amps <- readBin(file, what = "double", n = file.info(file)$size, 
                                                              size = samplesize)
    byrow <- dplyr::case_when( 
     stringr::str_detect(common_info$orientation,stringr::regex("vector", 
                                                  ignore_case = TRUE)) ~ FALSE,
     stringr::str_detect(common_info$orientation,stringr::regex("multipl", 
                                                  ignore_case = TRUE)) ~ TRUE,
                                                  TRUE ~ NA) %>% 
      { if(is.na(.)) { 
        stop("Orientiation needs to be vectorized or multiplexed.")
      } else{
        .  
      }}
    
    signals <- matrix(as.matrix(amps), ncol = n_chan, byrow = byrow)  %>% 
      tibble::as.tibble() 
  } else if(common_info$format == "ASCII"){
    signals <- readr::read_delim(file, delim = " ",  
                          col_types = 
                          readr::cols(.default = readr::col_double()))
  }
  
  
  colnames(signals)  <- chan_info$labels
  chan_info$labels <- forcats::as_factor(chan_info$labels)


  

  eeg_info <- list(srate = common_info$srate, 
                           reference = NA)
  # name the channels of the corresponding events, using factor  
  events <- dplyr::mutate(events, channel = 
                                 dplyr::if_else(channel == 0, NA_integer_, 
                                                                  channel) %>%  
                                 chan_info$labels[.] %>% 
                                  forcats::as_factor(levels = chan_info$labels))
  
 

  raw_signals <- tibble::tibble(sample = 1:nrow(signals)) %>%
    dplyr::bind_cols(signals)
  
    
    # the first event can't be the end of the segment
    # and the last segment ends at the end of the file     
    end_segs <- events %>% dplyr::filter(!!sep)  %>% 
      dplyr::slice(-1) %>% {.$sample - 1} %>% c(., nrow(signals)) 
    
    beg_segs <- events %>% dplyr::filter(!!sep) %>% .$sample 
    # segs <- list(beg = beg_segs, t0 = t0, end = end_segs)

    s0 <-  events %>% 
      dplyr::filter(!!zero) %>% .$sample

    # In case the time zero is not defined  
    if(length(s0)==0) s0 <- beg_segs
    

    
    data <- purrr::pmap_dfr(list(beg_segs,  s0, end_segs), .id = ".id",
                          function(b, s0, e) raw_signals %>%
                                # filter the relevant samples
                                dplyr::filter(sample >= b, 
                                       sample <= e) %>%
                                # the first sample of a segment is 1
                                dplyr::mutate(sample = sample - s0 + 1L) %>%
                                # order the signals df:
                                dplyr::select(sample, dplyr::everything())) %>% 
                                dplyr::mutate(.id = as.integer(.id))
    seg_events <- 
     purrr::pmap_dfr(list(beg_segs, s0, end_segs), .id = ".id",
                      function(b, s0, e) events %>%
                      # filter the relevant events
                      # started after the segment (b) 
                      # or span after the segment (b)  
                      dplyr::filter(sample >= b | sample + size - 1 >= b, 
                      # start before the end               
                             sample <= e) %>%
                      dplyr::mutate(size = if_else(sample < b, b - size, size), 
                                  sample = case_when(sample >= b ~ sample - s0 + 1L,
                                                     sample < b ~ b - s0 + 1L))) %>% 
                    dplyr::mutate(.id = as.integer(.id))

    seg_info <- tibble::tibble(.id = seq(length(beg_segs)), 
                              recording = recording, segment = .id, 
                              type = "initial")

  
  eegble <- new_eegbl(data = data, events = seg_events, chan_info = chan_info, 
    eeg_info = eeg_info, seg_info = seg_info)
  

  message(paste0("# Data from ", nrow(eegble$seg_info), 
    " segment(s) and ", nchan(eegble), " channels was loaded."))
  message(say_size(eegble))
  eegble
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
    stringr::str_detect("Mk[0-9]*?=") %>% which 
  start <- markers_info_lines %>% min - 1
  end <- markers_info_lines %>% max - start 
  
  col_names = c("Mk_number=Type","description","sample", 
                "size", "channel","date")
  
  events <- suppressWarnings(readr::read_csv(file, col_names = col_names,
                                             col_types = readr::cols(
                                               `Mk_number=Type` = readr::col_character(),
                                               description = readr::col_character(),
                                               sample = readr::col_integer(),
                                               size = readr::col_integer(),
                                               channel = readr::col_integer(),
                                               date = readr::col_double()
                                             ),
                                             skip = start, n_max= end,
                                             trim_ws = TRUE)) %>%
    tidyr::separate(`Mk_number=Type`, 
                    c("mk","type"), sep ="=") %>%
    dplyr::mutate(mk = as.numeric(stringr::str_remove(mk,"Mk"))) %>%
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
                                          dotall = TRUE, multiline=TRUE) ) %>% .[[1]]
  
  read_metadata <- function(tag, vhdr = content_vhdr) {
    info <- vhdr[vhdr[,2] == tag,3]
    if(length(info) == 0){
      return(tibble::tibble(type = character(), value = character()))
    } else {
      return(readr::read_delim(info,
                               delim = "=", comment = ";", col_names=c("type", "value")))
    }
  }
  
  
  out <- list()
  
  
  channel_info <- read_metadata("Channel Infos") %>% 
    tidyr::separate(value, c("labels","ref","res","unit"), sep=",", fill="right")
  coordinates <- read_metadata("Coordinates") %>% 
    tidyr::separate(value, c("radius","theta","phi"), sep=",", fill="right")%>%
    readr::type_convert(col_types = 
                      readr::cols(radius = readr::col_integer(),
                      theta = readr::col_integer(),
                      phi = readr::col_integer()))
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
      SamplingInterval = readr::col_double())) %>%
    dplyr::transmute(data_points = DataPoints,
                     # seg_data_points = as.numeric(SegmentDataPoints),
                     orientation = DataOrientation,
                     format = DataFormat,
                     domain = DataType,
                     srate =  1000000 / SamplingInterval,
                     data_file = DataFile,
                     vmrk_file = MarkerFile) 
  
  if(common_info$format == "ASCII") {
    format_info <- read_metadata("ASCII Infos") %>% 
      tidyr::spread(type, value)  %>%
      readr::type_convert(col_types = readr::cols(
        DecimalSymbol = readr::col_character(),
        SkipColumns  = readr::col_integer(),
        SkipLines = readr::col_integer()))
  } else if(common_info$format == "BINARY") {
    format_info <- read_metadata("Binary Infos") %>% 
      tidyr::spread(type, value) %>% 
        dplyr::rename(bits = BinaryFormat)
  }
  
  
  common_info  <- dplyr::bind_cols(common_info, format_info)
  
  if(stringr::str_sub(common_info$domain,1,nchar("time")) %>% 
     stringr::str_to_lower() != "time") {
    stop("DataType needs to be 'time'")
  }
  
  chan_info <- dplyr::full_join(channel_info, coordinates, by = "type") %>% 
    dplyr::mutate(x = radius * sin(phi*2*pi/360) * cos(theta*2*pi/360),
            y = radius * sin(phi*2*pi/360) * sin(theta*2*pi/360),
            z = radius * cos(phi*2*pi/360)) %>%
    dplyr::select(labels, theta, phi, radius,  x, y, z)
  
  out <- list()
  out$chan_info <- chan_info
  out$common_info <- common_info
  return(out)
}


#' @export
eegble  <- function(data, events, chan_info, eeg_info, seg_info) {
  validate_eegbl(new_eegbl(data, events, chan_info, eeg_info, seg_info))
}
