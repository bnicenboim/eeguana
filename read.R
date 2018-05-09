
read_vhdr <- function(file, verbose = TRUE,...) {
   # Takes the files from the header:
    file_path <- str_match(file,"(.*(/|\\\\)).")[,2] %>%
                 {if(is.na(.)) NULL else .} 
    header_info <- read_vhdr_metadata(file)
    data_file <- header_info$common_info$data_file
    data_ext <- tools::file_ext(data_file)
    # It only accepts .dat files (for now)
    if(data_ext == "dat" || data_ext == "eeg" ){
      vmrk_file <- header_info$common_info$vmrk_file
      srate  <- header_info$common_info$srate
      events <- read_vmrk(paste0(file_path, vmrk_file))
      data <- read_dat(file = paste0(file_path, data_file), 
                        chan_info = header_info$chan_info, 
                        srate = srate, 
                        orientation = header_info$common_info$orientation, 
                        format = header_info$common_info$format,
                        events = events, 
                        .id = file,
                        verbose = verbose)
    } else {
      warning(paste0(".",data_ext, " files are unsupported."))
    }  
}

read_dat <- function(file, chan_info = NULL, 
                   srate, orientation, format, events = NULL, .id = file, verbose = verbose) {
  
  n_chan <- nrow(chan_info)
  samplesize <- case_when(
  					str_detect(format, regex("float_32", ignore_case = TRUE)) ~ 4,
  					str_detect(format, regex("int_32", ignore_case = TRUE)) ~ 4,
  					str_detect(format, regex("int_16", ignore_case = TRUE)) ~ 2,
  					TRUE ~ NA_real_)

  bin <- readBin(file, what = "double", n = file.info(file)$size, size = samplesize)

  byrow <- case_when( 
  				str_detect(orientation, regex("vector", ignore_case = TRUE)) ~ FALSE,
  				str_detect(orientation, regex("multipl", ignore_case = TRUE)) ~ TRUE,
  				TRUE ~ NA) %>% 
  				{ if(is.na(.)) { 
					stop("Orientiation needs to be vectorized or multiplexed.")
				 } else{
				   . 	
				 }}

  signals <- matrix(bin, ncol = n_chan, byrow = byrow)  %>% 
                tibble::as.tibble() 


  colnames(signals)  <- chan_info$labels
  
  # time <- tibble::tibble(sample = 1:dim(signals)[[1]]) %>%
  #             dplyr::mutate(time = (sample - 1) / srate)
  # data <- eeg_data(data =   signals, srate = srate,
  #                    chan_info = chan_info,
  #                    events = event_table, timings = timings,
  #                    continuous = TRUE)

  eegble <- list(data = NULL, chan_info = NULL, gral_info = NULL)
  
  eegble$chan_info <- chan_info
  eegble$gral_info <- list(srate = srate, 
               		 	   reference = NA)
  eegble$data <- tibble::tibble(sample = 1:dim(signals)[[1]]) %>%
  					left_join(events, by = "sample") %>%
              dplyr::transmute(time = (sample - 1) / srate, event, .id = .id) %>% 
              bind_cols(signals) %>% 
              group_by(`.id`)

  eegble <- unclass(eegble)
  class(eegble) <- "eegbl"
  if(verbose) print(paste("Object size",capture.output(pryr::object_size(eegble))))
  eegble
}



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
              "size_sample", "channel","date")

  tibble_vmrk <- suppressWarnings(readr::read_csv(file, col_names = col_names,
                    col_types = readr::cols(
                                `Mk_number=Type` = readr::col_character(),
                                description = readr::col_character(),
                                sample = readr::col_integer(),
                                size_sample = readr::col_integer(),
                                channel = readr::col_integer(),
                                date = readr::col_double()
                                ),
                    skip = start, n_max= end,
                    trim_ws = TRUE)) %>%
                  tidyr::separate(`Mk_number=Type`, 
                            c("mk","type"), sep ="=") %>%
                  dplyr::mutate(mk = as.numeric(stringr::str_remove(mk,"Mk")))

  events <- tibble_vmrk %>% 
                  dplyr::transmute(event = as_factor(description), sample) %>% 
                  dplyr::filter(!is.na(event))
  
  # segs <- tibble_vmrk %>%  dplyr::transmute(
  #                           bounds = ifelse(type == "Stimulus", NA, type), sample) %>%
  #                 dplyr::filter(!is.na(bounds))                   
  
  return(events)              
}


read_vhdr_metadata <- function(file) {

  content_vhdr <- readr::read_file(file) %>% 
            stringr::str_match_all(stringr::regex("([A-Za-z ]*?)\\](.*?)(\\[|\\Z)", 
              dotall = TRUE, multiline=TRUE) ) %>% .[[1]]

  read_metadata <- function(tag, vhdr = content_vhdr) {
   info <- vhdr[vhdr[,2] == tag,3]
   if(length(info) == 0){
    return(tibble::tibble(type=character(), value=character()))
   } else {
   return(readr::read_delim(info,
                 delim = "=", comment = ";", col_names=c("type", "value")))
   }
 }  
  out <- list()
  binary_info <- read_metadata("Binary Infos") %>% 
                 tidyr::spread(type, value) %>% rename(format = BinaryFormat)
  channel_info <- read_metadata("Channel Infos") %>% 
              separate(value, c("labels","ref","res","unit"), sep=",", fill="right")
  coordinates <- read_metadata("Coordinates") %>% 
                separate(value, c("radius","theta","phi"), sep=",", fill="right")
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
                            domain = DataType,
                            srate =  1000000 / SamplingInterval,
                            data_file = DataFile,
                            vmrk_file = MarkerFile) %>% 
                dplyr::bind_cols(binary_info)
                        
  if(stringr::str_sub(common_info$domain,1,nchar("time")) %>% 
      stringr::str_to_lower() != "time") {
    stop("DataType needs to be 'time'")
  }

  chan_info <- dplyr::full_join(channel_info, coordinates, by = "type") %>% 
                dplyr::mutate(type = "EEG", sph.theta = NA, sph.phi = NA, 
                        sph.radius = NA, urchan = NA,X=NA,Y=NA,Z=NA) %>%
                dplyr::select(labels, type, theta, radius, X, Y, Z, sph.theta,
                 sph.phi, sph.radius, urchan, ref)

  out <- list()
  out$chan_info <- chan_info
  out$common_info <- common_info
  return(out)
}


read_edf <- function(file, ...) {
  
}