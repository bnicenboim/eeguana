
#' Handle Missing Values in eeg_lst objects
#'
#' Remove all NAs from an `eeg_lst` signal table.
#'
#' @param object an `eeg_lst`.
#' @param ... 	further arguments special methods could require.
#' @export
na.omit.eeg_lst <- function(object, ...){
  object$.signal <- na.omit(object$.signal)
  if (!is.null(object$.events) && nrow(object$.events) > 0) {
    range_s <- object$.signal[, .(.lower = min(.sample), .upper = max(.sample)), by = .id]
    object$.events <- update_events(object$.events, range_s)
  }
  object$.segments <- semi_join.(object$.segments, object[[1]], by = ".id")
  object
}

#' Write an eeg_lst object to BrainVision file(s) (experimental)
#'
#' The function will write each recording in a different file.
#'
#' @param x `eeg_lst` object.
#'
#' @param file Either a vector of file names (with the same length as the number of recordings), one file name (which will be appended the recording name if needed), or a path which will use the recording names as file names. By default, it will use the recording name in the current path as file names.
#' @param overwrite Stop writing if the file exists.
#'
#' @export
write_vhdr <- function(x, file, overwrite = FALSE){
  nr <- nrow(x$.signal)
  # remove NAs
  x <- na.omit(x)
  nr_diff <- nr - nrow(x$.signal)
  if(nr_diff>0) warning(nr_diff, " samples were removed because they contained NAs.", call. = FALSE)
 recs <- unique(x$.segments$.recording)
 if(length(file) ==1 & dir.exists(file)) {
   file <- file.path(file, recs)
 } else  if(missing(file)) {
   file <- recs
 } else if(length(file) ==1 & !dir.exists(file)) {
   file <- paste0(file, "_",recs)
 }

 for(n in seq_along(recs)){
  xr <- x %>% eeg_filter(.recording == recs[n]) %>%
    eeg_unsegment()
  message_verbose("Saving file '",tools::file_path_sans_ext(file[n]),"{.dat,.vhdr,.vmrk}'...")
  write_dat(xr, file[n], overwrite = overwrite)
  write_vhdr_metadata(xr, file[n], overwrite = overwrite)
   # make it work when it is segmented
   write_vmrk(xr, file[n], overwrite = overwrite)
 }
}

# split.eeg_lst <- function(x, f, drop = FALSE,
#                           by, sorted = FALSE, keep.by = TRUE, flatten = TRUE,
#                           ...) {
#   split(x$.segments, f = f, drop = drop,
#         by = by, sorted = sorted, keep.by = keep.by, flatten = flatten,
#         ..., verbose = getOption("eeguana.verbose"))
#   split(x$.segments, by = ".recording")
#   split(x$.segments, by = c(".id",".recording"), flatten = FALSE)
#   
# }

#' @noRd
write_dat <- function(x, file, overwrite= FALSE){
  file <- paste0(tools::file_path_sans_ext(file),".dat")
  if(file.exists(file) & overwrite==FALSE) stop("The file '",file, "' already exists.", call. = FALSE)
  dat <- file(file, "wb")
  on.exit(close(dat))
  #by default IEEE_FLOAT_32
  bytes <- 32 / 8
  mat_sig <- x$.signal %>% select.(where(is_channel_dbl)) %>% as.matrix()
  apply(mat_sig, MARGIN = 1, writeBin, dat ,size = bytes,useBytes= FALSE)
}


#' @noRd
write_vhdr_metadata <- function(x, file, overwrite= FALSE){
  fname <- tools::file_path_sans_ext(basename(file))
  file <- paste0(tools::file_path_sans_ext(file),".vhdr")
  if(file.exists(file) & overwrite==FALSE) stop("The file '",file, "' already exists.", call. = FALSE)
  ver2 <- paste0("Brain Vision Data Exchange Header File Version 2.0\n; Data created with the R package `eeguana` version ", packageVersion("eeguana"),"\n")
  chs <- channel_names(x)
  chs_n<- paste0("Ch",1:length(chs))
  unit <- ifelse(channels_tbl(x)$unit == "microvolt", "\u00b5V", channels_tbl(x)$unit) 
  resolution <- ifelse(is.na(channels_tbl(x)$resolution),"",as.character(resolution)) 
  ch_info <- paste0(chs,",,", resolution ,",",unit)
  names(ch_info) <- chs_n
  coordinates <- xyz_to_spherical_dt(.x = channels_tbl(x)$.x, .y=channels_tbl(x)$.y, .z = channels_tbl(x)$.z ) %>% apply(1, paste, collapse = "," ) 
  names(coordinates) <- chs_n

  vhdr <- list("Common Infos" = list(Codepage= "UTF-8", 
                                     DataFile =  paste0(fname,".dat"),
                                     MarkerFile = paste0(fname,".vmrk"),
                                     DataFormat = "BINARY",
                                     DataOrientation="MULTIPLEXED",
                                     DataType="TIMEDOMAIN",
                                     NumberOfChannels = nchannels(x),
                                     DataPoints = nsamples(x),
                                     SamplingInterval = 1000000 / as.double(sampling_rate(x))),
               "User Infos" = list(),
               "Binary Infos" = list(BinaryFormat="IEEE_FLOAT_32"),
               "Channel Infos" = as.list(ch_info),
               "Channel User Infos" = list(),
               "Coordinates" = as.list(coordinates)
  )
  attributes(vhdr)$title <- ver2
  attributes(vhdr$Coordinates)$comments <- c("Each entry: Ch<Channel number>=<Radius>,<Theta>,<Phi>")
  attributes(vhdr$`Channel Infos`)$comments <-c("Each entry: Ch<Channel number>=<Name>,<Reference channel name>,",
  "<Resolution in \"Unit\">,<Unit>, Future extensions...",
  "Fields are delimited by commas, some fields might be omitted (empty).",
  "Commas in channel names are coded as \"\\1\".")
  write.ini(vhdr, file,"UTF-8")
  
}

#' @noRd
write_vmrk <- function(x, file, overwrite= FALSE) {
  fname <- tools::file_path_sans_ext(basename(file))
  file <- paste0(tools::file_path_sans_ext(file),".vmrk")
  if(file.exists(file) & overwrite==FALSE) stop("The file '",file, "' already exists.", call. = FALSE)
  ver2 <- paste0("Brain Vision Data Exchange Marker File, Version 2.0\n; Data created with the R package `eeguana` version ", packageVersion("eeguana"),"\n")
  
  chs_n <- as.numeric(factor(x$.events$.channel,levels = channel_names(x)))
  chs_n <- ifelse(is.na(chs_n),0, chs_n)
  
  markers <- with(x$.events, paste0(.type,",", .description,",", .initial, ",", .final - .initial + 1,",",chs_n))
  
  for(n in seq_along(markers)){
    if (chr_detect(markers[n],"New Segment")) {
      markers[n] <- paste0(markers[n],",",chr_remove(format(Sys.time(), "%Y%m%d%H%M%OS6"),"\\."))
    }
  }
  
  
  names(markers) <- paste0("Mk",1:length(markers))
  vmrk <- list("Common Infos" = list(Codepage= "UTF-8", 
                                     DataFile =  paste0(fname,".dat")),
               "Marker Infos" =as.list(markers),
               "Marker User Infos"= list()
  )
  attributes(vmrk)$title <- ver2
  write.ini(vmrk, file,"UTF-8")
}
  


#' @noRd
xyz_to_spherical_dt <- function(.x, .y, .z) {
  radius <- round(sqrt(.x^2+.y^2+.z^2),1)
  radius <- ifelse(is.na(radius),0, radius)
  theta <- round((acos(.z/radius) / pi)* 180 * sign(.x + 1e-10),0)
  theta <- ifelse(is.na(theta), 0, theta)
  phi <- round(atan(.y/.x) /pi * 180,0)
  phi <- ifelse(is.na(phi), 0, phi)

  #https://en.wikipedia.org/wiki/Spherical_coordinate_system 
  data.table::data.table(radius = radius, theta=theta, phi = phi)
}
