#' @export
write_vhdr <- function(x, file, overwrite = FALSE){
  # do by recording
 recs <- unique(x$.segments$.recording)
 if(length(file) ==1 & dir.exists(file)) {
   file <- file.path(file, recs)
 } else  if(missing(file)) {
   file <- recs
 } else if(length(file) ==1 & !dir.exists(file)) {
   file <- paste0(file, "_",recs)
 }
 if(overwrite == FALSE & any(file.exists(file))){
   stop("write_vhdr cannot overwrite file",call. = FALSE)
 }
 for(n in seq_along(recs)){
  xr <- x %>% eeg_filter(.recording==recs[n])
  write_dat(xr, file[n])
  write_vhdr_metadata(xr, file[n])
  write_vmrk(xr, file[n])
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
write_dat <- function(x, file){
  file <- paste0(tools::file_path_sans_ext(file),".dat")
  dat <- file(file, "wb")
  on.exit(close(dat))
  #by default IEEE_FLOAT_32
  bytes <- 32 / 8
  mat_sig <- x$.signal %>% select.(where(is_channel_dbl)) %>% as.matrix()
  apply(mat_sig, MARGIN = 1, writeBin, dat ,size = bytes,useBytes= FALSE)
}


#' @noRd
write_vhdr_metadata <- function(x, file){
  fname <- tools::file_path_sans_ext(basename(file))
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
  write.ini(vhdr, paste0(tools::file_path_sans_ext(file),".vhdr"),"UTF-8")
  
}

#' @noRd
write_vmrk <- function(x, file) {
  fname <- tools::file_path_sans_ext(basename(file))
  ver2 <- paste0("Brain Vision Data Exchange Marker File, Version 2.0\n; Data created with the R package `eeguana` version ", packageVersion("eeguana"),"\n")
  
  chs_n <- as.numeric(factor(x$.events$.channel,levels = channel_names(x)))
  chs_n <- ifelse(is.na(chs_n),0, chs_n)
  # deal with several .ids in events
  #  x$.events %>% split( f= ".id")
  markers <- with(x$.events, paste0(.type,",", .description,",", .initial, ",", .final - .initial + 1,",",chs_n))
  #this should be added only to new segment
  # TO CHECK what happens in epoched datasets
  markers[1] <- paste0(markers[1],",",chr_remove(format(Sys.time(), "%Y%m%d%H%M%OS6"),"\\."))
  
  names(markers) <- paste0("Mk",1:length(markers))
  vmrk <- list("Common Infos" = list(Codepage= "UTF-8", 
                                     DataFile =  paste0(fname,".dat")),
               "Marker Infos" =as.list(markers),
               "Marker User Infos"= list()
  )
  attributes(vmrk)$title <- ver2
  
  write.ini(vmrk, paste0(tools::file_path_sans_ext(file),".vmrk"),"UTF-8")
  
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
