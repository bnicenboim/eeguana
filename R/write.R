write_vhdr_metadata <- fuction(x, file){
  fname <- tools::file_path_sans_ext(basename(file))
  ver2 <- "Brain Vision Data Exchange Header File Version 2.0"
  chs <- channel_names(x)
  unit <- ifelse(channels_tbl(x)$unit == "microvolt", "\u00b5V", channels_tbl(x)$unit) 
  resolution <- ifelse(is.na(channels_tbl(x)$resolution),"",as.character(resolution)) 
  ch_info <- paste0(chs,",,", resolution ,",",unit)
  names(ch_info) <- paste0("Ch",1:length(chs))
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
               "Channel Infos" = list(ch_info),
               "Channel User Infos" = list(),
               "Coordinates" 
  )
  ini::write.ini(vhdr, paste0(fname,".vhdr"),"UTF-8")
  
}

#' @noRd
xyz_to_spherical <- function(.x, .y, .z) {
  radius <- ifelse(anyNA(c(.x, .y ,.z)), 0, round(sqrt(.x^2+.y^2+.z^2),1) )
  theta <- acos(.z/radius) / (pi *180)
  #https://en.wikipedia.org/wiki/Spherical_coordinate_system 
  x <- ifelse(radius != 0, round(sin(theta * pi / 180) * cos(phi * pi / 180), 2), NA_real_)
  y <- ifelse(radius != 0, round(sin(theta * pi / 180) * sin(phi * pi / 180), 2), NA_real_)
  z <- ifelse(radius != 0, round(cos(theta * pi / 180), 2), NA_real_)
  dplyr::tibble(.x = x, .y = y, .z = z)
}
