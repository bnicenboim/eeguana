#' update eeg_lst from prev version
#' @noRd
update_eeg_lst <- function(.data){

if(!data.table::is.data.table(.data$.segments)){
      message_verbose("Object created with an old version of eeguana.")
      message_verbose("Use as_eeg_lst(object) to convert it to the new format.")
      message_verbose("This message will be converted into a warning in future versions.")
      # message_verbose("(.by_reference = TRUE won't work for segment table)")
      return(as_eeg_lst(.data))
    }
  .data
}
