#' update eeg_lst from prev version
#' @noRd
update_eeg_lst <- function(.data){

if(!data.table::is.data.table(.data$.segments)){
      message("Object created with an old version of eeguana.")
      message("Use as_eeg_lst(object) to convert it to the new format.")
      message("This message will be converted into a warning in future versions.")
      if(.by_ref) message(".by_reference=TRUE won't work for segment table")
      return(as_eeg_lst(.data))
    }
  .data
}
