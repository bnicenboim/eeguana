#' @noRd 
detect_minmax <- function(x, args = list(win_sample=NULL, difference=NULL)) {
    rmin <- RcppRoll::roll_minr(x,n=args$win_sample, na.rm =TRUE)
    rmax <- RcppRoll::roll_maxr(x,n=args$win_sample, na.rm = TRUE)
    abs(rmin-rmax) >= args$difference
}
#' @noRd 
detect_step <- function(x, args =list(difference=NULL)){
     c(NA, abs(diff(x)) > args$difference)
}
#' @noRd 
search_artifacts <- function(signal,..., fun, args = list()){
    dots <- rlang::enquos(...)
    if(rlang::is_empty(dots)) {
        ch_sel <- channel_names(signal)
    } else {
        ch_sel <- tidyselect::vars_select(channel_names(signal), !!!dots)
    }
    signal[,c(list(.sample_id = .sample_id),
              lapply(.SD,fun, args)),
           .SDcols = (ch_sel), by = .id]

}

#' add events from a table similar to signal, but with TRUE/FALSE depending if an artifact was detected.
#' @noRd 
add_intervals_from_artifacts <- function(old_events, artifact_found, sample_range, type){
    events_found <-artifact_found %>%
        split(.,.$.id) %>%
        map_dtr( function(.eeg)
            .eeg %>% dplyr::select(- dplyr::one_of(obligatory_cols[["signal"]])) %>%
            imap_dtr( ~{
                if(all(.x[!is.na(.x)]==FALSE)){
                  out <- events_tbl()  
                    out[,.id:=NULL][]  ## I need to remove .id because it gets added by map  
                } else {
                    ## left and right values of the window of bad values (respecting the min max samples)
                    left <-  .eeg$.sample_id[.x] + sample_range[[1]]
                    ## the smallest sample is one less than the present one because diff() in artifact_found reduces the vector by 1
                    left[left < (min(.eeg$.sample_id)-1L)] <- (min(.eeg$.sample_id)) 
                    right <-  .eeg$.sample_id[.x] + sample_range[[2]]
                    right[right > max(.eeg$.sample_id)] <- max(.eeg$.sample_id)
                    ##merge if there are steps closer than the window for removal 
                    intervals <- data.table::data.table(start=left, stop=right) %>%
                      na.omit() %>%
                      .[, .(start=min(start), stop=max(stop)),
                       by=.(group=cumsum(c(1, tail(start, -1) > head(stop, -1))))] 
                    data.table::data.table(type = "artifact",
                                           description=type,
                                           .sample_0 = intervals$start,
                                           .size = intervals$stop + 1L - intervals$start,
                                           .channel = .y)
                }
            }),.id = TRUE
            )
    events_found[,.id:= as.integer(.id)]
    new_events <- rbind(events_found, old_events, fill = TRUE)
    
    data.table::setorder(new_events,.id, .sample_0, .channel)
    new_events
}


