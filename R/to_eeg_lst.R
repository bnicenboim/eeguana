

as_eeg_lst.mne.io.base.BaseRaw <- function(.data){

##create channel info
ch_names <- .data$ch_names
ch_info <- .data$info$chs %>% purrr::map_dfr(~ list(channel= .$ch_name,
                                         ".x" = round(.$loc[[1]]/85,2),
                                         ".y"= round(.$loc[[2]]/85,2),
                                         ".z" =round(.$loc[[3]]/85,2),
                                         unit = "µV",
                                         .reference = NA
                                         ))


signal_m <- .data$to_data_frame()
t_s <- .data$times
sampling_rate <- .data$info$sfreq
samples <- as_sample_int(t_s, sampling_rate)
 
new_signal <- signal_tbl(signal_m, 1L,samples,ch_info)

#create events object
ann <- .data$annotations$`__dict__`
new_events <- events_tbl(.id = 1L,
                         .sample_0 = as_sample_int(ann$onset,sampling_rate),
                         .size = as_sample_int(ann$duration,sampling_rate)-1L,
                         .channel = NA_character_,
                         descriptions_dt = data.table::data.table(description = ann$description))

    eeg_lst(signal = new_signal,
            events= new_events,
            segments = tibble::tibble(.id=1L,recording="recording1", segment=1))


}
