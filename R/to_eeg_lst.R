
#' @export
as_eeg_lst.mne.io.base.BaseRaw <- function(.data, ...) {

   units_list <- c(
    "mol/m^3", "hertz", "newton", "pascal", "joule", "watt", "coulomb",
    "volt", "farad", "ohm",
    "S", "weber", "tesla", "henry", "celsius", "lumen", "lux"
  ) %>%
    stats::setNames(c(10, 101:116) %>% as.character()) %>%
    as.list()
  prefix <- c("", "deci", "centi", "milli", "micro", "nano") %>%
    stats::setNames(c(0, -1:-3, -6, -9) %>% as.character())
  
  
  ## create channel info
  ch_names <- .data$ch_names
  ## Meaning of kind code: https://github.com/mne-tools/mne-python/blob/2a0a55c6a795f618cf0a1603e22a72ee8e879f62/mne/io/constants.py
  ## FIFF.FIFFV_BIO_CH       = 102
  ## FIFF.FIFFV_MEG_CH       =   1
  ## FIFF.FIFFV_REF_MEG_CH   = 301
  ## FIFF.FIFFV_EEG_CH       =   2
  ## FIFF.FIFFV_MCG_CH       = 201
  ## FIFF.FIFFV_STIM_CH      =   3
  ## FIFF.FIFFV_EOG_CH       = 202
  ## FIFF.FIFFV_EMG_CH       = 302
  ## FIFF.FIFFV_ECG_CH       = 402
  ## FIFF.FIFFV_MISC_CH      = 502
  ## FIFF.FIFFV_RESP_CH      = 602  # Respiration monitoring
  ## FIFF.FIFFV_SEEG_CH      = 802  # stereotactic EEG
  ## FIFF.FIFFV_SYST_CH      = 900  # some system status information (on Triux systems only)
  ## FIFF.FIFFV_ECOG_CH      = 902
  ## FIFF.FIFFV_IAS_CH       = 910  # Internal Active Shielding data (maybe on Triux only)
  ## FIFF.FIFFV_EXCI_CH      = 920  # flux excitation channel used to be a stimulus channel
  ## FIFF.FIFFV_DIPOLE_WAVE  = 1000  # Dipole time curve (xplotter/xfit)
  ## FIFF.FIFFV_GOODNESS_FIT = 1001  # Goodness of fit (xplotter/xfit)
  ## FIFF.FIFFV_FNIRS_CH = 1100 # Functional near-infrared spectroscopy
  # SI derived units
  #
  ## FIFF.FIFF_UNIT_MOL_M3 = 10  # mol/m^3
  ## FIFF.FIFF_UNIT_HZ  = 101  # hertz
  ## FIFF.FIFF_UNIT_N   = 102  # Newton
  ## FIFF.FIFF_UNIT_PA  = 103  # pascal
  ## FIFF.FIFF_UNIT_J   = 104  # joule
  ## FIFF.FIFF_UNIT_W   = 105  # watt
  ## FIFF.FIFF_UNIT_C   = 106  # coulomb
  ## FIFF.FIFF_UNIT_V   = 107  # volt
  ## FIFF.FIFF_UNIT_F   = 108  # farad
  ## FIFF.FIFF_UNIT_OHM = 109  # ohm
  ## FIFF.FIFF_UNIT_MHO = 110  # one per ohm
  ## FIFF.FIFF_UNIT_WB  = 111  # weber
  ## FIFF.FIFF_UNIT_T   = 112  # tesla
  ## FIFF.FIFF_UNIT_H   = 113  # Henry
  ## FIFF.FIFF_UNIT_CEL = 114  # celsius
  ## FIFF.FIFF_UNIT_LM  = 115  # lumen
  ## FIFF.FIFF_UNIT_LX  = 116  # lux
  ##                                     #
  ##                                     # Others we need
  ##                                     #
  ## FIFF.FIFF_UNIT_T_M   = 201  # T/m
  ## FIFF.FIFF_UNIT_AM    = 202  # Am
  ## FIFF.FIFF_UNIT_AM_M2 = 203  # Am/m^2
  ## FIFF.FIFF_UNIT_AM_M3 = 204 # Am/m^3

  rel_ch <- .data$info$chs %>% purrr::discard(~ .x$kind == 3)
  sti_ch_names <- .data$info$chs %>%
    purrr::keep(~ .x$kind == 3) %>%
    purrr::map_chr(~ .x$ch_name)
  if (length(sti_ch_names) > 0) {
    warning("Stimuli channels will be discarded. Use find_events from mne to add them.", call. = FALSE)
  }

  scale_head <- purrr::map_dbl(rel_ch, ~ sqrt(sum((0 - .x$loc[1:3])^2))) %>%
    .[. != 0] %>% # remove the ones that have all 0
    #1 by default if there is no electrode info
    {. %||% 0} %>%
    min(na.rm = TRUE)

  ch_info <- rel_ch %>% purrr::map_dfr(function(ch) {
    if (ch$kind == 502 | scale_head == 0) { # misc channel
      location <- rep(NA_real_, 3)
    } else {
      location <- purrr::map(ch$loc[1:3], ~ round(. / scale_head, 2))
    }
    
    if (ch$unit$numerator %in% as.numeric(names(units_list))) {
      ch_unit <-  ch$unit$numerator
    } else {
      ch_unit <- 107 # default to Volts
    }
    pref_id <- round(log10(ch$range)) %>% as.character()
    if(pref_id %in% names(prefix)){
      unit <- paste0(
        prefix[[pref_id]],
        units_list[[ch_unit %>% as.character()]]
      )
    } else {
      warning("Unit cannot be identified", call. = FALSE)
      unit <-NA 
    }
    list(
      .channel = ch$ch_name,
      .x = location[[1]],
      .y = location[[2]],
      .z = location[[3]],
      unit = unit,
      .reference = NA_character_
    )
  })

  signal_m <- .data$to_data_frame()
  data.table::setDT(signal_m)
  if (length(sti_ch_names) > 0) {
    signal_m[, (sti_ch_names) := NULL]
  }

  t_s <- .data$times
  samples <- as_sample_int(c(t_s), sampling_rate = .data$info$sfreq, unit = "s")

  new_signal <- new_signal_tbl(.id = 1L, .sample = samples, signal_matrix = signal_m, channels_tbl = ch_info)

  # create events object
  ann <- .data$annotations$`__dict__`
  if (length(ann$onset) == 0) {
    new_events <- new_events_tbl(
      .initial =
        sample_int(integer(0), sampling_rate = .data$info$sfreq),
      .final =
        sample_int(integer(0), sampling_rate = .data$info$sfreq)
    )
  } else {
    new_events <- new_events_tbl(
      .id = 1L,
      .initial = ann$onset %>%
        as_sample_int(sampling_rate = .data$info$sfreq, unit = "s"),
      .final = as_sample_int(ann$onset + ann$duration, sampling_rate = .data$info$sfreq, unit = "s") - 1L,
      .channel = NA_character_,
      descriptions_dt = tidyr::separate(data.table::data.table(annotation = ann$description),
        col = "annotation", into = c(".type", ".description"), sep = "/", fill = "left"
      )
    )
  }
  data_name <- toString(substitute(.data))
  eeg_lst(
    signal_tbl = new_signal,
    events_tbl = new_events,
    segments_tbl = dplyr::tibble(.id = 1L, .recording = data_name, segment = 1L)
  )

}
