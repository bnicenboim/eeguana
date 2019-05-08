#' eeguana: A package for flexible manipulation of EEG data.
#'
#' The package *eeguana* provides a framework for doing simple pre-processing
#' with specialized functions (starting with `eeg_` and `ch_`) and manipulating
#' EEG data with *dplyr* verbs (e.g., `mutate`, `filter`, `summarize`) extended to a
#' new class `eeg_lst`, and `ggplot` wrapper functions.  The new class is inspired by
#' tidyverse principles but it's not really "tidy" (due to space considerations),
#' it's a list of (i) a wide table that contains the signal amplitudes at every sample
#' point of the EEG,  (ii) an events table with information about markers (or triggers),
#' blinks and other exported information, and (iii) a long table with experimental information,
#' such as participant (`recording`), conditions, etc.  
#'
#' @docType package
#' @name eeguana
NULL
# > NULL
