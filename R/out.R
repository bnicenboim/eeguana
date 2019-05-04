#' Display information of the eeg_lst object.
#'
#' * `nchannels()`: Returns the number of channels.
#' * `channel_names()`: Returns a vector with the name of the channels.
#' * `nchannels()`: Returns the number of channels.
#' * `nsamples()`: Returns the number of samples of the recording (or segments).
#' * `summary()`: Prints a summary of the eeg_lst object. (It also returns a list.)
#'
#' @param x An eeg_lst object.
#' 
#' @family summarize
#' 
#' @name summary
NULL
# > NULL

#' @rdname summary
#' @export
channel_names <- function(x, ...) {
  UseMethod("channel_names")
}
#' @rdname summary
#' @export
channel_names.signal_tbl <- function(x, ...) {
NextMethod()
  }
#' @rdname summary
#' @export
channel_names.default <- function(x, ...) {
  colnames(x)[x[,purrr::map_lgl(.SD, is_channel_dbl )]]
}
#' @rdname summary
#' @export
channel_names.eeg_lst <- function(x, ...) {
  channel_names(x$signal)
}
#
#' @rdname summary
#' @export
channel_ica_names <- function(x, ...) {
  UseMethod("channel_ica_names")
}
#' @rdname summary
#' @export
channel_ica_names.eeg_ica_lst <- function(x, ...) {
rownames(x$ica[[1]]$unmixing_matrix)
  }

#' @rdname summary
#' @export
nchannels <- function(x, ...) {
  UseMethod("nchannels")
}


#' @rdname summary
#' @export
nchannels.default <- function(x, ...) {
    channel_names(x) %>% length()
}

####
#' @rdname summary
#' @export
component_names <- function(x, ...) {
  UseMethod("component_names")
}
#' @rdname summary
#' @export
component_names.eeg_ica_lst <- function(x, ...) {
  colnames(x$ica[[1]]$unmixing_matrix)
}
#' @rdname summary
#' @export
component_names.default <- function(x, ...) {
  stop("Component names can only be extracted after running `eeg_ica()` on an eeg_lst object.", call. = FALSE)
}

#' @rdname summary
#' @export
ncomponents <- function(x, ...) {
  UseMethod("ncomponents")
}


#' @rdname summary
#' @export
ncomponents.eeg_lst <- function(x, ...) {
    component_names(x) %>% length()
}

sampling_rate <- function(x, ...) {
  UseMethod("sampling_rate")
}
sampling_rate.eeg_lst <- function(x) {
  attributes(x$signal$.sample_id)$sampling_rate
}
sampling_rate.signal_tbl<- function(x) {
  attributes(x$.sample_id)$sampling_rate
}
sampling_rate.events_tbl<- function(x) {
  attributes(x$.initial)$sampling_rate
}

duration <- function(x) {
  x$signal %>%
    dplyr::group_by(.id) %>%
    dplyr::summarize(duration = (max(.sample_id) - min(.sample_id)) /
      sampling_rate(x)) %>%
    .$duration
}

#' @rdname summary
#' @export
nsamples <- function(x, ...) {
  UseMethod("nsamples")
}

#' @rdname summary
#' @export
nsamples.eeg_lst <- function(x, ...) {
  duration(x) * sampling_rate(x)
}


#' Summary of eeg_lst information.
#'
#' @param object An eeg_lst object.
#' @inheritParams base::summary
#' @rdname summary
#'
#' @export
summary.eeg_lst <- function(object, ...) {
  summ <- list(
    sampling_rate = sampling_rate(object),
    segments = object$segments %>%
      dplyr::count(recording) %>%
      dplyr::rename(segment_n = n) %>% data.table::data.table(),
    events = object$events %>%
      dplyr::group_by_at(dplyr::vars(-.final, -.channel, -.initial, -.id)) %>%
      dplyr::count() %>% data.table::data.table(),
    size = utils::capture.output(print(utils::object.size(object), units = "auto")),
    duration= format(.POSIXct(nrow(object$signal) / sampling_rate(object) ,tz="GMT"), "%H:%M:%S")
  )
  class(summ) <- c("eeg_summary", class(summ))
  summ
}


#' @export
summary.eeg_ica_lst <- function(object, ...) {
    summ <- NextMethod()
    summ$ica_cor <- eeg_ica_cor_lst(object)

    class(summ) <- c("ica_summary", class(summ))
    summ
}


#' @export
eeg_ica_cor_lst <- function(.data,...){
    UseMethod("eeg_ica_cor_lst")
}

#' @export
eeg_ica_cor_lst.eeg_ica_lst <- function(.data,...){
    eogs <- channel_names(.data)[channel_names(.data) %>%
                                 stringr::str_detect(stringr::regex("eog", ignore_case = TRUE))]
    names(eogs) <- eogs
    comps <- object %>% eeg_ica_show(component_names(.)) 

    lapply(eogs, function(eog) 
        comps$signal %>%
        dplyr::summarize_at(component_names(comps),
                            ~ cor(x=., y = comps$signal[[eog]], use = "complete")) %>%
        t() %>%
        {dplyr::tibble(.ICA = rownames(.), cor= .[,1])} %>%
        dplyr::arrange(desc(abs(cor))))

}

#' @rdname summary
#' @export
print.ica_summary <- function(x, ...) {
    NextMethod()
    cat_line("")
    cat_line("# ICA data:")
    cat_line("")
    cat_line("# Correlations with EOG channels:")
    print(x$ica_cor,...)
    invisible(x)
}


#' @rdname summary
#' @export
print.eeg_summary <- function(x, ...) {
  cat_line(paste0("# EEG data:"))
  cat(paste0("# Sampling rate: ", x$sampling_rate, " Hz.\n"))
  cat(paste0("# Size in memory: ", x$size, ".\n"))
  cat(paste0("# Total duration: ", x$duration, ".\n"))
  cat("# Summary of segments\n")
  x$segments %>%
    print(.,...)

  cat("# Summary of events\n")

  x$events %>%
    print(.,...)

  invisible(x)
}

#' @export
print.eeg_lst <- function(x, ...){
  cat_line("# EEG data:")
  if(length(dplyr::group_vars(x)) >0 ){
    cat_line("# Grouped by: ", paste0(dplyr::group_vars(x), sep = ", "))
  }
  cat_line("")
cat_line("# Signal table:")
  print(x$signal,...)

  cat_line("")
   cat_line("# Events table:")
  if(nrow(x$events)>0){
    print(x$events,...)
  } else {
    cat_line("No events.")
  } 
  
  cat_line("")
  cat_line("# Segments table:")
  print(x$segments,...)
  invisible(x)
}

#' @export
print.eeg_ica_lst <- function(x, ...){
    cat_line("# EEG data:")
    if(length(dplyr::group_vars(x)) >0 ){
        cat_line("# Grouped by: ", paste0(dplyr::group_vars(x), sep = ", "))
    }
    cat_line("")
    cat_line("# Signal table:")
    print(x$signal,...)

    cat_line("")
    cat_line("## Events table:")
    if(nrow(x$events)>0){
        print(x$events,...)
    } else {
        cat_line("No events.")
    } 
    cat_line("")
    cat_line("# ICA:" )
    cat_line(paste0("# Component_names: ICA1...", component_names(x)[ncomponents(x)]))
    cat_line(paste0("# Channels_used: ", paste0(channel_ica_names(x), collapse=", ")))
    cat_line("")
    cat_line("# Segments table:")
    print(x$segments,...)
    invisible(x)
}


#' Count number of complete segments of an eeg_lst object.
#'
#' @param x An `eeg_lst` object.
#' @param ... Variables to group by.
#'
#'
#' @return A tbl.
#'
#' @importFrom magrittr %>%
#' @family summarize
#'
#' @examples
#' \dontrun{
#'
#' faces_segs_some %>% count_complete_cases(recording, description)
#' }
#' @family summarize
#' @export
count_complete_cases_tbl <- function(x, ...) {
  UseMethod("count_complete_cases_tbl")
}
#' @export
count_complete_cases_tbl.eeg_lst <- function(x, ...) {
  dots <- rlang::enquos(...)

  x$signal %>%
    dplyr::group_by(.id) %>%
    dplyr::filter_at(
      dplyr::vars(dplyr::one_of(channel_names(x))),
      dplyr::all_vars(all(!is.na(.)))
    ) %>%
    dplyr::summarize() %>%
    dplyr::semi_join(x$segments, ., by = ".id") %>%
    dplyr::select(-.id, -segment) %>%
    dplyr::count(!!!dots)
}
