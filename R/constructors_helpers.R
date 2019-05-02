#' @param signal_tbl 
#'
#' @param events 
#' @param segments 
#'
#' @noRd
new_eeg_lst <- function(signal = NULL, events = NULL, segments = NULL) {
    x <- list(
        signal = signal, events = events,
        segments = segments
    )
    x <- unclass(x)
    structure(x,
              class = c("eeg_lst"),
              vars = character(0)
              )
}



#' @param values 
#'
#' @param sampling_rate 
#'
#' @noRd
new_sample_int <- function(values, sampling_rate) {
  if (!all(is.na(values)) && !all.equal(values, round(values))) {
    stop("Sample integer values should be round numbers.",
      call. = FALSE
    )
  } else {
    values <- as.integer(values)
  }
  values <- unclass(values)
  structure(values,
    class = "sample_int",
    sampling_rate = sampling_rate
  )
}

#' @param .sample_id 
#'
#' @noRd
validate_sample_int <- function(.sample_id) {
  if (!is.integer(.sample_id)) {
    stop("Values should be integers.",
      call. = FALSE
    )
  }
  if (length(.sample_id) > 0) {
    if (attributes(.sample_id)$sampling_rate <= 0) {
      warning("Attribute sampling_rate should be a positive value.",
        call. = FALSE
      )
    }
  }
  .sample_id
}

#' @noRd
new_channel_dbl <- function(values, channel_info = list()) {
  values <- unclass(values) %>% as.double
  attributes(values) <- c(
    class = "channel_dbl",
    channel_info
  )
  values
}



#' @param channel 
#'
#' @noRd
validate_channel_dbl <- function(channel) {
  if (!is.double(channel)) {
    stop("Values should be double.",
      call. = FALSE
    )
  }
  purrr::walk(c(".x", ".y", ".z"), ~
  if (!is.numeric(attr(channel, .))) {
    warning(sprintf("Attribute %s should be a number.", .),
      call. = FALSE
    )
  } )

  # if (is.null(attributes(channel)$.reference)) {
  #   warning("Attribute .reference is missing.",
  #     call. = FALSE
  #   )
  # }
  channel
}


#' @param channels 
#'
#' @param channels_tbl 
#'
#' @noRd
update_channel_meta_data <- function(channels, channels_tbl) {
  if (nrow(channels_tbl) == 0 || is.null(channels_tbl)) {
    channels <- purrr::map(
      channels,
      function(sig) {
        .channel <- new_channel_dbl(values = sig,
                                    channels_tbl = list(.x=NA_real_,
                                                        .y= NA_real_,
                                                        .z =NA_real_,
                                                        .reference=NA_real_))
      }
    )
  } else {
    channels <- purrr::map2(
      channels %>% stats::setNames(make_names(channels_tbl$.channel)),
                  purrr::transpose(dplyr::select(channels_tbl, -.channel)),
      function(sig, chan_info) {
        .channel <- new_channel_dbl(values = sig, as.list(chan_info))
      }
    )
  }
  channels
}

# purrr::map2(
#       channels , purrr::transpose(channels_tbl),
#       function(sig, chan_info) {
#         channel <- new_channel_dbl(value = sig, as.list(chan_info))
#       }
# )


#' @param x 
#'
#' @noRd
validate_eeg_lst <- function(x, recursive = TRUE) {
  if(!is_eeg_lst(x)){
    warning("Class is not eeg_lst", call. = FALSE)
  }

  if(recursive){
    x$signal <- validate_signal_tbl(x$signal)
    x$events <- validate_events_tbl(x$events)
    x$segments <- validate_segments(x$segments)
  }
    diff_channels <- setdiff(x$events$.channel, channel_names(x))
    if (length(diff_channels) != 0 & any(!is.na(diff_channels))) {
        warning("Unknown channel in table of events",
                call. = FALSE
                )
    }
    if (!all.equal(unique(x$signal$.id), unique(x$segments$.id))) {
        warning("The values of .id mismatch between tables.",
                call. = FALSE
                )
    }

    if(any(!group_vars(x) %in% c(colnames(x$signal),colnames(x$segments)))){
        warning("Grouping variables are missing.",
                call. = FALSE
                )
    }
  ## nulls should be caught by the recursive=TRUE
  if(!is.null(sampling_rate(x$events)) && !is.null(sampling_rate(x$signal)) &&
     sampling_rate(x$events)!= sampling_rate(x$signal)){
      warning("Sampling rates in events and signal table are different",
              call. = FALSE)
    }
x
}

#' @param segments 
#'
#' @noRd
validate_segments <- function(segments) {
    if(is.null(segments)) {
        segments <- dplyr::tibble(.id = integer(0))
    }
  if(!is.integer(segments$.id)){
    warning("Column .id of segments table is not an integer.")
  }
   if( length(segments$.id) != length(unique(segments$.id)) ){
     warning("Some .id are repeated in the segments table, there is something wrong going on. Please open an issue with a reproducible example in https://github.com/bnicenboim/eeguana/issues",
      call. = FALSE
    )
  }
  segments
}

#' @param values
#' 
#' @noRd
new_component_dbl <- function(values)  {
    values <- unclass(values) %>% as.double
    attributes(values) <- list(
        class = "component_dbl"
    )
    values
}



#' @param component 
#'
#' @noRd
validate_component_dbl <- function(component) {
    if (!is.double(component)) {
        stop("Values should be double.",
             call. = FALSE
             )
    }
    component
}

#' @param signal_tbl 
#'
#' @param events 
#' @param segments 
#'
#' @noRd
new_ica_lst <- function(signal = NULL, mixing = NULL, events = NULL, segments = NULL) {
    x <- list(
        signal = signal,
        mixing = mixing,
        events = events,
        segments = segments
    )
    x <- unclass(x)
    structure(x,
              class = c("ica_lst","eeg_lst"),
              vars = character(0)
              )
}

#' @param x 
#'
#' @noRd
validate_ica_lst <- function(x) {
    x <- validate_eeg_lst(x)
    x$mixing <- validate_mixing_tbl(x$mixing)
    x
}

#' @param mixing_tbl 
#'
#' @noRd
validate_mixing_tbl <- function(mixing_tbl) {
    
    if (!data.table::is.data.table(mixing_tbl)) {
        warning("'mixing' should be a data.table.",
                call. = FALSE
                )
    }

     if(all(!sapply(mixing_tbl, is_channel_dbl)) && nrow(mixing_tbl)>0){
        warning("No channels found.")
    }
    # Validates channels 
    mixing_tbl[, lapply(.SD,validate_channel_dbl), .SDcols= sapply(mixing_tbl, is_channel_dbl)] 

    mixing_tbl
}

#' Builds a mixing_tbl table.
#'
#' @param mixing_matrix Matrix or table of channels with their mixing.
#' @param ids Integers indicating to which group the row of the mixing matrix belongs.
#' @param sample_ids Vector of integers.
#' @param channel_info A table with information about each channel (such as the one produced by `channels_tbl``)
#' 
#' @return A valid mixing_tbl table.
#' @noRd
new_mixing_tbl <- function(mixing_matrix, means_matrix , groups, channels_tbl) {
    ## if mixing_mat is not a list I convert it to always use the same map
    if(!is.list(mixing_matrix)) mixing_matrix <- list(mixing_matrix)
    if(!is.list(means_matrix))  means_matrix <- list(means_matrix)
    mixing_matrix_dt <-
        map2_dtr( mixing_matrix, means_matrix, function(mixm, meansm)  {
            .ICA <- data.table::data.table(.ICA =c("mean",paste0("ICA",seq_len(nrow(mixm)))))
            mm <- rbind(meansm,mixm) %>% data.table::data.table() %>%
                .[, (update_channel_meta_data(.SD, channels_tbl))] %>%
                cbind(.ICA, .)
            mm
        }
                    ,.id = ".group")
    data.table::setattr(mixing_matrix_dt, "class",c("mixing_tbl",class(mixing_matrix_dt)))
    mixing_matrix_dt[]
}

#' @noRd
as_mixing_tbl <- function(.data,...){
    UseMethod("as_mixing_tbl")
}
#' @noRd
as_mixing_tbl.data.table <- function(.data){
    data.table::setattr(.data, "class", c("mixing_tbl",class(.data)))
    validate_mixing_tbl(.data)
}
#' @noRd
as_mixing_tbl.mixing_tbl <- function(.data){
    validate_mixing_tbl(.data)
}
#' @noRd
as_mixing_tbl.data.frame <- function(.data){
    .data <- data.table::as.data.table(.data)
    as_mixing_tbl(.data)
}
