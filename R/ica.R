#' EEG signal decomposition using Independent Component Analysis (ICA)
#'  
#' This function returns an extended `eeg_lst`, `eeg_ica_lst`, with the mixing and unmixing 
#' matrix of every recording. It is possible to visualize the topography
#'  of the components with [plot_components()]. In order to extract the amplitude of
#'   the components with respect to time use [eeg_ica_show()], see examples section. To remove the 
#'   unwanted components, use [eeg_ica_keep()].
#' 
#' It is possible to also use a custom function in the `method` argument. The function should return
#'  a list that with `A` (mixing matrix), consistent with the formulation `X = S %*% A`, where X is matrix
#'   of N_samples by N_channels and/or `W` (unmixing matrix), consistent with the formulation `X %*% W = S`.
#' Some packages with other ICA methods or implementations: `steadyICA` and `ica`.
#' 
#' @param .data An eeg_lst object
#' @param ... Channels to include in ICA transformation. All the channels by default, but eye channels
#' and reference channels should be removed.
#' @param method Methods from different packages: [fast_ICA], a wrapper of [fastICA::fastICA], (default), and some more experimental methods:
#' [fast_ICA2] and [adapt_fast_ICA] adapted from [fICA][fICA::fICA] package. It can also accept a custom function, see details.
#' @param config Other parameters passed in a list to the ICA method. See the documentation of the relevant method.
#' @param ignore Events that should be ignored in the ICA, set to NULL for not using the events table.
#' @family ICA functions
#' @family preprocessing functions
#' @return An eeg_ica_lst object
#' @export
eeg_ica <- function(.data,
                    ...,
                    ignore = .type == "artifact",
                    method = fast_ICA,
                    config = list()) {
  UseMethod("eeg_ica")
}


#' @export
eeg_ica.eeg_lst <- function(.data,
                            ...,
                            ignore = .type == "artifact",
                            method = fast_ICA,
                            config = list()) {
  start_time <- Sys.time()

  ## if(unique(.data$segment$.recording) %>% length() != 1 && !".recording" %in% group_vars(.data)) {
  ##     warning("It seems that there is more than one recording. It may be appropriate to do 'data %>% group_by(recording)' before applying 'eeg_ica()' ")
  ## }

  ## https://www.martinos.org/mne/stable/auto_tutorials/plot_artifacts_correction_ica.html
  ## https://martinos.org/mne/dev/auto_tutorials/plot_ica_from_raw.html
  ## http://www.fieldtriptoolbox.org/example/use_independent_component_analysis_ica_to_remove_eog_artifacts/
  ## method = rlang::quo(fICA::adapt_fICA) # for testing
  ## ignore = rlang::quo(.type == "artifact") # for testing
  method <- rlang::enquo(method)
  dots <- rlang::enquos(...)
  ignore <- rlang::enquo(ignore)

  if (!rlang::quo_is_null(ignore)) {
    signal_raw <-  eeg_events_to_NA(.data, !!ignore,
      all_chs = FALSE,
      entire_seg = FALSE,
      drop_events = FALSE
    ) %>% .$.signal
  } else {
    signal_raw <- .data$.signal
  }
  signal_raw <- signal_raw %>%
    dplyr::select(.id, channel_names(.data))
  ## remove more if dots are used
  if (!rlang::is_empty(dots)) {
    chs <- sel_ch(signal_raw, !!!dots)
    signal_raw <- dplyr::select(signal_raw, c(".id", chs))
  }

  ## creates a DT with length length(signal_tbl) where the grouping var is repeated,
  ## This is used to split the signal_tbl, in case that there are many recordings together
  total <- nrow(signal_raw)
  signal_raw <- signal_raw[stats::complete.cases(signal_raw), ][
    data.table::as.data.table(.data$.segments)
    [, .(.id, .recording)],
    on = ".id",
    nomatch = 0
  ][, .id := NULL][]
  used <- nrow(signal_raw)
 
  #TODO maybe it can be done inside data.table?
  signal_raw <- split(signal_raw, by = ".recording", keep.by = FALSE)

  method_label <- rlang::as_label(method)
  message(paste0("# ICA is being done using ", method_label, "..."))
 if(total > used){
    message("# ",round(used/total,2)*100, "% of the samples will be used.")
  }
  data_out <- function(x) {
    out <- list()
    if (!is.null(x$A)) {
      out$mixing_matrix <- x$A
      out$unmixing_matrix <- MASS::ginv(x$A)
    } else if (!is.null(x$W)) {
      out$mixing_matrix <- MASS::ginv(x$W)
      out$unmixing_matrix <- x$W
    } else {
      out$unmixing_matrix <- x$W
      out$mixing_matrix <- x$A
    }
    out
  }

  ICA_fun <- function(x) {
    ## the method needs to be evaluated in the package env,
    ## and the parameters are passed in alist with do.call
    do.call(rlang::eval_tidy(method), c(list(x), config)) %>% data_out()
  }

  l_ica <- lapply(signal_raw, function(x) {
    ica <- ICA_fun(x)
    # A:
    colnames(ica$mixing_matrix) <- colnames(x)
    rownames(ica$mixing_matrix) <- paste0("ICA", seq_len(nrow(ica$mixing_matrix)))
    # W
    rownames(ica$unmixing_matrix) <- colnames(x)
    colnames(ica$unmixing_matrix) <- paste0("ICA", seq_len(ncol(ica$unmixing_matrix)))
    ica
  })
  .data$ica <- l_ica

  end_time <- Sys.time()
  timing <- end_time - start_time
  message(paste0("# ICA took ", round(timing[[1]], 2), " ", attributes(timing)$units))
  as_eeg_ica_lst(.data)
}
#' Add independent components (or sources) to the signal table for visualization.
#'
#' @param .data An eeg_ica_lst object
#' @param ... Components to extract from the mixing matrix of the ICA transformation.
#' @family ICA functions
#' @export
eeg_ica_show <- function(.data, ...) {
  UseMethod("eeg_ica_show")
}

#' @export
eeg_ica_show.eeg_ica_lst <- function(.data, ...) {
  dots <- rlang::enquos(...)
  comp_sel <- tidyselect::vars_select(component_names(.data), !!!dots)
  signal_raw <- .data$.signal[, c(".id", channel_ica_names(.data)), with = FALSE] %>%
    .[data.table::as.data.table(.data$.segments)
    [, .(.id, .recording)]
    , ,
    on = ".id", nomatch = 0
    ]
  signal_raw[, ".id" := NULL]

  l_signal <- split(signal_raw, by = ".recording", keep.by = FALSE)

  ica_c <- map2_dtr(l_signal, .data$ica, ~ {
    X <- scale(.x, scale = FALSE)
    {tcrossprod(as.matrix(X), t(.y$unmixing_matrix[, comp_sel, drop = FALSE])) *
      ## I make it 10 times larger so that the components can be plot alongside
      ## the channels
       10} %>%
      data.table::as.data.table() %>%
      .[, lapply(.SD, component_dbl)]
  })
  data.table::setnames(ica_c, comp_sel)
  .data$.signal <- cbind(.data$.signal, ica_c)
  class(.data$.signal) <- class(signal_raw)
  validate_eeg_lst(.data)
}

#' Select independent components (or sources) to keep.
#'
#' This function will transform the channels according to the indepent components that are kept or removed.
#'
#' @param .data An eeg_ica_lst object
#' @param ... Components to keep from the mixing matrix of the ICA transformation. See [dplyr::select] and [tidyselect::select_helpers] for details.
#' @family preprocessing functions
#' @family ICA functions
#' @export
eeg_ica_keep <- function(.data, ...) {
  UseMethod("eeg_ica_keep")
}

#' @export
eeg_ica_keep.eeg_ica_lst <- function(.data, ...) {
    dots <- rlang::enquos(...)
    if(all(unique(names(dots))!="")){
        ## in case recording1 = ICA1, ... syntax is used
        comp_sel <- lapply(dots, function(dot) {
            tidyselect::vars_select(component_names(.data), !!dot)
        })
                                        #names and order
        comp_sel <- purrr::imap(.data$ica, ~{
            if(.y %in% names(comp_sel)){
                comp_sel[[.y]]
            } else {
                                        #all
                comp_sel <- colnames(.x$unmixing_matrix)
                names(comp_sel) <- comp_sel
                comp_sel
            }
        }
        )
    } else {
        comp_sel <- purrr::imap(.data$ica, ~ {
            tidyselect::vars_select(component_names(.data), !!!dots)
        })
    }

    .data$ica <- purrr::map2(comp_sel, .data$ica, function(sel, ica) {
        list(
            unmixing_matrix = ica$unmixing_matrix[, sel, drop = FALSE],
            mixing_matrix = ica$mixing_matrix[sel, , drop = FALSE]
        )
    }
)
  chs <- colnames(.data$ica[[1]]$mixing_matrix)

  signal_raw <- .data$.signal[, c(".id", chs), with = FALSE][
    data.table::as.data.table(.data$.segments)
    [, .(.id, .recording)]
    , ,
    on = ".id"
  ][, .id := NULL]

  l_signal <- split(signal_raw, by = ".recording", keep.by = FALSE)

  signal_back <- map2_dtr(l_signal, .data$ica, ~ {
    X <- scale(.x, scale = FALSE)
    proj <- tcrossprod(.y$unmixing_matrix, t(.y$mixing_matrix))
    tcrossprod(X, t(proj)) %>%
      scale(., center = -1 * attr(X, "scaled:center"), scale = FALSE) %>%
      data.table::as.data.table()
  })
  ## Puts back the signal in its original place
  .data$.signal <- data.table::copy(.data$.signal)
  chs_tbl <- channels_tbl(.data$.signal)
  for (ch in colnames(signal_back)) {
    data.table::set(.data$.signal, j = ch, value = channel_dbl(signal_back[[ch]]))
  }
  channels_tbl(.data$.signal) <- chs_tbl
  validate_eeg_lst(.data)
}

#' Transforms an object to a eeg_lst
#'
#' @param .data An `eeg_ica_lst` and experimentally an MNE raw signal using [reticulate::reticulate].
#' @param ... Not in use.
#'
#' @export
as_eeg_lst <- function(.data, ...) {
  UseMethod("as_eeg_lst")
}


#' @export
as_eeg_lst.eeg_ica_lst <- function(.data, ...) {
  eeg_lst(signal_tbl = .data$.signal, events_tbl = .data$.events, segments_tbl = .data$.segments)
}
#' @export
as_eeg_lst.eeg_lst <- function(.data, ...) {
  validate_eeg_lst(eeg_lst)
}
