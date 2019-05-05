#' EEG signal decomposition using Independent Component Analysis (ICA)
#'
#' @export
eeg_ica <- function(.data, ...){
    UseMethod("eeg_ica")
}

#' @rdname eeg_ica
#' @param .data An eeg_lst object
#' @param ... Channels to include in ICA transformation. All the channels by default, but eye channels and reference channels should be removed.
#' @param method Methods from different packages: `fICA::adapt_fICA` (default), `fICA::fICA`, `fICA::reloaded_fICA`, `fastICA::fastICA`, or a custom function that returns a list that contains `S`  (reconstucted sources) and `A` (mixing matrix), consistent with the formulation `X= S %*% A`, where X is matrix of N_samples X N_channels or `W` (unmixing matrix), consistent with the formulation `X %*% W = S`.
#' @param config Other parameters passed in a list to the ICA method. These are the default parameters except that when possible the method is run in C rather than in R. See the documentation of the relevant method.
#' @param tolerance Convergence tolerance.
#' @param max_iterations Maximum number of iterations.
#' 
#' @return An ica_lst object
#' @export
eeg_ica.eeg_lst <- function(.data, 
                    ...,
                    method = fICA::adapt_fICA,
                    config= list(),
                    tolerance = 1e-06,
                    max_iterations = 1000
                              ){
  start_time <- Sys.time()
  
    ## if(unique(.data$segment$recording) %>% length() != 1 && !"recording" %in% group_vars(.data)) {
    ##     warning("It seems that there is more than one recording. It may be appropriate to do 'data %>% group_by(recording)' before applying 'eeg_ica()' ")
    ## }

    ##https://www.martinos.org/mne/stable/auto_tutorials/plot_artifacts_correction_ica.html
    ##https://martinos.org/mne/dev/auto_tutorials/plot_ica_from_raw.html
    ##http://www.fieldtriptoolbox.org/example/use_independent_component_analysis_ica_to_remove_eog_artifacts/
    ##method = rlang::quo(fICA::adapt_fICA) # for testing
    method = rlang::enquo(method)
    dots <- rlang::enquos(...)

    ## ## Use id_all only if there are NAs
    ## id_all<- NULL
    ##   if(na.rm == FALSE && anyNA(.data$signal)){
    ##     stop("Missing values in the eeg_lst, use na.rm = TRUE", call. = FALSE)
    ##   } else {
    ##       id_all <- .data$signal %>%
    ##           dplyr::select(-one_of(channel_names(.data)))
    ##   }


  signal_complete <- .data$signal[complete.cases(.data$signal)] %>%
            dplyr::select(.id, channel_names(.data))
    ## cols from signal_tbl that are not channels:
    ## id_signal <- dplyr::select(.data$signal, -one_of(channel_names(.data)))
    ## remove more if dots are used
    if(!rlang::is_empty(dots)){
        ## id_signal_col <- setdiff(colnames(signal_complete),
                                      ## tidyselect::vars_select(colnames(signal_complete), !!!dots))
        ## id_signal <- bind_cols_dt(id_signal,
                                       ## dplyr::select(signal_complete, id_signal_col))
        ## data.table::setkey(id_signal,.id,.sample_id)
       chs <- sel_ch(signal_complete, !!!dots)
        signal_complete <- dplyr::select(signal_complete, c(".id", chs))
    }



    ## creates a DT with length length(signal_tbl) where the grouping var is repeated,
  ## This is used to split the signal_tbl, in case that there are many recordings together
  signal_complete <- signal_complete[data.table::as.data.table(.data$segments)[
                                                    ,.(.id,recording)], on = ".id"][,.id:=NULL]

  l_signal <- split(signal_complete, by = "recording", keep.by = FALSE)

  method_label = rlang::as_label(method)
  message(paste0("# ICA is being done using ", method_label,"..."))
  
    if(method_label== "fastICA::fastICA"){
        default_config <- list(n.comp = nchannels(.data), method="C",maxit =max_iterations,
                               tol = tolerance)
        data_in <- function(x) x
        data_out <- function(x) {
            list(                 ##mix mat has a column for each channel, and
                ##a row for each component
                 unmixing_matrix = x$K %*% x$W,
                 mixing_matrix =  x$A)
        }

    } else if(stringr::str_detect(method_label,"^fICA::")){
        default_config <- list(inR = FALSE, maxiter =max_iterations,
                               eps = tolerance)
        data_in <- function(x) as.matrix(x)
        data_out <- function(x) {
            list(##mix mat has a column for each channel, and
                 ##a row for each component
                 unmixing_matrix = t(x$W),
                 mixing_matrix =  t(MASS::ginv(x$W)))
        }
    } else {
        message("Using custom ICA function: ", method_label)
        default_config <- list()
        data_in <- function(x) x
        data_out <- function(x)      {
            out <- list()
            if(!is.null(x$A)) {
                out$mixing_matrix <- x$A
                out$unmixing_matrix <- MASS::ginv(x$A)
            } else if(!is.null(x$W)){
                out$mixing_matrix = MASS::ginv(x$W)
                out$unmixing_matrix = x$W
            } else {
                out$unmixing_matrix = x$W
                out$mixing_matrix <- x$A
            }
            out
        }
    }

    config <- purrr::list_modify(default_config, !!!config)

  ICA_fun <- function(x){
      ##method needs to be evaluated in the package env, and the parameters are passed in alist with do.call
        do.call(rlang::eval_tidy(method), c(list(data_in(x)), config)) %>% data_out()
    }

  l_ica <- lapply(l_signal, function(x) {
      ica <- ICA_fun(x)
      #A:
      colnames(ica$mixing_matrix) <- colnames(x)
      rownames(ica$mixing_matrix) <- paste0("ICA", seq_len(nrow(ica$mixing_matrix)))
      #W
      rownames(ica$unmixing_matrix) <- colnames(x)
      colnames(ica$unmixing_matrix) <- paste0("ICA", seq_len(ncol(ica$unmixing_matrix)))
      ica
  })


  .data$ica <- l_ica
    
    end_time <- Sys.time()
    timing <- end_time - start_time
    message(paste0("# ICA took ",round(timing[[1]],2)," ",attributes(timing)$units))
   as_eeg_ica_lst(.data) 
}

#' @export
eeg_ica_show <- function(.data, ...){
    UseMethod("eeg_ica_show")
}

#' @export
eeg_ica_show.eeg_ica_lst <- function(.data,...){

    dots <- rlang::enquos(...)
    comp_sel <- tidyselect::vars_select(component_names(.data), !!!dots)
    signal_raw <- .data$signal[data.table::as.data.table(.data$segments)[
                                                   ,.(.id,recording)],
                                    , on = ".id"][,c(".id",".sample_id"):=NULL]

    l_signal <- split(signal_raw, by = "recording", keep.by = FALSE)

   ica_c <-  map2_dtr(l_signal, .data$ica, ~ {
        X<-scale(.x, scale = FALSE)
        tcrossprod(as.matrix(X), t(.y$unmixing_matrix[, comp_sel]))  %>%
            data.table::as.data.table() %>%
            .[,lapply(.SD, component_dbl)]
   })
    data.table::setnames(ica_c, comp_sel)
    .data$signal <- cbind(.data$signal, ica_c)
    class(.data$signal) <- class(signal_raw)
    validate_eeg_lst(.data)
}

#' @export
eeg_ica_keep <- function(.data, ...){
    UseMethod("eeg_ica_keep")
}

#' @export
eeg_ica_keep.eeg_ica_lst <- function(.data, ...){
    dots <- rlang::enquos(...)
    comp_sel <- tidyselect::vars_select(component_names(.data), !!!dots)
    chs <- colnames(.data$ica[[1]]$mixing_matrix)
    

    .data$ica <- lapply(.data$ica, function(ica) {
        list(unmixing_matrix = ica$unmixing_matrix[, comp_sel, drop= FALSE],
             mixing_matrix =  ica$mixing_matrix[comp_sel, , drop = FALSE])
    })



    signal_raw <- .data$signal[,c(".id",chs), with = FALSE][
                            data.table::as.data.table(.data$segments)[
                                              ,.(.id,recording)],
                             , on = ".id"][,.id :=NULL]

    l_signal <- split(signal_raw, by = "recording", keep.by = FALSE)

    signal_back <-  map2_dtr(l_signal, .data$ica, ~ {
        X<-scale(.x, scale = FALSE)
        proj <- tcrossprod(.y$unmixing_matrix, t(.y$mixing_matrix))
        tcrossprod(X, t(proj)) %>%
            scale(., center=-1*attr(X,"scaled:center"), scale = FALSE) %>%
            data.table::as.data.table() 
    })
    ## Puts back the signal in its original place
    .data$signal <- data.table::copy(.data$signal)
    chs_tbl <- channels_tbl(.data$signal)
    for(ch in colnames(signal_back)){
        data.table::set(.data$signal,j= ch, value =  channel_dbl(signal_back[[ch]]))
    }
    channels_tbl(.data$signal) <- chs_tbl
      validate_eeg_lst(.data)
} 

#' to.data object
#'
#' @param .data 
#' @param ... 
#' 
#' @export
as_eeg_lst <- function(.data, ...){
    UseMethod("as_eeg_lst")
}

#' to.data object
#'
#' @param .data 
#' @param ... 
#' 
#' @export
as_eeg_lst.eeg_ica_lst <- function(.data, ...){
    eeg_lst(signal_tbl = .data$signal, events_tbl = .data$events, segments_tbl = .data$segments)
}
#' @export
as_eeg_lst.eeg_lst <- function(.data, ...){
    validate_eeg_lst(eeg_lst)
}

