#' EEG signal decomposition using Independent Component Analysis (ICA)
#'
#' @export
eeg_ica <- function(.data, ...){
    UseMethod("eeg_ica")
}

#' @rdname eeg_ica
#' @param .data An eeg_lst object
#' @param ... Channels to include in ICA transformation. All the channels by default, but eye channels and reference channels should be removed.
#' @param method Methods from different packages: `fICA::adapt_fICA` (default), `fICA::fICA`, `fICA::reloaded_fICA`, `fastICA::fastICA`, or a custom function that returns a list that contains `S`  (reconstucted sources) and `A` (mixing matrix), consistent with the formulation `X=A %*% S` or `W` (unmixing matrix), consistent with the formulation `X %*% W = S` (i.e., W == solve(X)).
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
                    max_iterations = 1000,
                    na.rm = FALSE
                              ){
  start_time <- Sys.time()
  
    if(unique(.data$segment$recording) %>% length() != 1 && !"recording" %in% group_vars(.data)) {
        warning("It seems that there is more than one recording. It may be appropriate to do 'data %>% group_by(recording)' before applying 'eeg_ica()' ")
    }

    ##https://www.martinos.org/mne/stable/auto_tutorials/plot_artifacts_correction_ica.html
    ##https://martinos.org/mne/dev/auto_tutorials/plot_ica_from_raw.html
    ##http://www.fieldtriptoolbox.org/example/use_independent_component_analysis_ica_to_remove_eog_artifacts/
    ##method = rlang::quo(fICA::adapt_fICA) # for testing
    method = rlang::enquo(method)
    dots <- rlang::enquos(...)

    ## Use id_all only if there are NAs
    id_all<- NULL
      if(na.rm == FALSE && anyNA(.data$signal)){
        stop("Missing values in the eeg_lst, use na.rm = TRUE", call. = FALSE)
      } else {
          id_all <- .data$signal %>%
              dplyr::select(-one_of(channel_names(.data)))
      }

    .data$signal <- .data$signal[complete.cases(.data$signal)]
    signal_complete <- dplyr::select(.data$signal, channel_names(.data))
    ## cols from signal_tbl that are not channels:
    id_signal <- dplyr::select(.data$signal, -one_of(channel_names(.data)))
    ## remove more if dots are used
    if(!rlang::is_empty(dots)){
        id_signal_col <- setdiff(colnames(signal_complete),
                                      tidyselect::vars_select(colnames(signal_complete), !!!dots))
        id_signal <- bind_cols_dt(id_signal,
                                       dplyr::select(signal_complete, id_signal_col))
        data.table::setkey(id_signal,.id,.sample_id)
        signal_complete <- dplyr::select(signal_complete, !!!dots)
    }



    ## creates a DT with length length(signal_tbl) where the grouping var is repeated,
    ## This is used to split the signal_tbl, in case that there are many recordings together
    rep_group <- repeated_group_col(.data)


    if(nrow(rep_group)==0){
        l_signal <- list(signal_complete)
    } else {
        l_signal <- signal_complete %>% split(rep_group)
    }

    channel_means <- l_signal  %>%
        purrr::map(colMeans, na.rm=na.rm)

  
    method_label = rlang::as_label(method)
  message(paste("# ICA is being done using", method_label,"..."))
  
    if(method_label== "fastICA::fastICA"){
        default_config <- list(n.comp = nchannels(.data), method="C",maxit =max_iterations,
                               tol = tolerance)
        data_in <- function(x) x
        data_out <- function(x) {
            list(sources = x$S,
                 ##mix mat has a column for each channel, and
                 ##a row for each component
                 mixing_matrix =  x$A)
        }

    } else if(stringr::str_detect(method_label,"^fICA::")){
        default_config <- list(inR = FALSE, maxiter =max_iterations,
                               eps = tolerance)
        data_in <- function(x) as.matrix(x)
        data_out <- function(x) {
            list(sources = x$S,
                 ##mix mat has a column for each channel, and
                 ##a row for each component
                 mixing_matrix =  t(MASS::ginv(x$W)))
        }
    } else {
        message("Using custom ICA function: ", method_label)
        default_config <- list()
        data_in <- function(x) x
        data_out <- function(x)      {
            out <- list()
            out$sources = x$S
            if(!is.null(x$A)) {
                out$mixing_matrix = x$A
            } else {
                out$mixing_matrix = MASS::ginv(x$W)
            }
            out
        }
    }

    config <- purrr::list_modify(default_config, !!!config)
    
    ICA_fun <- function(x){
      ##method needs to be evaluated in the package env, and the parameters are passed in alist with do.call
        do.call(rlang::eval_tidy(method), c(list(data_in(x)), config)) %>% data_out()
    }

    l_ica <- purrr::map(l_signal, ICA_fun)

    reconstr <- purrr::map2(l_ica,channel_means, ~  tcrossprod(.x$sources, t(.x$mixing_matrix)) + rep(.y, nrow(.x$sources)) ) %>%
        do.call("rbind", .)
    rel_diff <- signif(mean(abs(reconstr - as.matrix(signal_complete)),na.rm=na.rm)/mean(abs(as.matrix(signal_complete)),na.rm=na.rm) * 100,2)
    if(rel_diff>5){
        warning("The mean relative difference between the original data and the reconstructed from independent sources is too large: ",rel_diff, "%. Have you excluded EOG and reference channels? If so, try increasing `max_iterations`.")
    } else{
        message("Mean relative difference between the original data and the reconstructed from independent sources: ",rel_diff, "%.")
                    }
    signal_source_tbl <- l_ica %>%
        map_dtr( ~ .x$sources %>%
                     data.table::data.table() %>%
                {.[,lapply(.SD, component_dbl)][]} %>%
                setNames(paste0("ICA",seq_len(ncol(.x$sources)))) 
                ) %>%
        bind_cols_dt(id_signal,.)
    data.table::setkey(signal_source_tbl,.id,.sample_id)

    ## if the data had NA, put back the missing samples
    if(!is.null(id_all)){
        signal_source_tbl <- signal_source_tbl[id_all, ]
    }        ## rbind(id_all, fill = TRUE)

    mixing <- new_mixing_tbl(mixing_matrix = l_ica %>% purrr::transpose() %>% .$mixing_matrix,
                         means_matrix= channel_means,
                         groups = group_vars(.data),
                         channels_tbl = channels_tbl(signal_complete)) %>%
        validate_mixing_tbl
    ica <- ica_lst(signal = signal_source_tbl,
                   mixing = mixing,
                   events = .data$events,
                   segments = .data$segments) %>% dplyr::group_by(!!!dplyr::groups(.data))
    
    end_time <- Sys.time()
    timing <- end_time - start_time
    message(paste0("# ICA took ",round(timing[[1]],2)," ",attributes(timing)$units))
    
    message(say_size(ica))
    ica
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

#' @export
as_eeg_lst.ica_lst <- function(.data, ...){
   rep_group <- repeated_group_col(.data)
   if(length(rep_group)==0){
       l_signal <- list(.data$signal)
   } else {
       l_signal <- .data$signal %>%
           split(rep_group)
   }
    reconstr_signals <- map2_dtr(l_signal,
                .data$mixing %>%
                split(.data$mixing$.group), function(s,m){

                    source <- dplyr::select_if(s,is_component_dbl) %>% as.matrix()
                    ## the mixing matrix won't be the huge one,
                    ## the code above could be optimized bu it should be fine:
                    mixing_matrix <- m[.ICA!="mean",] %>%
                        dplyr::select_if(is_channel_dbl)  %>%
                        as.matrix
                    means <- m[.ICA=="mean",] %>%
                        dplyr::select_if(is_channel_dbl)  %>%
                        data.table::data.table()

                    ## source %*% mixing_matrix + means  
                    prod <- data.table::data.table(tcrossprod(source,t(mixing_matrix)))

                    means[rep(1,nrow(source)), purrr:::map2(.SD,prod, ~.x+.y)]
                })
    signal <- .data$signal %>% dplyr::select_at(vars(-one_of(component_names(.data)))) %>%
        bind_cols_dt(reconstr_signals)

    eeg_lst(signal_tbl= signal, events_tbl= .data$events, segments_tbl = .data$segments ) %>%
        dplyr::group_by(!!!dplyr::groups(.data))
}


#' @export
as_mixing_matrix_lst <- function(.data){
    UseMethod("as_mixing_matrix_lst")
}


#' @export
as_unmixing_matrix_lst <- function(.data){
    UseMethod("as_unmixing_matrix_lst")
}

#' @export
as_mixing_matrix_lst.ica_lst <- function(.data, ...){
 
    purrr::map(.data$mixing %>%
                    split(.data$mixing$.group), function(m){

                    ## the mixing matrix won't be the huge one,
                    ## the code above could be optimized bu it should be fine:
                    m[.ICA!="mean",] %>%
                        dplyr::select_if(is_channel_dbl)  %>%
                        as.matrix
                })
}


#' @export
as_unmixing_matrix_lst.ica_lst <- function(.data, ...){
    
    purrr::map(.data$mixing %>%
               split(.data$mixing$.group), function(m){

                   ## the mixing matrix won't be the huge one,
                   ## the code above could be optimized bu it should be fine:
                   m[.ICA!="mean",] %>%
                       dplyr::select_if(is_channel_dbl)  %>%
                       as.matrix %>% MASS::ginv()
               })
}
