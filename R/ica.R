#' Apply ICA
#'
#' @param .data 
#' @param ... 
#' 
#' @export
eeg_ica <- function(.data, ...){
    UseMethod("eeg_ica")
}


#' @rdname eeg_ica
#' @param .data 
#' @param ... Channels for the eeg_ica
#' @param method 
#' @param config 
#' 
#' @return An ica_lst object
#' @export
eeg_ica.eeg_lst <- function(.data, 
                    ...,
          method = "fastica",
          config= list() 
                              )
{

    ##https://www.martinos.org/mne/stable/auto_tutorials/plot_artifacts_correction_ica.html
    ##http://www.fieldtriptoolbox.org/example/use_independent_component_analysis_ica_to_remove_eog_artifacts/
    method = stringr::str_to_lower(method)
    default_config = if(method=="infomax") {
                 list(algorithm = "gradient", 
                      fun = "logistic",
                      learning_rate = 0.00065/log(nchannels(.data)),
                      annealing_rate_angle = 60,
                      annealing_rate_step = 0.90,
                      max_iterations = 512)
             } else if(method== "fastica"){
                 list(algorithm = "parallel",
                      fun = "logcosh",
                      alpha = 1.0,
                      tolerance = 1e-04,
                      max_iterations = 512)
             }
    config <- purrr::list_modify(default_config, !!!config)
    ncomponents <- nchannels(.data)
    dots <- rlang::enquos(...)
    removed_channel <- c()

    rep_group <- repeated_group_col(.data)
    signal_raw <- dplyr::select(.data$signal, channel_names(.data))

    if(!rlang::is_empty(dots)){
        purrr::walk(dots, ~if(!rlang::as_label(.x) %in% channel_names  ){
                               stop(.x, " is not a channel",
                                    "Only channels can be selected",
                                    call. = FALSE)
                           } )
        removed_channel <- purrr::map_chr(dots, ~ rlang::as_label())
        signal_raw <- dplyr::select(signal, !!!dots)
    } 

   
    l_signal <- signal_raw %>% split(rep_group)
    channel_means<- l_signal  %>%
        purrr::map(colMeans, na.rm=TRUE)

    if(method == "fastica"){
        l_ica <- purrr::map(l_signal, ~ fastICA::fastICA(.x,
                                          n.comp = ncomponents,
                                          alg.typ = config$algorithm,
                                          fun = config$fun,
                                          alpha = config$alpha,
                                          method = "C", #c("R","C"),
                                          row.norm = FALSE,
                                          maxit = config$max_iterations,
                                          tol = config$tolerance,
                                          verbose = FALSE,
                                          w.init = NULL) %>%
                                          {list(sources = .$S,
                                                ncomponents = nrow(.$W),
                                        #mix mat has a column for each channel, and
                                                #a row for each component
                                                mixing_matrix =  .$A)} )
    }
   
    reconstr <- purrr::map2(l_ica,channel_means, ~  tcrossprod(.x$sources, t(.x$mixing_matrix)) + .y) %>%
        do.call("rbind", .)
    message("Absolute difference between original data and reconstructed from independent sources: ",
            mean(abs(reconstr - as.matrix(signal_raw))) %>% signif(2))


    signal_source_tbl <- l_ica %>%
        map_dtr( ~ .x$sources %>%
                data.table::data.table() %>%
                {.[,lapply(.SD, component_dbl)][]} %>%
                setNames(paste0("ICA",seq_len(ncol(.x$sources)))) 
                ) %>%
        cbind(.data$signal[,c(obligatory_cols$signal,removed_channel), with=FALSE],.)

    mixing_tbl <- mixing_tbl(mixing_matrix = l_ica %>% purrr::transpose() %>% .$mixing_matrix,
                             means_matrix= channel_means,
                             groups = group_chr(.data),
                             channel_info = channels_tbl(signal_raw))
    ica <- ica_lst(signal = signal_source_tbl,
            mixing = mixing_tbl,
            events = .data$events,
            segments = .data$segments)
   ica %>% dplyr::group_by(!!!dplyr::groups(.data))
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

as_eeg_lst.ica_lst <- function(.data, ...){

    rep_group <- repeated_group_col(.data)
    reconstr_signals <- map2_dtr(.data$signal %>%
                split(rep_group),
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
        cbind(reconstr_signals)

    eeg_lst(signal= signal, events= .data$events, segments = .data$segments ) %>%
        dplyr::group_by(!!!dplyr::groups(.data))
}
