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
#' @param ... 
#' @param method 
#' @param configuration 
#' 
#' @return An ica_lst object
#' @export
eeg_ica.eeg_lst <- function(.data, 
                    ...,
          method = "fastica",
          config= list(), 
                              )
{
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
    
    purrr::list_modify(default_config, unlist(config))

  ncomponents = nchannels(.data)
  dots <- rlang::enquos(...)
  signal_raw <- dplyr::select(.data$signal, !!!dots)
  group_vars <- group_chr(.data)
    l <- signal_raw %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::one_of(group_vars)) %>%
        dplyr::distinct()

    if(!identical(na.omit(l),l)) { 
        stop("Data cannot be grouped by a column that contains NAs.")
    }
     if (ncol(l) == 0) {
       l_signal_raw <- list(.data)
        } else {
       l_signal_raw <- base::split(.data, l)
        }
       l_signal_raw <- purrr::discard(~nrow(.x) == 0) 


  # rest of the channels and cols of signal for the later reconstruction
  non_ica_signal <- .data$signal[,setdiff(colnames(.data$signal), colnames(signal_raw)), with = FALSE]
  
  if(stringr::str_to_lower(method) %in% c("imax","infomax","im")){

    #     infomax <- ica::icaimax(signal_raw,
    #           nc = ncomponents,
    #            rate = configuration$learning_rate,
    #            rateanneal= c(configuration$annealing_rate_angle,configuration$annealing_rate_step),
    #           maxit = configuration$max_iterations, 
    #           alg  = configuration$algorithm,
    #           fun = configuration$fun)
    # S_ICA <- data.table::as.data.table(infomax$S)
    # ncomponents <- nrow(infomax$W)
    # mixing_matrix <- ica_res$M 
    # unmixing_matrix <- ica_res$W 

  } else if(stringr::str_to_lower(method) %in% c("fi","fastica","fica")){

      l_ica <- purrr::map(l_signal_raw, ~ fastICA::fastICA(.x,
                                  n.comp = ncomponents,
                                  alg.typ = configuration$algorithm,
                                  fun = configuration$fun,
                                  alpha = configuration$alpha,
                                  method = "C", #c("R","C"),
                                  row.norm = FALSE,
                                  maxit = configuration$max_iterations,
                                  tol = configuration$tolerance,
                                  verbose = FALSE,
                                  w.init = NULL)
#FROM HERE
    S_ICA <- ica_res$S
    ncomponents <- nrow(ica_res$W)
    mixing_matrix <- ica_res$A 
    
  }
warning("grouping not implemented!!!!, this might apply ICA to several subjects together")
  component_names <- paste0("C",seq_len(ncomponents))
  
   channel_means<- colMeans(signal_raw)

#col_info instead of channel_info and without any obligatory attribute...
#ica_lst (signal, events, segments, mixing, unmixing with the channels info)

  reconstr <- (S_ICA %*% mixing_matrix + channel_means)
  message("Absolute difference between original data and reconstructed from independent sources: ",
    mean(abs(reconstr - as.matrix(signal_raw))) %>% signif(2))


  S_ICA <- data.table::as.data.table(S_ICA)
  data.table::setnames(S_ICA, component_names)
  S_ICA <- S_ICA[,lapply(.SD,component_dbl)]


  mixing_tbl <- mixing_tbl(mixing_matrix = mixing_matrix,
                           components = component_names,
                           group_name = groupby, #group name
                           groups = group_ids,
                           channel_means = channel_means, 
                           channel_info = channels_tbl(signal_raw))

  ica_lst(signal = cbind(non_ica_signal, S_ICA),
            mixing = mixing_tbl,
            events = .data$events,
            segments = .data$segments)
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

}
