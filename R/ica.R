#' Apply ICA
#'
#' @param .eeg_lst 
#' @param ... 
#' 
#' @export
eeg_ica <- function(.eeg_lst, ...){
    UseMethod("eeg_ica")
}


#' @rdname eeg_ica
#' @param .eeg_lst 
#' @param ... 
#' @param method 
#' @param configuration 
#' 
#' @return An ica_lst object
#' @export
eeg_ica.eeg_lst <- function(.eeg_lst, 
                    ...,
          method = "infomax",
          configuration= 
            if(method=="infomax") {
              list(algorithm = "gradient",
              fun = "logistic",
              learning_rate = 0.00065/log(nchannels(.eeg_lst)),
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
                            )
{

  ncomponents = nchannels(.eeg_lst)
  dots <- rlang::enquos(...)
  signal_raw <- dplyr::select(.eeg_lst$signal, !!!dots)
  
  # rest of the channels and cols of signal for the later reconstruction
  non_ica_signal <- .eeg_lst$signal[,setdiff(colnames(.eeg_lst$signal), colnames(signal_raw)), with = FALSE]
  
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

      ica_res <- fastICA::fastICA(signal_raw,
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
    S_ICA <- ica_res$S
    ncomponents <- nrow(ica_res$W)
    mixing_matrix <- ica_res$A 
    
  }
warning("grouping not implemented!!!!, this might apply ICA to several subjects together")
  component_names <- paste0("C",seq_len(ncomponents))
  
  intercept <- colMeans(signal_raw)

#col_info instead of channel_info and without any obligatory attribute...
#ica_lst (signal, events, segments, mixing, unmixing with the channels info)

  reconstr <- (S_ICA %*% mixing_matrix + intercept) 
  message("Absolute difference between original data and reconstructed from independent sources: ", 
    mean(abs(reconstr - as.matrix(signal_raw))) %>% signif(2))

  # S <- cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
  # A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
  # X <- S %*% A
  # 
  # a <- fastICA::fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
  #              method = "R", row.norm = FALSE, maxit = 200,
  #              tol = 0.0001, verbose = TRUE)
  # a$S %*% a$A + colMeans(X)
  # X
  
  S_ICA <- data.table::as.data.table(S_ICA)
  data.table::setnames(S_ICA, component_names)
  S_ICA <- S_ICA[,lapply(.SD,component_dbl)]
  # S_ICA[,.id := .eeg_lst$signal$.id][
  #                ,.sample_id := .eeg_lst$signal$.sample_id]
  
  
  mixing_tbl <- mixing_tbl(mixing_matrix = mixing_matrix,
                components = component_names, 
                channel_info = channels_tbl(signal_raw)) 

    ica_lst(signal = cbind(non_ica_signal, S_ICA), 
            mixing = mixing_tbl, 
            events = .eeg_lst$events, 
            segments = .eeg_lst$segments)

    #signal_out <- as.matrix(signal_raw) -  (dplyr::select(S_ICA,C2) %>% as.matrix() %*% mixing_matrix[,2])
  #  mean(abs(signal_out - as.matrix(signal_matrix)))



}
