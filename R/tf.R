
#' Compute the power spectral density (PSD) of an `eeg_lst` object
#'
#' Wrapper of [gsignal::pwelch] function.
#' 
#' @param .data A `eeg_lst` object.
#' @param .method "welch" for Welch’s method.
#' @param .config  See [gsignal::pwelch].
#'
#' @return A `psd_lst` object
#' @export
#'
#' @examples
#' psd_faces <- eeg_psd(data_faces_ERPs) 
#' psd_faces %>% 
#'    eeg_select(F3) %>%
#'    plot()     
#'
#' @export
eeg_psd <- function(.data, 
                            .method = "welch", 
                            .config = list(
                              window = function(x) 2^ceiling(log2(sqrt(NROW(x)))), 
                              overlap = 0.5,              
                              nfft = NULL,
                              detrend = c("long-mean", "short-mean", "long-linear", "short-linear", "none"),
                              range = "half")){
  UseMethod("eeg_psd")
}

#' @export
eeg_psd.eeg_lst <- function(.data, 
                             .method = "welch", 
                             .config = list(
                                window = function(x) 2^ceiling(log2(sqrt(NROW(x)))), 
                                overlap = 0.5,              
                                nfft = NULL,
                                detrend = c("long-mean", "short-mean", "long-linear", "short-linear", "none"),
                                range = "half")){
  .config$window <- .config$window %||% 2^ceiling(log2(sqrt(NROW(x))))
  .config$overlap <- .config$overlap %||% 0.5
  .config$detrend <- .config$detrend %||% c("long-mean", "short-mean", "long-linear", "short-linear", "none")
  .config$range <- .config$range %||% "half"

  srate <- sampling_rate.eeg_lst(.data)
  if(tolower(.method) != "welch") stop("Only 'welch' method is allowed.", call. = FALSE)
  # isScalar is taken from gsignal
  isScalar <- function (x)  ifelse(is.character(x), nchar(x) == 1L, (is.atomic(x) && length(x) ==1L))
  chs <- channel_names(.data)
  dfs <- .data$.signal %>% split(by = ".id", keep.by = FALSE)

  ls_signal <-
    .data$.signal %>% split(by = ".id", keep.by = TRUE) %>%
    lapply(function(df){ #each segment:
      win <- if(is.function(.config$window)) .config$window(df) else .config$window
      nfft <- if(is.null(.config$nfft)){
        if (isScalar(win)) win else length(win)
      }
      # it does the first col first, to store the frequencies
      first_col <- df[,chs[1],with = FALSE]
      x <- first_col %>% 
        as.matrix()
      first_psd <- gsignal::pwelch(x, window = win, overlap= .config$overlap,
                      nfft = nfft, detrend= .config$detrend, range = .config$.config$range, fs = srate)
      psd_dt <- data.table::data.table(.id = unique(df$.id), .freq = first_psd$freq, x = first_psd$spec) 
      data.table::setnames(psd_dt,"x", colnames(x))
      # rest of the columns without storing frequencies
      if(length(chs) > 1 ) {
        rest_col <- df[,chs[-1],with = FALSE]
        rest_psd <- rest_col  %>% lapply(function(x)
              gsignal::pwelch(as.numeric(x), window = win, overlap= .config$overlap,
                        nfft = nfft, detrend = .config$detrend, range = .config$.config$range, fs = srate)$spec)
        psd_dt <- cbind(psd_dt, do.call("cbind", rest_psd))
      }
      psd_dt
    })
   
  .psd <- do.call("rbind", ls_signal) 
  
  psd_lst(psd_tbl = .psd,
          segments_tbl = .data$.segments,
          channels_tbl = channels_tbl(.data))
}

#' Compute power bands.
#'
#' Compute power bands.
#' 
#' @param .data An `eeg_lst` or `psd_lst`.
#' @param .bands A named list of frequency bands.
#' @param ... Other arguments passed to `eeg_psd()`
#'
#' @export
eeg_power_band <- function(.data, .bands= list(delta = c(0.5, 4),
                                              theta = c(4,8),
                                              alpha = c(8, 13),
                                              beta = c(13,30)),...){

  # FREQ_BANDS = {"delta": [0.5, 4.5],
  #   "theta": [4.5, 8.5],
  #   "alpha": [8.5, 11.5],
  #   "sigma": [11.5, 15.5],
  #   "beta": [15.5, 30]}
  UseMethod("eeg_power_band")
}

#' @export
eeg_power_band.psd_lst <- function(.data, .bands= list(delta = c(0.5, 4),
                                              theta = c(4,8),
                                              alpha = c(8, 13),
                                              beta = c(13,30)),
                                   ...){
  

  chs <- channel_names(.data)
  .data$.psd <- .data$.psd %>% summarize.(across.(tidyselect::all_of(!!chs), 
                                     function(.ch) {
                                       lapply(.bands, function(b){
                                         b_freq <- between(.freq,b[1],b[2])
                                        int.simpson2(.freq[b_freq], .ch[b_freq])
                                       }) %>% unlist()
                                     }
                                       ), .freq = names(.bands), .by = ".id") %>%
    select.(obligatory_cols$.psd, tidyselect::everything())
 .data
}




#' @export
eeg_power_band.eeg_lst <- function(.data, .bands= list(delta = c(0.5, 4),
                                                       theta = c(4,8),
                                                       alpha = c(8, 13),
                                                       beta = c(13,30)),...){
  eeg_psd(.data,...) %>%
    eeg_power_band(.bands = .bands)
    
}
