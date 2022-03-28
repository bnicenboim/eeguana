
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
#' eeg_psd(data_)
#' 
eeg_psd <- function(.data, 
                             .method = "welch", 
                             .config = list(
                                window = function(x) 2^ceiling(log2(sqrt(NROW(x)))), 
                                overlap = 0.5,              
                                nfft = NULL,
                                detrend = c("long-mean", "short-mean", "long-linear", "short-linear", "none"),
                                range = "half")){
  
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
      
      first_col <- df[,chs[1],with = FALSE]
      x <- first_col %>% 
        as.matrix()
      first_psd <- gsignal::pwelch(x, window = win, overlap= .config$overlap,
                      nfft = nfft, detrend= .config$detrend, range = .config$.config$range, fs = srate)
      psd_dt <- data.table::data.table(.id = unique(df$.id), .freq = first_psd$freq, x = first_psd$spec) 
      data.table::setnames(psd_dt,"x", colnames(x))
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
