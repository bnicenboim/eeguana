#' Simple plot an eegble object.
#' @param x An \code{eegble} object.
#' @param max_sample Downsample to approximately 2000 samples by default.
#' 
#' 
#' @return A ggplot object 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
plot.eegbl <- function(x, max_sample = 2000){
  
  if(is.numeric(max_sample) & max_sample != 0 & 
    # it will downsample if the samples are at least twice as large than the max_sample
    max(duration(x)) * srate(x) * 2 > max_sample ){
    x <- downsample(x, max_sample = max_sample)
  } 
  
  df <- as_tibble(x) 
  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude, group = .id)) + 
      ggplot2::geom_line() +
      ggplot2::facet_grid(channel ~ ., 
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::theme_bw()
  plot
}

#' ggplot object based on an eegble object.
#' @param x An \code{eegble} object.
#' @param max_sample Downsample to approximately 2000 samples by default.
#' 
#' 
#' @return A ggplot object 
#' 
#' @importFrom magrittr %>%
#' 
#' @export
plot_gg <- function(x, ...){
  UseMethod("plot_gg")
}

#' @export
plot_gg.eegbl <- function(x, ..., max_sample = 2000){
  
  if(is.numeric(max_sample) & max_sample != 0 & 
    # it will downsample if the samples are at least twice as large than the max_sample
    max(duration(x)) * srate(x) * 2 > max_sample ){
    x <- downsample(x, max_sample = max_sample)
  } 

  dots <- rlang::enquos(...) 
  df <-  as_tibble(x) 
  plot <- ggplot2::ggplot(df, 
      ggplot2::aes(x = time, y = amplitude, !!!dots)) + 
      ggplot2::scale_y_reverse() + 
      ggplot2::scale_colour_brewer(type = "qual", palette = "Dark2") +
      ggplot2::theme_bw()
  plot
}


#' A topographic plot of an eegble object.
#' 
#' Create a default topographic plot based on the segments of the \code{eegble} object.
#' 
#' The following methods of interpolation are available :
#' \itemize{
#' \item \code{"MBA"} (Default) Multilevel B-splines using the function \code{mba.surf}; requires the package \code{MBA}.
#' }
#' 
#' @param x An \code{eegble} object.
#' @param method Method of interpolation.
#' @param ... Various arguments passed to the interpolation method.
#' 
#' 
#' @return A ggplot object 
#' 
#' 
#' @export 
plot_topo <- function(x, method = "MBA", ...) {
  UseMethod("plot_topo")
}

#' @export 
plot_topo.eegbl <- function(x, method = "MBA", ...){
  grouping_vars <- colnames(x$segments) %>% setdiff(c(".id","segment"))
  chan_vars <- colnames(x$channels) %>% setdiff("labels")
  s_x <- summarize_id_as_tibble(x, mean, na.rm = TRUE) %>%
            dplyr::group_by_at(c(grouping_vars,chan_vars, "channel")) %>%
            dplyr::summarize(A = mean(mean, na.rm = TRUE)) %>% 
            dplyr::group_by_at(grouping_vars)


  grid <- interpolate_xy(s_x, x = x, y = y, value = A, ...)

  grid  %>% ggplot(aes(x, y))+  facet_wrap(grouping_vars) + 
  geom_raster(aes(fill = A), interpolate = F, hjust = 0.5, vjust = 0.5)+
  geom_contour(aes(z = A)) + 
  geom_text(data = filter(s_x, !is.na(x),!is.na(y)) , aes(x = x, y = y, label = channel), colour = 'black')  +
   # scale_fill_distiller(palette = "Spectral", guide = "colourbar", oob = scales::squish) + #, oob = scales::squish
  scale_fill_gradientn(colours = c("darkred","yellow","green","darkblue"),
                         values=c(1.0,0.75,0.5,0.25,0)) + 
   # scale_fill_distiller(palette = "RdBu", guide = "colourbar") + #, oob = scales::squish
      ggplot2::theme_bw()
  # scale_fill_viridis_c(option="B") +
}