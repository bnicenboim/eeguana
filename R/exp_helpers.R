#' @export
interpolate_xy <- function(.data, x, y, value, method = "MBA",...){
  # x <- rlang::quo(x)
  # y <- rlang::quo(y)
  # value <- rlang::quo(A)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)

  .data <- select(.data, one_of(dplyr::group_vars(.data)), !!x, !!y, !!value, channel) 
  l <- select(.data, one_of(dplyr::group_vars(.data))) 

  #REMOVE EMPTY
  if(method == "MBA") {
  if(!"MBA" %in% rownames(utils::installed.packages())){
      stop("Package MBA needs to be installed to interpolate using multilevel B-splines ")
    }
   #change to sp = FALSE and adapt, so that I remove the sp package
  interpolation_alg <- function(xyz, ...) MBA::mba.surf(xyz = xyz, 100, 100, sp = TRUE, extend = TRUE, ...)
   
    } else {
  stop("Non supported method.")
  }
  grid <- split(.data, l) %>% 
          purrr::discard(~ nrow(.x)==0) %>% 
          purrr::map_dfr(function(.d) {
                          common <- .d %>% 
                          dplyr::ungroup() %>%
                          dplyr::select(-channel, -!!x, -!!y, -!!value) %>%
                          distinct()

                          if(nrow(common)>1) {stop("Bad grouping.")}
                        
                          interpolate_from <- .d %>% 
                          dplyr::ungroup() %>%
                          dplyr::select(!!x, !!y, !!value) %>% 
                          dplyr::filter(!is.na(x) | !is.na(y))

                          if(nrow(interpolate_from)==1) stop("Interpolation is not possible from only one point.")
                          mba_interp <- interpolation_alg(interpolate_from) 
                           

                          dplyr::tibble(!!quo_name(x) := mba_interp$xyz.est@coords[,1],
                          !!quo_name(y) := mba_interp$xyz.est@coords[,2], 
                          !!quo_name(value) := mba_interp$xyz.est@data$z) %>% 
                          dplyr::filter(((!!x)^2 + (!!y)^2 < 1.1)) %>% 
                          {dplyr::bind_cols(dplyr::slice(common, rep(1, each = nrow(.))),.) } 
                                      })

   grid 
}

