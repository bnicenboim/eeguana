#' @export
interpolate_tbl <- function(.data, ...) {
  UseMethod("interpolate_tbl")
}

#' @export
interpolate_tbl.eeg_lst <- function(.data, x = .x, y = .y, value = amplitude, label = channel, diam_points =200, method = "MBA",...) {
  grouping <- group_chr(.data)
  .data <- dplyr::as_tibble(.data) %>% dplyr::group_by_at(vars(grouping))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)
  dots <- rlang::enquos(...)
  # NextMethod()
  interpolate_tbl(.data, !!x, !!y, !!value, !!label, diam_points, method, !!!dots)
}

#' @export
interpolate_tbl.tbl_df <- function(.data, x = .x, y = .y, value = amplitude, label = channel, diam_points =200, method = "MBA",...) {
  # x <- rlang::quo(.x)
  # y <- rlang::quo(.y)
  # value <- rlang::quo(amplitude)
  # label <- rlang::quo(channel)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)

  .data <- dplyr::select(.data, dplyr::one_of(dplyr::group_vars(.data)), !!x, !!y, !!value, !!label)     
   group_vars <- dplyr::group_vars(.data)
   group_vars <- group_vars[!group_vars %in%  
                      c(rlang::quo_text(x),rlang::quo_text(y),rlang::quo_text(value),rlang::quo_text(label))]

## add columns that are constant=>
   is_grouped <- .data %>% dplyr::group_by_at(group_vars) %>%
                  dplyr::group_by(!!label,add=TRUE) %>%
                 dplyr::summarize(L = n()) %>% 
                 # dplyr::filter(!is.na(!!x) |!is.na(!!y)) %>% 
                 dplyr::pull(L) %>%
                 all(.==1) 
   if(!is_grouped){
    stop("Data needs to grouped or summarized so that each label appears one per group.")
   } 

# .data %>% summarize(L = length(!!label)) %>% print(n=100)
#    if(is.null(dplyr::groups(.data) && "channel" %in% colnames) {
#     message("Grouping by channel")
#     .data <- .data %>% dplyr::group_by(!!x,!!y, channel)
#    } else if(!is.null(dplyr::groups(.data)){
#    } else {
#     stop("The table needs to be grouped.")
#    }

#   l <- .data %>%  dplyr::summarize(!!value := mean(!!value))

    l <- .data %>% dplyr::ungroup() %>% dplyr::select(dplyr::one_of(group_vars)) %>%
    # in case it should group by some NA
    
    dplyr::mutate_all(tidyr::replace_na, "NA") %>% distinct()

  if (method == "MBA") {
    if (!"MBA" %in% rownames(utils::installed.packages())) {
      stop("Package MBA needs to be installed to interpolate using multilevel B-splines ")
    }
    # change to sp = FALSE and adapt, so that I remove the sp package
    interpolation_alg <- function(xyz, ...) MBA::mba.surf(xyz = xyz, diam_points, diam_points, sp = FALSE, extend = TRUE, ...)
  } else {
    stop("Non supported method.")
  }

  # if there are no groups, it will just create a list with the entire df
  grid <- {
    if (ncol(l) == 0) {
      list(.data)
    } else {
      split(.data, l)
    }
  } %>%
    purrr::discard(~nrow(.x) == 0) %>%
    purrr::map_dfr(function(.d) {
      common <- .d %>%
        dplyr::ungroup() %>%
        dplyr::select(-!!label, -!!x, -!!y, -!!value) %>%
        dplyr::distinct()

      if (nrow(common) > 1
      # when there is no common columns, distintict returns anyway a number of columns, distinct bug?? TODO: report
      && ncol(common) > 0) {
        stop("Bad grouping.")
      }

      interpolate_from <- .d %>%
        dplyr::ungroup() %>%
        dplyr::select(!!x, !!y, !!value) %>%
        dplyr::filter(!is.na(!!x) | !is.na(!!y))

      if (nrow(interpolate_from) == 1) stop("Interpolation is not possible from only one point.")
      mba_interp <- interpolation_alg(interpolate_from)

      dplyr::tibble(
        !!quo_name(x) := rep(mba_interp$xyz$x, times = mba_interp$no.Y),
        # eq to mba_interp$xyz.est@coords[,1] with sp = TRUE, which requires an extra package
        !!quo_name(y) := rep(mba_interp$xyz$y, each = mba_interp$no.X),
        # eq to mba_interp$xyz.est@coords[,2]
        !!quo_name(value) := c(mba_interp$xyz$z)
      ) %>%
        # eq to mba_interp$xyz.est@data$z
        dplyr::filter(((!!x)^2 + (!!y)^2 < 1.1)) %>%
        {
          dplyr::bind_cols(dplyr::slice(common, rep(1, each = nrow(.))), .)
        }
    })

  dplyr::bind_rows(grid,.data)
}
