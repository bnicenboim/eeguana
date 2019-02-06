#' Create a table with interpolated signals of an eeg_lst object.
#'
#' Create a default topographic plot based on the segments of the `eeg_lst` object.
#'
#' The following methods of interpolation are available :
#'
#'
#' @param .data An `eeg_lst` object or a long table with amplitudes..
#' @param ... Various arguments passed to the interpolation method.
#' @export
interpolate_tbl <- function(.data, ...) {
  UseMethod("interpolate_tbl")
}
#' @rdname interpolate_tbl
#' @param method Method of interpolation (Only `"MBA"` Multilevel B-splines using the function `mba.surf` of the package `MBA`.).
#' @param x Coordinate x
#' @param y Coordinate y
#' @param value amplitude (default)
#' @param label channel (default)
#' @param diam_points Density of the interpolation (number of points that are interpolated in the diameter of the scalp).
#' @export
interpolate_tbl.eeg_lst <- function(.data, x = .x, y = .y, value = amplitude, label = channel, diam_points = 200, method = "MBA", ...) {
  grouping <- group_chr(.data)
  .data <- dplyr::as_tibble(.data) %>% dplyr::group_by_at(dplyr::vars(grouping))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)
  dots <- rlang::enquos(...)
  # NextMethod()
  interpolate_tbl(.data, !!x, !!y, !!value, !!label, diam_points, method, !!!dots)
}
#' @rdname interpolate_tbl
#' @export
interpolate_tbl.tbl_df <- function(.data, x = .x, y = .y, value = amplitude, label = channel, diam_points = 200,
                                   method = "MBA", ...) {
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
    c(rlang::quo_text(x), rlang::quo_text(y), rlang::quo_text(value), rlang::quo_text(label))]

  ## add columns that are constant=>
  is_grouped <- .data %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::group_by(!!label, add = TRUE) %>%
    dplyr::summarize(L = n()) %>%
    # dplyr::filter(!is.na(!!x) |!is.na(!!y)) %>%
    dplyr::pull(L) %>%
    all(. == 1)
  if (!is_grouped) {
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

  l <- .data %>% dplyr::ungroup() %>% dplyr::select(dplyr::one_of(group_vars)) # %>%
  # dplyr::distinct()

  if (!identical(na.omit(l), l)) {
    stop("Data cannot be grouped by a column that contains NAs.")
  }

  if (method == "MBA") {
    if (!"MBA" %in% rownames(utils::installed.packages())) {
      stop("Package MBA needs to be installed to interpolate using multilevel B-splines ")
    }
    # change to sp = FALSE and adapt, so that I remove the sp package
    interpolation_alg <- function(xyz, ...) MBA::mba.surf(
        xyz = xyz,
        diam_points,
        diam_points,
        sp = FALSE,
        extend = TRUE,
        b.box = c(-1.1, 1.1, -1.1, 1.1)
      )
  } else {
    stop("Non supported method.")
  }

  # if there are no groups, it will just create a list with the entire df
  grid <- {
    if (ncol(l) == 0) {
      list(.data)
    } else {
      base::split(.data, l)
    }
  } %>%
    purrr::discard(~ nrow(.x) == 0) %>%
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
        !!rlang::quo_name(x) := rep(mba_interp$xyz$x, times = mba_interp$no.Y),
        # eq to mba_interp$xyz.est@coords[,1] with sp = TRUE, which requires an extra package
        !!rlang::quo_name(y) := rep(mba_interp$xyz$y, each = mba_interp$no.X),
        # eq to mba_interp$xyz.est@coords[,2]
        !!rlang::quo_name(value) := c(mba_interp$xyz$z)
      ) %>%
        # eq to mba_interp$xyz.est@data$z
        dplyr::filter(((!!x)^2 + (!!y)^2 <= 1.1)) %>%
        {
          dplyr::bind_cols(dplyr::slice(common, rep(1, each = nrow(.))), .)
        }
    })

  dplyr::bind_rows(grid, .data)
}

#' Looks for grobs matching a pattern. Almost identical to gtable::gtable_filter
#'
#' @param x
#' @param pattern
#' @noRd
#'
g_filter <-
  function(x, pattern, trim = TRUE) {
    matches <- grepl(pattern, x$layout$name)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    if (trim) {
      gtable_trim(x)
    } else {
      x
    }
  }
#' Looks for grobs not matching a pattern. Opposite of gtable::gtable_filter
#'
#' @param x
#' @param pattern
#' @noRd
#'
g_filter_out <- function(x, pattern, trim) {
  matches <- grepl(pattern, x$layout$name)
  x$layout <- x$layout[!matches, , drop = FALSE]
  x$grobs <- x$grobs[!matches]
  if (trim) {
    gtable_trim(x)
  } else {
    x
  }
}


#' identical to gtable::gtable_trim
#' @noRd
gtable_trim <-
  function(x) {
    w <- range(x$layout$l, x$layout$r)
    h <- range(x$layout$t, x$layout$b)
    x$widths <- x$widths[seq.int(w[1], w[2])]
    x$heights <- x$heights[seq.int(h[1], h[2])]
    x$layout$l <- x$layout$l - w[1] + 1
    x$layout$r <- x$layout$r - w[1] + 1
    x$layout$t <- x$layout$t - h[1] + 1
    x$layout$b <- x$layout$b - h[1] + 1
    x
  }

#' stereographic projection over a 2d plane
#'
#' @param x
#' @param y
#' @param z
#' @noRd
stereographic <- function(x, y, z) {
  mu <- 1 / (sqrt(x^2 + y^2 + z^2) + z)
  x <- x * mu
  y <- y * mu
  list(x = x, y = y)
}
#' polar projection over a 2d plane
#'
#' @param x
#' @param y
#' @param z
#' @noRd
polar <- function(x, y, z, scale = TRUE) {
  az <- atan2(y, x)
  el <- atan2(z, sqrt(x^2 + y^2))
  x <- (pi / 2 - el) * cos(az)
  y <- (pi / 2 - el) * sin(az)
  if (scale) {
    k <- pi / 2
  } else {
    k <- 1
  }
  list(x = x / k, y = y / k)
}
#' orthographic projection over a 2d plane
#'
#' @param x
#' @param y
#' @param z
#' @noRd
orthographic <- function(x, y, z) {
  list(x = x, y = y)
}

#' change coordinate system from 3d to 2d
#'
#' @param data 
#' @param projection 
#'
#' @return
#'
#' @noRd
change_coord <- function(data, projection) {
  if (stringr::str_to_lower(projection) == "orthographic") {
    project <- orthographic
  } else if (stringr::str_to_lower(projection) == "polar") {
    project <- polar
  } else if (stringr::str_to_lower(projection) == "stereographic") {
    project <- stereographic
  }

  if (all(is.na(data$.z)) & !identical(project, orthographic)) {
    warning("Z coordinates are missing, using 'ortographic' projection ")
    project <- orthographic
  }

  new_coord <- project(data$.x, data$.y, data$.z)
  data$.x <- new_coord$x
  data$.y <- new_coord$y
  data$.z <- NA
  data
}