#' Create a table with interpolated signals of an eeg_lst object.
#'
#' Create a default topographic plot based on the segments of the `eeg_lst` object.
#'
#'
#' @param .data An `eeg_lst` object or a long table with amplitudes..
#' @param ... Various arguments passed to the interpolation method.
#' @export
eeg_interpolate_tbl <- function(.data, ...) {
  UseMethod("eeg_interpolate_tbl")
}
#' @rdname eeg_interpolate_tbl
#' @param size Indicates how much to extrapolate, `1` indicates no extrapolation beyond the position of the electrodes.
#' @param method Method of interpolation (`"MBA"` Multilevel B-splines using the function `mba.surf` of the package `MBA` or (`"akima"` bicubic spline Akima interpolation algorithm using the function `interp` of the package `akima`.)..).
#' @param x Variable storing the x coordinate, generally `.x` (default).
#' @param y Variable storing the y coordinate, generally `.y` (default).
#' @param value Values used for the interpolation, generally `amplitude` (default). 
#' @param label Label of the points that are used for the interpolation, generally `channel` (default).
#' @param diam_points Density of the interpolation (number of points that are interpolated in the diameter of the scalp).
#' @export
eeg_interpolate_tbl.eeg_lst <- function(.data, size = 1.2, x = .x, y = .y, value = amplitude, label = channel, diam_points = 200, method = "MBA", ...) {
  grouping <- group_chr(.data)
  .data <- dplyr::as_tibble(.data) %>% dplyr::group_by_at(dplyr::vars(grouping))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  value <- rlang::enquo(value)
  label <- rlang::enquo(label)
  dots <- rlang::enquos(...)
  # NextMethod()
  eeg_interpolate_tbl(.data, size = size, !!x, !!y, !!value, !!label, diam_points, method, !!!dots)
}
#' @rdname eeg_interpolate_tbl
#' @export
eeg_interpolate_tbl.tbl_df <- function(.data, size = 1.2, x = .x, y = .y, value = amplitude, label = channel, diam_points = 200,
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
    stop("Data needs to grouped or summarized so that each label appears one per group.\n",
         "Tip: You should do probably need to do `eeg_lst %>% group_by(YOUR_GROUPS) %>% summarize_all_ch(mean)` before calling this function",call. = FALSE)
  }


  l <- .data %>% dplyr::ungroup() %>% dplyr::select(dplyr::one_of(group_vars)) # %>%

  if (!identical(stats::na.omit(l), l)) {
    stop("Data cannot be grouped by a column that contains NAs.")
  }

  if (stringr::str_to_lower(method) == "mba") {
    if (!"MBA" %in% rownames(utils::installed.packages())) {
      stop("Package MBA needs to be installed to interpolate using multilevel B-splines ")
    }
    interpolation_alg <- function(xyz, ...) {
      results <- MBA::mba.surf(
        xyz = xyz,
        diam_points,
        diam_points,
        sp = FALSE,
        extend = TRUE,
        b.box = c(
          min(xyz[[1]], na.rm = TRUE) * size,
          max(xyz[[1]], na.rm = TRUE) * size,
          min(xyz[[2]], na.rm = TRUE) * size,
          max(xyz[[2]], na.rm = TRUE) * size
        ),
        ...
      )

      dplyr::tibble(
        !!rlang::quo_name(x) := rep(results$xyz$x, times = results$no.Y),
        # eq to mba_interp$xyz.est@coords[,1] with sp = TRUE, which requires an extra package
        !!rlang::quo_name(y) := rep(results$xyz$y, each = results$no.X),
        # eq to mba_interp$xyz.est@coords[,2]
        !!rlang::quo_name(value) := c(results$xyz$z)
      )
    }
  } else if (stringr::str_to_lower(method) == "akima") {
    if (!"akima" %in% rownames(utils::installed.packages())) {
      stop("Package akima needs to be installed to interpolate using bicubic spline Akima interpolation algorithm.")
    }
    interpolation_alg <- function(xyz, ...) {
      results <- akima::interp(
        x = xyz[[1]], y = xyz[[2]], z = xyz[[3]],
        xo = seq(min(xyz[[1]], na.rm = TRUE) * size, max(xyz[[1]], na.rm = TRUE) * size, length = diam_points),
        yo = seq(min(xyz[[2]], na.rm = TRUE) * size, max(xyz[[2]], na.rm = TRUE) * size, length = diam_points),
        extrap = TRUE,
        linear = FALSE,
        duplicate = "error",
        ...
      ) 
        dplyr::tibble(
          !!rlang::quo_name(x) := rep(results$x, times = diam_points),
          # eq to mba_interp$xyz.est@coords[,1] with sp = TRUE, which requires an extra package
          !!rlang::quo_name(y) := rep(results$y, each = diam_points),
          # eq to mba_interp$xyz.est@coords[,2]
          !!rlang::quo_name(value) := c(results$z)
        )
    }
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

      interpolation_alg(interpolate_from) %>%
        # eq to mba_interp$xyz.est@data$z
        dplyr::filter(((!!x)^2 + (!!y)^2 <= 1 * size)) %>%
        {
          dplyr::bind_cols(dplyr::slice(common, rep(1, each = nrow(.))), .)
        }
    })

  dplyr::bind_rows(grid, .data)
}

#' Looks for grobs matching a pattern. Almost identical to gtable::gtable_filter
#'
#' Looks for grobs matching a pattern. Almost identical to gtable::gtable_filter
#' @param x gtable
#' @param pattern regex
#' @noRd
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
#' Looks for grobs not matching a pattern. Opposite of gtable::gtable_filter
#' @param x gtable
#' @param pattern regex
#' @noRd
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
#' @param x gtable
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
#' @param x x
#' @param y y
#' @param z z
#' @noRd
stereographic <- function(x, y, z) {
  mu <- 1 / (sqrt(x^2 + y^2 + z^2) + z)
  x <- x * mu
  y <- y * mu
  list(x = x, y = y)
}
#' polar projection over a 2d plane
#'
#' @param x x
#' @param y y
#' @param z z
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
#' @param x x
#' @param y y
#' @param z z
#' @noRd
orthographic <- function(x, y, z) {
  list(x = x, y = y)
}

#' Change coordinate system from 3D to 2D
#'
#'
#' @param data A tbl created with channels_tbl
#' @param projection projection type
#'
#' @return A modified channels_tbl
#'
#' @export
change_coord <- function(data, projection = "polar") {
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
