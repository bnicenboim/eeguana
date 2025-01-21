# temp findpeaks from https://github.com/gjmvanboxtel/gsignal/blob/4df5bad7a391b3ed55484aa767d52a150e29b22f/R/findpeaks.R#L119
findpeaks <- function(data,
                      MinPeakHeight = .Machine$double.eps,
                      MinPeakDistance = 1,
                      MinPeakWidth = 1,
                      MaxPeakWidth = Inf,
                      DoubleSided = FALSE) {
  # check function arguments
  ld <- length(data)
  if (!is.numeric(data) ||
      !(is.vector(data) || is.array(data) || inherits(data, "ts"))
      || ld < 3)
    stop("data must be a numeric vector of at least 3 elements")
  if (!is.logical(DoubleSided))
    stop("DoubleSided should a a logical value TRUE or FALSE")
  wdata <- abs(gsignal::detrend(data, 0))
  if (DoubleSided) {
    tmp <- data
    data <- wdata
    wdata <- tmp
  } else {
    if (min(data, na.rm = TRUE) < 0) {
      stop("Data contains negative values. Use the 'DoubleSided' option?")
    }
  }
  # Rough estimates of first and second derivative
  df1 <- diff(data, differences = 1)[c(1, 1:(ld - 1))]
  df2 <- diff(data, differences = 2)[c(1, 1, 1:(ld - 2))]
  # check for changes of sign of 1st derivative and negativity of 2nd deriv.
  # <= in 1st derivative includes the case of oversampled signals.
  idx <- which(df1 * c(df1[2:length(df1)], 0) <= 0 &
                 c(df2[2:length(df2)], 0) < 0)
  # Get peaks that are beyond given height
  tf  <- which(data[idx] > MinPeakHeight)
  idx <- idx[tf]
  if (length(idx) <= 0) return(NULL)
  # sort according to magnitude
  tmp <- sort(data[idx], decreasing = TRUE, index = TRUE)
  idx_s <- idx[tmp$ix]
  ## Treat peaks separated less than MinPeakDistance as one
  D <- with(expand.grid(A = idx_s, B = t(idx_s)), abs(A - B))
  dim(D) <- c(length(idx_s), length(idx_s))
  diag(D) <- NA                     # eliminate diagonal comparison
  if (isTRUE(any(D < MinPeakDistance))) {
    i <- 1
    node2visit <- seq_along(idx_s)
    visited <- NULL
    idx_pruned <- idx_s
    while (length(node2visit) > 0) {
      d <- D[node2visit[1], ]
      visited <- c(visited, node2visit[1])
      node2visit <- node2visit[-1]
      neighs <- setdiff(which(d < MinPeakDistance), visited)
      if (length(neighs) > 0) {
        idx_pruned <- setdiff(idx_pruned, idx_s[neighs])
        visited <- c(visited, neighs)
        node2visit <- setdiff(node2visit, visited)
      }
    }
    idx <- idx_pruned
  }
  idx <- sort(idx)
  extra_x <- extra_pp <- extra_roots <-
    extra_height <- extra_baseline <- data.frame()
  # Estimate widths of peaks and filter for:
  # width smaller than given.
  # wrong concavity.
  # not high enough
  # data at peak is lower than parabola by 1%
  idx_pruned <- idx
  n  <- length(idx)
  for (i in 1:n) {
    ind <- round(max(idx[i] - MinPeakDistance / 2, 1)) :
      round(min(idx[i] + MinPeakDistance / 2, ld))
    pp <- rep(0L, 3)
    if (any(data[ind] > data[idx[i]])) {
      pp <- pracma::polyfit(ind, data[ind], 2)
      xm <- -pp[2]^2 / (2 * pp[1])       # position of extrema
      H <- pracma::polyval(pp, xm)       # value at extrema
    } else {                             # use it as vertex of parabola
      H <- data[idx[i]]
      xm <- idx[i]
      pp <- rep(1L, 3)
      pp[1] <- pracma::mldivide((ind - xm)^2, (data[ind] - H))
      pp[2] <- -2 * pp[1] * xm
      pp[3] <- H + pp[1] * xm^2
    }
    width <- sqrt(abs(1 / pp[1])) + xm
    if ((width > MaxPeakWidth || width < MinPeakWidth) ||
        pp[1] > 0 || H < MinPeakHeight ||
        data[idx[i]] < 0.99 * H || abs(idx[i] - xm) > MinPeakDistance / 2) {
      idx_pruned <- setdiff(idx_pruned, idx[i])
    } else {
      extra_x <- rbind(extra_x, ind[c(1, length(ind))])
      extra_pp <- rbind(extra_pp, pp)
      extra_roots <- rbind(extra_roots, xm + c(-width, width) / 2)
      extra_height <- rbind(extra_height, H)
      extra_baseline <- rbind(extra_baseline, mean(c(H, MinPeakHeight)))
    }
  }
  idx <- idx_pruned
  # check for double sided
  if (DoubleSided) {
    pks <- wdata[idx]
  } else {
    pks <- data[idx]
  }
  # return values
  list(pks = pks, loc = idx)
}
