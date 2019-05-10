##'  Generates pink noise
##'
##'  Adapted from Matlab code of Smith (2011)
##'
##'
##' Reference:
##' Smith, J.O. Spectral Audio Signal Processing,
##' <http://ccrma.stanford.edu/~jos/sasp/>, online book,
##' 2011 edition.
##'
##' @param n number of observations.
##' @param scale scale parameter.
##' @return a vector
##' @export
##'
rpink <- function(n, scale = 1) {
  ## adapted to R from https://ccrma.stanford.edu/~jos/sasp/Example_Synthesis_1_F_Noise.html
  B <- c(0.049922035, -0.095993537, 0.050612699, -0.004408786)
  A <- c(1, -2.494956002, 2.017265875, -0.522189400)
  v <- stats::rnorm(n) # Gaussian white noise: N(0,1)
  x <- signal::filter(B, A, v) * scale
  x %>% as.numeric() # Apply 1/F roll-off to PSD
}
