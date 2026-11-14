# Compute the one-dimensional discrete Fourier Transform.

Computes the Discrete Fourier Transform (DFT) of an array with a fast
algorithm, the “Fast Fourier Transform” (FFT). Wrapper of
[stats::fft](https://rdrr.io/r/stats/fft.html) with an extra argument
similar to python's
[numpy.fft.fft](https://docs.scipy.org/doc/numpy/reference/generated/numpy.fft.fft.html#numpy.fft.fft).

## Usage

``` r
sig_fft(x, n = NULL)
```

## Arguments

- x:

  A vector

- n:

  Length of the transformed axis of the output. If `n` is smaller than
  the length of the input, the input is cropped. If it is larger, the
  input is padded with zeros. If n is not given, the length of the input
  along the axis specified by axis is used.

## Examples

``` r
a <- c(0, 4, 0, 0)
sig_fft(a)
#> [1]  4+0i  0-4i -4+0i  0+4i
```
