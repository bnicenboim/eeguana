# Compute the one-dimensional inverse discrete Fourier Transform.

This function computes the inverse of the one-dimensional n-point
discrete Fourier transform computed by fft using a wrapper of
`fft(z, inverse= TRUE)/length(z)` with an extra argument making it
similar to
[numpy.fft.ifft](https://docs.scipy.org/doc/numpy-1.17.0/reference/generated/numpy.fft.ifft.html#numpy.fft.ifft).
For a general description of the algorithm and definitions, see
[stats::fft](https://rdrr.io/r/stats/fft.html) and
[numpy.fft](https://docs.scipy.org/doc/numpy-1.17.0/reference/generated/numpy.fft.ifft.html#numpy.fft.ifft).

## Usage

``` r
sig_ifft(x, n = NULL)
```

## Arguments

- x:

  A vector

- n:

  Length of the transformed axis of the output. If `n` is smaller than
  the length of the input, the input is cropped. If it is larger, the
  input is padded with zeros. If n is not given, the length of the input
  along the axis specified by axis is used.

## Value

A vector

## Details

The input should be ordered in the same way as is returned by fft, i.e.,
(*if I converted the indexes from python correctly*)

- `a[1]` should contain the zero frequency term,

- `a[2:ceiling(n/2)]` should contain the positive-frequency terms,

- `a[ceiling(n/2) + 2:length(a)]` should contain the negative-frequency
  terms, in increasing order starting from the most negative frequency.

- For an even number of input points, `a[ceiling(n/2)]` represents the
  sum of the values at the positive and negative Nyquist frequencies, as
  the two are aliased together. See see
  [numpy.fft](https://docs.scipy.org/doc/numpy-1.17.0/reference/generated/numpy.fft.ifft.html#numpy.fft.ifft)
  for details.

**Notes**

If the input parameter `n` is larger than the size of the input, the
input is padded by appending zeros at the end. Even though this is the
common approach, it might lead to surprising results. If a different
padding is desired, it must be performed before calling ifft.

## Examples

``` r

a <- c(0, 4, 0, 0)
sig_ifft(a)
#> [1]  1+0i  0+1i -1+0i  0-1i
sig_ifft(sig_fft(a)) == a
#> [1] TRUE TRUE TRUE TRUE
```
