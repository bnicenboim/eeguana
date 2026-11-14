# Create a table with interpolated signals of an eeg_lst object.

Create a default topographic plot based on the segments of the `eeg_lst`
object.

## Usage

``` r
eeg_interpolate_tbl(.data, ...)

# S3 method for class 'eeg_lst'
eeg_interpolate_tbl(
  .data,
  .radius = 1.2,
  .diam_points = 100,
  .method = "MBA",
  ...
)

# S3 method for class 'data.frame'
eeg_interpolate_tbl(
  .data,
  .radius = 1.2,
  .x = .x,
  .y = .y,
  .value = .value,
  .label = .key,
  .diam_points = 100,
  .method = "MBA",
  ...
)
```

## Arguments

- .data:

  An `eeg_lst` object or a long table with amplitudes..

- ...:

  Various arguments passed to the interpolation method.

- .radius:

  Indicates the radius of the extrapolation, for standard locations of
  electrodes, `1` indicates no extrapolation beyond the position of the
  electrodes, `1.2` is the default

- .diam_points:

  Density of the interpolation (number of points that are interpolated
  in the diameter of the scalp).

- .method:

  Method of interpolation (`"MBA"` Multilevel B-splines using the
  function `mba.surf` of the package `MBA` or (`"akima"` bicubic spline
  Akima interpolation algorithm using the function `interp` of the
  package `akima`.)..).

- .x:

  Variable storing the x coordinate, generally `.x` (default).

- .y:

  Variable storing the y coordinate, generally `.y` (default).

- .value:

  Values used for the interpolation, generally `.value` (default).

- .label:

  Label of the points that are used for the interpolation, generally
  `.key` (default).
