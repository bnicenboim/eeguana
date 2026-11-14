# Install the Python packages needed for `eeguana`

`install_py_eeguana` facilitates the installation of Python packages
required by `eeguana` within an R environment. It leverages the
`reticulate` package to manage Python environments and supports various
installation methods, environment configurations, and Python versions.

## Usage

``` r
install_py_eeguana(
  conda = "auto",
  envname = "r-eeguana",
  restart_session = TRUE,
  forge = TRUE,
  ...,
  new_env = identical(envname, "r-eeguana"),
  python_version = NULL
)
```

## Arguments

- conda:

  The path to a `conda` executable. Use `"auto"` to allow `reticulate`
  to automatically find an appropriate `conda` binary. See **Finding
  Conda** and
  [`conda_binary()`](https://rstudio.github.io/reticulate/reference/conda-tools.html)
  for more details.

- envname:

  The name of the virtual environment to create or use. Default is
  'r-eeguana'.

- restart_session:

  Whether to restart the R session after installation. Default is TRUE.

- forge:

  Whether to use conda-forge to install packages. Default is TRUE.

- ...:

  Additional arguments passed to
  [`reticulate::py_install`](https://rstudio.github.io/reticulate/reference/py_install.html).

- new_env:

  Whether to create a new environment if `envname` is 'r-eeguana'.
  Default behavior is determined by the identity of `envname`.

- python_version:

  The requested Python version. Ignored when attempting to install with
  a Python virtual environment.

## Value

Returns `NULL` invisibly and prints a message upon successful
installation. If `restart_session` is TRUE and the R session is running
within RStudio, the session will restart automatically.

## Details

The function selects an appropriate method for environment management
and Python installation, focusing on ease of use and flexibility. It
supports creating new virtual or conda environments, installing
necessary packages, and handling Python version requirements.

The function is designed to be robust, handling various scenarios such
as existing environments, and provides detailed feedback during the
process.
