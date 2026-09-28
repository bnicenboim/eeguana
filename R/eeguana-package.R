#' @section Options:
#' * `eeguana.verbose`: `TRUE` by default; with `FALSE`, the functions do not
#'   show messages about what they do, such as the filters they apply.
#' * `eeguana.print_max_channels`: maximum number of channels shown when an
#'   `eeg_lst`, `psd_lst`, or `eeg_ica_lst` is printed; `Inf`, all of them, by
#'   default. See [print.eeg_lst()].
#'
#' Set them with [options()], for example `options(eeguana.print_max_channels = 8)`.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tidytable case_when
#' @importFrom tidytable if_else
## usethis namespace: end
NULL
