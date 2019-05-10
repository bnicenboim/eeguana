#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Convenience function for range subsets 
#' 
#' between is a thin wrapper for the between function of [data.table]. It is equivalent to x >= lower & x <= upper when incbounds=TRUE, or x > lower & y < upper when FALSE.
#' 
#' @inheritParams  data.table::between
#' @export
between <- data.table::between
