#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr "%>%"
#' @usage lhs \%>\% rhs
NULL

compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

sort_names <- function(x) x[order(names(x))]

mpf <- function(...) message(sprintf(...))
