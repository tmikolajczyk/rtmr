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
spf <- function(...) stop(sprintf(...))

mutate_each_col_f <- function(f) {
  function(df, cols = names(df)) {
    df[, cols] <- colwise(f)(df[, cols])
    df
  }
}

colwise <- function(.fun) {
  function(df, ...) {
    out <- do.call("lapply", c(list(df, .fun, ...)))
    quickdf(out)
  }
}

rename <- function(df, ...) {
  dots <- list(...)
  names(df)[match(names(df), unlist(dots))] <- names(dots)
  df
}

check_unique_list <- function(lst) {
  is.list(lst) && length(lst) <= 1
}

as_lazy <- function(expr, env = baseenv()) lazyeval::as.lazy(expr, env)$expr
