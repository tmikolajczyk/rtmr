taskseries_df <- function(series_id = NULL, ...) {
  structure(
    data.frame(
      series_id = series_id,
      ...,
      stringsAsFactors = FALSE
    ),
    class = c("taskseries_df", "data.frame")
  )
}

as.taskseries_df <- function(x) UseMethod("as.taskseries_df")
as.taskseries_df.taskseries_df <- function(x) x
as.taskseries_df.data.frame <- function(x) {
  if (!exists("series_id", x)) {
    stop("no series_id")
  } else {
    structure(x, class = c("taskseries_df", "data.frame"))
  }
}
as.taskseries_df.NULL <- function(x) taskseries_df()
is.taskseries_df <- function(x) inherits(x, "taskseries_df")

