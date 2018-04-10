filter_.taskseries <- function(tsrs, expr) keep_ids(tsrs, eval_across_plist(expr, tsrs))

keep_ids <- function(tsrs, ids) {
  as.taskseries(purrr::map(tsrs, ~ .x[.x[["series_id"]] %in% ids, ]))
}

discard_ids <- function(tsksrs, ids) {
  as.taskseries(purrr::map(tsksrs, ~ .x[!.x[["series_id"]] %in% ids, ]))
}
