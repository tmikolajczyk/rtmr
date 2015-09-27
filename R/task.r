##' Return standard task info from rtm_search in a nicer way.
##'
##' @param df a data frame of tasks returned by rtm_search
##' @export
filter_task_info <- function(df) {
  ## these are (all?) the fields containing meaningful info to the user.
  ## BEWARE: not all of them are atomic vectors (some lists and data frames)
  ## and they're not always the same thing, usually because of the possibility
  ## of recursion in JSON and in R lists and (what is often forgotten)
  ## data frames (remember columns can be lists and even other data frames!)
  mful_info <- intersect(c("name", "url", "location_id", "tags",
                           "participants", "notes", "rrule"),
                         names(df))
  ## when the task series list is 1 entry long, it is not a data frame but
  ## a named list so we need to do some trickery to get it into a data frame
  ## just quickdf and as.data.frame won't do b/c of the potentially
  ## nested nature of its contents
  if (!is.data.frame(df)) {
    first_part <- quickdf(df[setdiff(mful_info, c("notes", "participants"))])
    if (length(first_part[["notes"]]) > 0)
      first_part[["notes"]] <- df[["notes"]]
    else
      first_part[["notes"]] <- ""
    if (length(first_part[["participants"]]) > 0)
      first_part[["participants"]] <- df[["participants"]]
    else
      first_part[["participants"]] <- ""
    
    df[["task"]] <- list(df[["task"]])
  } else
    first_part <- df[, mful_info]

  tasks <- aggregate_task_column(df[["task"]])
  
  first_part[["seriesid"]] <- seq_len(nrow(first_part))
  res <- merge(first_part, tasks)
  res
}

##' Handle the task column properly depending on its form, either
##' a data frame or a list.
##'
##' If it's a data frame do nothing. If it's a list, it's a list of named
##' lists whose names are those of the data frame's columns, so converting
##' is not too difficult. Usually each list has one element but
##' sometimes one list will have more (which is why it wasn't stored as
##' a data frame in the first place).
##'
##' @param task_col the task "column", never actually an atomic vector though
##' @return a data frame
aggregate_task_column <- function(task_col) {
  if (class(task_col) == "list") {
    for (i in seq_along(task_col)) {
      task_col[[i]][["seriesid"]] <- i
    }
    rbinddf <- function(x, y) rbind(quickdf(x), quickdf(y))
    Reduce(rbinddf, task_col)
  } else
    task_col
}

## this idea and code (quickdf) is attributed to Hadley Wickham
## http://adv-r.had.co.nz/Profiling.html
## it's much faster than using as.data.frame, and we seem to be
## guaranteed well-formed lists
quickdf <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}
