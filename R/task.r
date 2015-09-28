##' Process task info from rtm_search.
##'
##' @param df a data frame of tasks returned by rtm_search
##' @export
filter_task_info <- function(df) {
  ## when the task series list is 1 entry long, it's not a data frame but
  ## a named list so we need to do some trickery to get it into a data frame
  ## just quickdf and as.data.frame won't do b/c of the potentially
  ## nested nature of its contents
  if (!is.data.frame(df)) {
    list_elems <- c("notes", "participants", "tags", "task")
    atomic_df <- quickdf(df[setdiff(names(df), list_elems)])

    ## add the lists elements as columns one-by-one
    col_or_blank <- function (col) if(length(df[[col]]) > 0) df[[col]] else ""
    atomic_df[["notes"]] <- col_or_blank("notes")
    atomic_df[["participants"]] <- col_or_blank("participants")
    atomic_df[["tags"]] <- col_or_blank("tags")

    ## the task list is a special case, as it's a data frame in its own right
    task <- quickdf(df[["task"]])
    names(task) <- gsub("^", "task\\.", names(task))
    df <- cbind(atomic_df, task)
  }

  ## convert times for all
  time_vars <- c("created", "modified", "task.due",
                 "task.added", "task.completed")
  df[, time_vars] <- plyr::colwise(rtm_date, time_vars)(df)
  df
}

##' Convert date stored in RTM server format to R POSIXlt format.
##'
##' @param date date string in RTM server format
##' @return a POSIXlt value
rtm_date <- function(date) {
  strptime(date, "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
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
