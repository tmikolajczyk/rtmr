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

  ## if tasks are repeating it may be the case that it has multiple
  ## entries per row, we need to test if df[["task"]] is a list
  if (exists("task", where = df) && !is.data.frame(df[["task"]])) {
    df <- handle_multiple_tasks(df)
  }

  ## convert times for all
  time_vars <- c("created", "modified", "task.due",
                 "task.added", "task.completed")
  df[, time_vars] <- plyr::colwise(rtm_date, time_vars)(df)
  df
}

##' Handle the case where a task is repeating and has multiple entries
##' per row.
##'
##' In this case df[["task"]] is a list and needs to be converted to a
##' data frame and merged back into the larger data frame.
##'
##' @param df the list
##' @return the data frame
handle_multiple_tasks <- function(df) {
  for (i in seq_along(df[["task"]])) {
    df[["task"]][[i]][["series_id"]] <- i
  }
  tasks <- Reduce(rbinddf, df[["task"]])
  ## rename tasks according to the convention
  names(tasks) <- gsub("^", "task\\.", names(tasks))
  df[["task"]] <- NULL
  df[["task.series_id"]] <- seq_len(nrow(df))
  df <- merge(df, tasks)
  df[["task.series_id"]] <- NULL
  df
}

##' Convert date stored in RTM server format to R POSIXlt format.
##'
##' @param date date string in RTM server format
##' @return a POSIXlt value
rtm_date <- function(date) {
  lubridate::fast_strptime(date, "%Y-%m-%dT%H:%M:%SZ", lt = FALSE)
}

##' Convert a list to a data frame
##'
##' this idea and code (quickdf) is attributed to Hadley Wickham
##' http://adv-r.had.co.nz/Profiling.html
##' it's much faster than using as.data.frame, and in our use case
##' we seem to be guaranteed well-formed lists
##' @param lst list suitable to be a data frame with quick conversion
##' @return a data frame
quickdf <- function(lst = list()) {
  class(lst) <- "data.frame"
  attr(lst, "row.names") <- .set_row_names(if (length(lst) > 0) length(lst[[1]]) else 0)
  lst
}

##' Create a data frame out of two lists or data frames.
##'
##' @param x a list or data frame
##' @param y a list or data frame
##' @return a data frame
rbinddf <- function(x, y) {
  rbind(quickdf(x), quickdf(y))
}

##' Call rtm_req with a "rtm.tasks.*" method on the given task.
##'
##' @param method one of the RTM API "rtm.tasks.*" methods
##' @param task a task
##' @param ... additional arguments
##' @return response from the server
##' @export
rtm_task_method <- function(method, task, ...) {
  rtm_method <- paste0("rtm.tasks.", method)
  args <- lookup_task_method_args(method)
  dots <- list(...)
  
  ## if (length(args) < 1L)
  ##   stop("Invalid rtm.tasks method: ", method)
  if (length(dots) != length(args)) {
    stop("The number of arguments to rtm_task_method() ",
         "do not match parameters to ", method)
  }

  if (length(args) > 0L)
    dots <- stats::setNames(dots, args)

  ## note: not implemented yet, if we only get part of the way through
  ## and lose a connection might want to revert the timeline somehow
  timeline <- rtm_timeline()
  for (i in seq_len(nrow(task))) {
    base_call <- c(list(rtm_method, timeline = timeline,
                        list_id = task[["list_id"]][i],
                        taskseries_id = task[["id"]][i],
                        task_id = task[["task.id"]][i]))

    ## stopped here
    base_call <- c(base_call)
    rsp <- do.call("rtm_req", base_call)
  }
   
  cat(sprintf("Method \"%s\" completed successfully!\n", method))
  invisible(rsp)
}

##' Look up the argument that a method takes
##'
##' @param method the method as a character string
##' @return the argument as a character string
lookup_task_method_args <- function(method) {
  task_method_args()[[method]]
}

##' A list of the arguments to RTM task method calls
##'
##' @return the list
##' @export
task_method_args <- function() {
  list(addTags = "tags",
       movePriority = "direction",
       removeTags = "tags", setDueDate = "due",
       setEstimate = "estimate", setLocation = "location_id",
       setName = "name", setPriority = "priority",
       setRecurrence = "repeat", setTags = "tags",
       setURL = "url", notes.add = c("note_title", "note_text"))
}
  
##' Convert the time from text to UTC via the RTM service.
##'
##' @param text a character string representing text
##' @return a time
rtm_time <- function(text) {
  rsp <- rtm_req("rtm.time.parse", FALSE, text = text)
  if (exists("time", rsp) && exists("$t", rsp[["time"]]))
    return(rsp[["time"]][["$t"]])
  else
    stop("problem getting time from RTM")
}

