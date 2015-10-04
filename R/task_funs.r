##' Add a task.
##'
##' @param name the name of the task including all Smart Add tags
##' @return response from the server
##' @export
add_task <- function(name) {
  ## parse is 1 because we always use Smart Add, for now
  rsp <- rtm_req("rtm.tasks.add", name = name,
                 timeline = rtm_timeline(), parse = "1")
  cat("Added task\n")
  invisible(rsp)
}

##' Move a task from one list to another
##'
##' @param task the task
##' @param to_list_name the name of the list to move it to
##' @return response from the server
##' @export
move_to <- function(task, to_list_name) {
  to_list_id <- get_rtm_list_id(to_list_name)
  if (length(to_list_id) == 0L)
    stop("There's no list called ", to_list_name,
         " to move the task to")
  rsp <- rtm_req("rtm.tasks.moveTo",
                 timeline = rtm_timeline(),
                 from_list_id = task[["list_id"]][1],
                 to_list_id = to_list_id,
                 taskseries_id = task[["id"]][1],
                 task_id = task[["task.id"]][1])
  cat("Method \"moveTo\" completed successfully!\n")
  invisible(rsp)
}

##' Calls corresponding RTM task method
##'


##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param direction direction
##' @return response from the server
##' @export
move_priority <- function(task, direction) {
  rtm_task_method("movePriority", task, direction)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param tags tags
##' @return response from the server
##' @export
remove_tags <- function(task, tags) {
  rtm_task_method("removeTags", task, tags)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param due due date
##' @return response from the server
##' @export
set_due_date <- function(task, due) {
  rtm_task_method("setDueDate", task, due)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param estimate time estimate
##' @return response from the server
##' @export
set_estimate <- function(task, estimate) {
  rtm_task_method("setEstimate", task, estimate)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param location_id location id
##' @return response from the server
##' @export
set_location <- function(task, location_id) {
  rtm_task_method("setLocation", task, location_id)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param name the task name
##' @return response from the server
##' @export
set_name <- function(task, name) {
  rtm_task_method("setName", task, name)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param priority priority
##' @return response from the server
##' @export
set_priority <- function(task, priority) {
  rtm_task_method("setPriority", task, priority)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param `repeat` recurrence
##' @return response from the server
##' @export
set_recurrence <- function(task, `repeat`) {
  rtm_task_method("setRecurrence", task, `repeat`)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param tags tags
##' @return response from the server
##' @export
set_tags <- function(task, tags) {
  rtm_task_method("setTags", task, tags)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param url url
##' @return response from the server
##' @export
set_url <- function(task, url) {
  rtm_task_method("setURL", task, url)
}

##' Calls corresponding RTM task method
##'
##' @param task the task
##' @param note_title note title
##' @param note_text note text
##' @return response from the server
##' @export
notes_add <- function(task, note_title, note_text) {
  rtm_task_method("notes.add", task, note_title, note_text)
}
