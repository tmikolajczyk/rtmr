##' Search tasks using the RTM advanced search feature.
##'
##' See https://www.rememberthemilk.com/help/?ctx=basics.search.advanced
##' for specific parameters.
##' 
##' @param filter 
##' @return a list of data frames contained in the response from rtm_req
##'     for an advanced search
##' @export
##' @examples
##'
##' rtm_search("list:Shopping")
##' 
##' rtm_search("priority:1")
rtm_search <- function(filter) {
  rsp <- rtm_req("rtm.tasks.getList", filter = filter)
  if (exists("tasks", rsp) &&
      exists("list", rsp[["tasks"]]) &&
      exists("taskseries", rsp[["tasks"]][["list"]]))
    df_list <- rsp[["tasks"]][["list"]][["taskseries"]]
  else
    stop("unable to find task info")

  names(df_list) <- get_rtm_list_name(rsp[["tasks"]][["list"]][["id"]])
  
  lapply(df_list, filter_task_info)
}
