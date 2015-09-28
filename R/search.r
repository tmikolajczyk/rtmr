##' Search tasks using the RTM advanced search feature.
##'
##' See https://www.rememberthemilk.com/help/?ctx=basics.search.advanced
##' for specific parameters.
##' 
##' @param filter the search terms passed to RTM advanced search
##' @return a list of data frames contained in the response from rtm_req
##'     for an advanced search
##' @export
##' @examples
##'
##' rtm_search("list:Shopping")
##' 
##' rtm_search("priority:1 list:Work")
rtm_search <- function(filter) {
  rsp <- rtm_req("rtm.tasks.getList", filter = filter)
  if (exists("tasks", rsp) && !exists("list", rsp[["tasks"]])) {
    cat("No tasks matching your query\n")
    return(list())
  } else if (exists("list", rsp[["tasks"]]) &&
             exists("taskseries", rsp[["tasks"]][["list"]])) {
    df_list <- rsp[["tasks"]][["list"]][["taskseries"]]
  } else {
    stop("unable to find task info")
  }

  list_ids <- rsp[["tasks"]][["list"]][["id"]]
  list_names <- get_rtm_list_name(list_ids)
  processed_dfs <- lapply(df_list, filter_task_info)

  for (i in seq_along(processed_dfs)) {
    processed_dfs[[i]][["list_id"]] <- list_ids[i]
    processed_dfs[[i]][["list_name"]] <- list_names[i]
  }
  plyr::ldply(processed_dfs)
}
