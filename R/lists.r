##' Get the RTM list name from an id.
##'
##' @param id the RTM list id
##' @return the list name
##' @export
get_rtm_list_name <- function(id) {
  lists_info <- rtm_lists()
  as.character(setNames(lists_info[["name"]], lists_info[["id"]])[id])
}

##' Retrieve a list of lists from the RTM API.
##'
##' @return a list of lists
rtm_lists <- function() {
  rtm_lists <- rtm_req("rtm.lists.getList")
  if (exists("lists", rtm_lists) &&
      exists("list", rtm_lists[["lists"]]))
    rtm_lists[["lists"]][["list"]]
  else
    stop("problem retrieving RTM lists")
}
