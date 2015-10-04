##' Create a new list. Create a Smart List if a filter is provided.
##'
##' @param name the list name
##' @param filter criteria for a Smart List
##' @return respose from RTM server
##' @export
add_list <- function(name, filter = NULL) {
  timeline <- rtm_timeline()
  if (is.null(filter))
    rtm_req("rtm.lists.add", timeline, name = name)
  else
    rtm_req("rtm.lists.add", timeline, name = name, filter = filter)
  cat("Successfully added list!")
}

##' Perform the list operation
##'
##' @param list_id the list id
##' @return response from RTM server
##' @export
delete_list <- function(list_id) {
  rtm_req("rtm.lists.delete", timeline = rtm_timeline(), list_id = list_id)
}

##' Perform the list operation
##'
##' @param list_id the list id
##' @return response from RTM server
##' @export
archive_list <- function(list_id) {
  rtm_req("rtm.lists.archive", timeline = rtm_timeline(), list_id = list_id)
}

##' Perform the list operation
##'
##' @param list_id the list id
##' @param name the list name
##' @return response from RTM server
##' @export
set_name_list <- function(list_id, name) {
  rtm_req("rtm.lists.setName", timeline = rtm_timeline(),
          list_id = list_id, name = name)
}

##' Perform the list operation
##'
##' @param list_id the list id
##' @return response from RTM server
##' @export
unarchive_list <- function(list_id) {
  rtm_req("rtm.lists.unarchive", timeline = rtm_timeline(), list_id = list_id)
}
