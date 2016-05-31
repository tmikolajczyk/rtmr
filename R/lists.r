##' Get the RTM list name from an id.
##'
##' @param id the RTM list id
##' @return the list name
##' @export
get_rtm_list_name <- function(id) {
  lists <- rtm_lists()
  as.character(setNames(lists$name, lists$id)[id])
  ## ids <- purrr::map_chr(lists_info, "id")
  ## listnames <- purrr::map_chr(lists_info, "name")
  ## as.character(setNames(listnames, ids)[id])
}

##' Retrieve a list of task lists from the RTM API.
##'
##' @return a list of lists
rtm_lists <- function() {
  if (!exists("lists", envir = .state))
    .state$lists <- rtm_req("rtm.lists.getList")$lists$list
  .state$lists
  ## if (exists("lists", lists) &&
  ##     exists("list", lists[["lists"]]))
  ##   lists[["lists"]][["list"]]
  ## else
  ##   stop("problem retrieving RTM lists")
}

##' Get a list id from a name.
##'
##' @param name a list name
##' @return a list id as a character string
get_rtm_list_id <- function(name) {
  lists <- rtm_lists()
  lists[["id"]][lists[["name"]] == name]
}
