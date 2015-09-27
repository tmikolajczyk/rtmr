##' Cache the list of RTM lists so we only need it once per R session.
##'
##' @param envir environment to store it in, should be the global
rtm_cache_lists <- function(varname = ".rtm_lists_info",
                            envir = globalenv()) {
  rtm_lists <- rtm_req("rtm.lists.getList")
  if (exists("lists", rtm_lists) &&
      exists("list", rtm_lists[["lists"]]))
    assign(varname, rtm_lists[["lists"]][["list"]], envir = envir)
  else
    stop("problem caching RTM lists info")
}

##' Get the RTM list name from an id.
##'
##' @param id the id
##' @return a list name
get_rtm_list_name <- function(id) {
  if (!exists(".rtm_lists_info", globalenv()))
    rtm_cache_lists()
  lists_info <- get(".rtm_lists_info", envir = globalenv())
  as.character(setNames(lists_info[["name"]], lists_info[["id"]])[id])
}
