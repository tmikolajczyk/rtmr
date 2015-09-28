##' Get the timeline in use or request a new one.
##'
##' @return a timeline id character string
rtm_timeline <- function() {
  if (!exists(".rtm_timeline", globalenv()))
    rtm_cache_timeline()
  get(".rtm_timeline", envir = globalenv())
}

##' Request a new timeline and store it in the global environment.
##'
##' @param varname the name of the global variable to store it in
##' @param envir the environment, by default the global environment
rtm_cache_timeline <- function(varname = ".rtm_timeline",
                               envir = globalenv()) {
  rsp <- rtm_req("rtm.timelines.create")
  assign(varname, rsp[["timeline"]], envir = envir)
}
