##' Request a new timeline.
##'
##' @return a timeline id character string
rtm_timeline <- function() {
  rtm_req("rtm.timelines.create")
}
