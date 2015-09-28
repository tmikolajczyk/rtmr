##' Request a new timeline.
##'
##' @return a timeline id character string
rtm_timeline <- function() {
  rsp <- rtm_req("rtm.timelines.create")
  rsp[["timeline"]]
}
