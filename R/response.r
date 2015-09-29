##' Check the HTTP server response, throw an error if needed.
##'
##' @param rsp object returned from \code{httr::GET()}
check_http_response <- function(rsp) {
  if (rsp[["status_code"]] < 400) return(invisible())
  text <- httr::content(rsp, as = "text")
  if (!identical(text, ""))
    text <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  stop("HTTP error: ", rsp[["status_code"]], "\n", text, call. = FALSE)
}

##' Check the JSON sent by the server.
##'
##' @param rsp text presumably in JSON format
check_json_response <- function(rsp) {
  if (!jsonlite::validate(rsp))
    stop("JSON error: Invalid JSON response from RTM server.")
  return(invisible())
}

##' Check for an RTM fail.
##'
##' @param rsp JSON structured data
check_rtm_response <- function(rsp) {
  json_inconsistent <- "JSON valid but not consistent with RTM scheme."
  if (!exists("rsp", rsp) || (!exists("stat", rsp[["rsp"]])))
    stop(json_inconsistent)
  rsp <- rsp[["rsp"]]
  class(rsp) <- "rtm_response"
  if (status(rsp) == "fail")
    stop(sprintf("RTM error %s: %s", rsp[["err"]][["code"]],
                 rsp[["err"]][["msg"]]))
  else
    return(invisible())
}

##' Get the status of a response.
##'
##' @param x an object
##' @return "ok" or "fail"
status <- function(x) {
  UseMethod("status")
}

##' Should not be called
##'
##' @param x an object
##' @return doesn't return
status.default <- function(x) {
  stop("not a rtm_response object")
}

##' Get the status of a response
##'
##' @param x an rtm_response object
##' @return "ok" or "fail"
status.rtm_response <- function(x) {
  if (x$stat == "ok" || x$stat == "fail")
    x$stat
  else
    stop("rtm_response error: invalid status value")
}
