##' Make an HTTP GET request of the RTM service.
##'
##' @param method the RTM method
##' @param ... additional parameters
##' @param simplify use jsonlite simplify and flatten options
##' @return a list or data frame, as parsed by jsonlite::fromJSON
rtm_req <- function(method, ..., simplify = TRUE) {

  url <- "https://api.rememberthemilk.com/services/rest/"

  response <- rGET(url, config = rtm_token(),
                   query = list(method = method, ..., format = "json"))
  httr::stop_for_status(response)

  rtm_response(jsonlite::fromJSON(httr::content(response, type = "application/json",
                                                as = "text", encoding = "utf8"),
                                  simplifyVector = simplify)$rsp)
}

rtm_response <- function(response) {
  structure(response, class = "rtm_response")
}

print.rtm_response <- function(x, completed = FALSE, ...) {
  cat("<rtm_response>\n")
  cat(x$stat, "\n")
  if (x$stat == "ok")
    utils::str(x$tasks$list, max.level = 2)
}

as.rtm_response <- function(x) UseMethod("as.rtm_response")
as.rtm_response.list <- function(x) structure(x, class = "rtm_response")

is.rtm_response <- function(x) inherits(x, "rtm_response")
