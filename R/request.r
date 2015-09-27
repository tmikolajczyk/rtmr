##' Make an HTTP GET request of the RTM service.
##'
##' This API does not limit requests but note that the service
##' asks: "Please keep requests to an average of 1 request
##' per second. We may throttle certain API keys (or block them
##' entirely) if they utilize too many resources."
##'
##' https://www.rememberthemilk.com/services/api/
##' 
##' @param method name of the RTM method service being requested
##' @param auth logical value indicating whether to include an
##'     authentication parameter
##' @param ... additional query parameters, must be named
##' @return the response
##' @export
##' @examples
##' 
##' response <- rtm_req("rtm.test.echo", auth = FALSE, moo = "bar")
##' response[["moo"]] # bar
##'
##' rtm_req("rtm.tasks.getList", filter = "dueWithin:\"5 days\"")
##' 
rtm_req <- function(method, auth = TRUE, ...) {
  url <- "https://api.rememberthemilk.com/"
  path <- "services/rest/"
  
  ## construct query
  query <- c(list(method = method, api_key = api_key(),
                  format = "json"), list(...))
  if (auth)
    query <- c(query, list(auth_token = rtm_pat()))
  query <- c(query, list(api_sig = sign_request(query)))
  
  response <- httr::GET(url, path = path, query = query)
  check_http_response(response)
  
  response_text <- httr::content(response, as = "text")
  check_json_response(response_text)
  
  response_rtm <- jsonlite::fromJSON(response_text)
  check_rtm_response(response_rtm)
  
  ret <- response_rtm[["rsp"]]
  class(ret) <- "rtm_response"
  ret
}

##' Sign an API request with a shared secret and MD5 hash according
##' to the rule at
##' https://www.rememberthemilk.com/services/api/authentication.rtm
##'
##' @param query a list of all query parameters
##' @return a hexadecimal string value
sign_request <- function(query) {
  q_order <- order(names(query))
  ## order query parameters alphabetically
  ## by key name, concatenate key/value pairs
  pairs_str <- paste0(names(query)[q_order],
                  unlist(query)[q_order],
                  collapse = "")
  ## prepend the shared secret
  ss_pairs_str <- paste0(shared_secret(), pairs_str)
  ## calculate MD5 hash
  digest::digest(ss_pairs_str, algo = "md5", serialize = FALSE)
}
