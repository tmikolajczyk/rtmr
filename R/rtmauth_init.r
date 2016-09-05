##' Retrieve RTM access token.
##'
##' See:
##' https://www.rememberthemilk.com/services/api/authentication.rtm,
##' which is similar to the OAuth authentication scheme. This code
##' hews closely to httr::init_oauth1.0, to integrate well with that
##' package's Token class functionality.
##'
##' @param endpoint The RTM endpoint, created by 'rtm_endpoint'
##' @param app The RTM consumer application, created by 'rtm_app'
##' @param permission optional, a string of permissions to ask for
##' @param is_interactive Is the current environment interactive?
##' @param private_key not used
##' @return the RTM json content response
##' @import httr
##' @export
init_rtmauth <- function(endpoint, app, permission = "read",
                         is_interactive = interactive(),
                         private_key = NULL) {

  ## rtmauth_sig <- function(url, token = NULL, frob = NULL, ...) {
  ##   c(compact(c(parse_url(url)$query, list(frob = frob), list(..., callback = oauth_callback()))),
  ##     rtmauth_signature(url, app, token, frob, ..., callback = oauth_callback()))
  ## }

  ## 1. Get an unauthorized request frob
  response <- POST(endpoint$request, query = rtmauth_signature(
    endpoint$request, app, format = "json")) #, callback = oauth_callback()))
  stop_for_status(response)
  params <- content(response, type = "application/json")
  frob <- params$rsp$frob

  ## 2. Authorize the frob
  authorize_url <-  modify_url(endpoint$authorize, query = rtmauth_signature(
    endpoint$authorize, app, frob = frob, perms = permission))
  verifier <- rtmauth_listener(authorize_url, is_interactive)

  # 3. Request access token
  response <- POST(endpoint$access, query = rtmauth_signature(
    endpoint$access, app, frob = frob, format = "json"))
  stop_for_status(response)
  content(response, type = "application/json")$rsp
}

##' Open a browser and wait for user to indicate authorization.
##'
##' @param request_url the url to send to the browser
##' @param is_interactive is an interactive environment available
##' @return user entry value
rtmauth_listener <- function(request_url, is_interactive = interactive()) {
  if (!is_interactive) {
    stop("rtmauth_listener() needs an interactive environment.", call. = FALSE)
  }
  httr::BROWSE(request_url)
  message("Waiting for authentication in browser...")
  ret <- readline("Press enter to continue, or Esc/Ctrl + C to abort")
  ret
}
