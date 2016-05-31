##' Main function to call to get an RTM token.
##'
##' Modeled after oauth1.0_token, uses the httr::Token class.
##' 
##' @param endpoint The RTM endpoint
##' @param app the RTM app
##' @param permission a string of permissions to ask for
##' @param private_key not used
##' @param cache uses the cache functionality of the Token class
##' @return A TokenRTM reference class object.
##' @export
rtmauth_token <- function(endpoint, app, permission = "read",
                           private_key = NULL,
                           cache = getOption("httr_oauth_cache")) {
  params <- list(permission = permission)

  TokenRTM$new(app = app, endpoint = endpoint, params = params,
    private_key = private_key, cache_path = cache)
}

#' RTMAuth token objects.
#'
#' See httr::Token for more info.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @name Token-class
TokenRTM <- R6::R6Class("TokenRTM", inherit = Token, list(
  init_credentials = function(force = FALSE) {
    self$credentials <- init_rtmauth(self$endpoint, self$app,
      permission = self$params$permission, private_key = self$private_key)
  },
  can_refresh = function() {
    FALSE
  },
  refresh = function() {
    stop("Not implemented")
  },
  sign = function(method, url) {
    rtmauth <- rtmauth_signature(url, self$app, self$credentials$auth$token)
    url <- parse_url(url)
    url$query <- rtmauth
    request(url = build_url(url))
    # rtmauth_signature(url, self$app, self$credentials$auth$token)
  }
))

request <- function(method = NULL, url = NULL, auth_token = NULL) {
  if (!is.null(method))
    stopifnot(is.character(method), length(method) == 1)
  if (!is.null(url))
    stopifnot(is.character(url), length(url) == 1)

  structure(
    list(
      method = method,
      url = url,
      headers = NULL,
      fields = NULL,
      options = NULL,
      auth_token = auth_token,
      output = NULL
    ),
    class = "request"
  )
}
