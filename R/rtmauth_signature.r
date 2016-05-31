##' Sign parameters for an RTM request.
##'
##' See "Signing Requests" at:
##' https://www.rememberthemilk.com/services/api/authentication.rtm
##'
##' Follows the design of httr::oauth_signature.
##' 
##' @param url Url of request
##' @param app RTM app object
##' @param frob frob
##' @param ... additional parameters
##' @return list object of length two containing the signature and api_key
rtmauth_signature <- function(url, app, token = NULL, frob = NULL, ...) {
  url <- parse_url(url)
  rtmrauth <- compact(list(api_key = app$key, auth_token = token, frob = frob))
  other_params <- compact(list(...))
  rtmrauth <- c(rtmrauth, other_params)
  params <- c(url$query, rtmrauth)
  ## note: there's no documentation on this, but we run into problems
  ##  if we escape ':', and probably other chars, when doing a
  ##  rtm.tasks.getList query, so this is commented out
  # params_esc <- stats::setNames(curl::curl_escape(params),
  #                               curl::curl_escape(names(params)))
  ## 1. sort parameters by key name
  # params_srt <- sort_names(params_esc)
  params_srt <- sort_names(params)
  # 2. concatenate all key/value pairs together
  params_str <- paste0(names(params_srt), params_srt, collapse = "")
  # 3. concatenate onto shared secret
  base_string <- paste0(app$secret, params_str)
  # 4. calculate the md5 hash of this string
  c(params, list(api_sig = openssl::md5(base_string)))
}

## code taken from httr::oauth_encode
rtmauth_encode <- function(x) vapply(x, rtmauth_encode1, character(1))

# As described in http://tools.ietf.org/html/rfc5849#page-29
# code taken from httr::oauth_encode1
rtmauth_encode1 <- function(x) {
  encode <- function(x) paste0("%", toupper(as.character(charToRaw(x))))

  x <- as.character(x)
  chars <- strsplit(x, "")[[1]]
  ok <- !stringr::str_detect(chars, "[^A-Za-z0-9_.~-]")

  if (all(ok)) return(x)

  chars[!ok] <- unlist(lapply(chars[!ok], encode))
  paste0(chars, collapse = "")
}
