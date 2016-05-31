# environment to store credentials
.state <- new.env(parent = emptyenv())

rtmauth_app <- function(key, secret) oauth_app(appname = "rtm", key, secret)

rtm_auth <- function(token = NULL,
                     new_user = FALSE,
                     key = getOption("rtmr.api_key"),
                     secret = getOption("rtmr.shared_secret"),
                     cache = getOption("httr_oauth_cache"),
                     verbose = FALSE) {
  if (new_user) {
    rtm_deauth()
  }

  if (is.null(token)) {
    rtm_app <- rtmauth_app(key = key, secret = secret)
    rtm_token <- rtmauth_token(rtmauth_endpoint(), rtm_app)
    stopifnot(is_legit_token(rtm_token, verbose = verbose))
    .state$token <- rtm_token
  } else if (inherits(token, "TokenRTM")) {
    stopifnot(is_legit_token(token, verbose = verbose))
    .state$token <- token
  } else if (inherits(token, "character")) {
    token <- try(suppressWarnings(readRDS(token)), silent = TRUE)
    if (inherits(rtm_token, "try-error")) {
      spf("Cannot read token from alleged .rds file:\n%s", token)
    } else if (!is_legit_token(rtm_token, verbose = TRUE)) {
      spf("File does not contain a proper token:\n%s", token)
    }
    .state$token <- rtm_token
  } else {
    spf("Input provided via 'token' is neither a",
        "token,\nnor a path to an .rds file containing a token.")
  }
  invisible(.state$token)
}


rtm_deauth <- function(clear_cache = TRUE, verbose = TRUE) {

  if (clear_cache && file.exists(".httr-oauth")) {
    if (verbose) {
      message("Disabling .httr-oauth by renaming to .httr-oauth-SUSPENDED")
    }
    file.rename(".httr-oauth", ".httr-oauth-SUSPENDED")
  }

  if (token_available()) {
    if (verbose) {
      message("Removing rtm token stashed internally in 'rtmr'.")
    }
    rm("token", envir = .state)
  } else {
    message("No token currently in force.")
  }
  invisible(NULL)
}

rtm_token <- function(verbose = FALSE) {
  if (!token_available()) rtm_auth(verbose = verbose)
  httr::config(token = .state$token)
}

token_available <- function(verbose = FALSE) !is.null(.state$token)

is_legit_token <- function(x, verbose = FALSE) {
  
  if (!inherits(x, "TokenRTM")) {
    if (verbose) message("Not a TokenRTM object.")
    return(FALSE)
  }

  if (x$credentials$stat != "ok") {
    if (verbose) {
      message("RTM Status error")
    }
    return(FALSE)
  }

  ## need to fill these in with other error messages
  ## if ("invalid_client" %in% unlist(x$credentials)) {
  ##   # shouldn't happen if id and secret are good
  ##   if (verbose) {
  ##     message("Authorization error. Please check client_id and client_secret.")
  ##   }
  ##   return(FALSE)
  ## }

  ## if ("invalid_request" %in% unlist(x$credentials)) {
  ##   # in past, this could happen if user clicks "Cancel" or "Deny" instead of
  ##   # "Accept" when OAuth2 flow kicks to browser ... but httr now catches this
  ##   if (verbose) message("Authorization error. No access token obtained.")
  ##   return(FALSE)
  ## }

  TRUE

}
