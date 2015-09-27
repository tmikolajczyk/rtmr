##' Authenticate a user with the RTM API.
##' 
##' See https://www.rememberthemilk.com/services/api/authentication.rtm
##' for details.
##' @param perms the permissions the user will have, in increasing order
##'     "read", "write", "delete"
##' @return an authorization token if successful
##' @export
get_auth <- function(perms = "delete") {
  while (TRUE) {
    ## request a Frob
    response <- rtm_req("rtm.auth.getFrob", auth = FALSE);
    
    ## construct a url out of it and other specified parameters
    query <- list(api_key = api_key(), perms = perms,
                  frob = response[["frob"]]);
    query <- c(query, list(api_sig = sign_request(query)));
    auth_url <- httr::modify_url("http://www.rememberthemilk.com/",
                                 path = "services/auth/", query = query)

    ## point the user to the url
    message("Please authenticate in a browser.\n",
            "If the site does not appear automatically, ",
            "copy the link below into a web browser.\n",
            auth_url, "\n");
    browseURL(auth_url);
    invisible(readline(prompt = "Press Enter when finished\n"));

    ## request a token once the user has (presumably) authorized access
    token_rsp <- rtm_req("rtm.auth.getToken",
                         auth = FALSE, frob = response[["frob"]]);

    ## rinse and repeat if necessary
    if (status(token_rsp) == "fail") {
      message("Hmm, did not receive authentication. ",
              "Perhaps you pressed enter too soon?\n");
      choice <- readline(prompt =
                           "Press enter to try authenticating again or \"q\" to quit.\n")
      if (choice %in% c("q", "Q"))
        break
      else
        next
    }
    break
  }
  message("Authentication complete.",
          sprintf("Thank you %s.\n",
                  token_rsp[["auth"]][["user"]][["fullname"]]),
          sprintf("Your personal authentication token (PAT) is:\n\n%s\n",
                  token_rsp[["auth"]][["token"]]));
  token_rsp$auth
}

##' Retrieves the personal authentication token
##'
##' @param force force entry of new PAT
##' @return a character string
##' @export
rtm_pat <- function(force = FALSE) {
  env <- Sys.getenv('RTM_PAT')
  if (!identical(env, "") && !force) return(env)

  ## if (interactive()) {
  ## }
  
  stop("Couldn't find your RTM_PAT environment variable. ",
       "Please set it to your RTM personal authentication token, ",
       "preferably in your .Renviron file in your home folder.\n",
       "See ?rtm_pat for more details.", call. = FALSE)
}

##' The rtmr API key.
##'
##' @return a string
api_key <- function() {
  "baafc86bb439540c55070f890e251b69"
}

##' The rtmr shared secret.
##'
##' @return a string
shared_secret <- function() {
  "e41a2019dae2ab81"
}

##' Checks with the API to see whether the user's PAT is still valid.
##'
##' from the website: "\code{auth_token}'s can and do expire
##' (for example, if the user revokes the permissions they granted to
##' your application)"
##' 
##' @return logical indicating whether the token is still valid or not 
verify_token <- function() {
  response <- rtm_req("rtm.auth.checkToken")
  if (response[["stat"]] == "ok")
    return(TRUE)
  else if (response[["stat"]] == "fail")
    return(FALSE)
}
