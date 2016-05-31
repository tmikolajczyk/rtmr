##' The RTM request, authorize and access endpoints.
##'
##' See oauth_endpoint.
##' @return the RTM endpoint
rtmauth_endpoint <- function() {
  httr::oauth_endpoint(request = "?method=rtm.auth.getFrob",
                       authorize = "https://www.rememberthemilk.com/services/auth/",
                       access = "?method=rtm.auth.getToken",
                       base_url = "https://api.rememberthemilk.com/services/rest")
}
