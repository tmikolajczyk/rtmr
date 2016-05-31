##' HTTP exponential backoff.
##'
##' Copied from jennybc's googlesheets on github. Used to avoid rate
##' limit issues:Let's see if this works best, or if directly limiting
##' to 1/sec or max 3/sec somehow would be more effective.
##'
##' https://www.rememberthemilk.com/services/api/ratelimit.rtm
##'
##' @param VERB original function
##' @param n max times
##' @return function
VERB_n <- function(VERB, n = 5) {
  function(...) {
    for (i in seq_len(n)) {
      out <- VERB(...)
      status <- httr::status_code(out)
      if (status < 500 || i == n) break
      backoff <- stats::runif(n = 1, min = 0, max = 2 ^ i - 1)
      mess <- paste("HTTP error %s on attempt %d ...\n",
                    "  backing off %0.2f seconds, retrying")
      mpf(mess, status, i, backoff)
      Sys.sleep(backoff)
    }
    out
  }
}

rGET <- VERB_n(httr::GET)
