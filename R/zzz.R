.onLoad <- function(libname, pkgname) {

  op <- options()

  op.rtmr <- list(
    rtmr.api_key = "76cba4e4cb0030c326ad3764db43e950",
    rtmr.shared_secret =  "4e0da78b2b46863b"
  )

  toset <- !(names(op.rtmr) %in% names(op))
  if (any(toset)) options(op.rtmr[toset])

}
