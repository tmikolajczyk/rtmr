context("Authentication")

## test_that(
##   "cause error when no PAT set", {
##     skip("until we can test this another way")
##     Sys.unsetenv("RTM_PAT")
##     expect_error(rtm_req("rtm.contacts.getList"),
##                  "Couldn't find your RTM_PAT")
##   })
