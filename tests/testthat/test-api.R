context("Basic API functionality")

test_that(
  "send an echo request", {
    # skip_if_not(internet)
    response <- rtm_req("rtm.test.echo", auth = FALSE, moo = "bar")
    
    expect_equal(response[["moo"]], "bar")
  })

test_that(
  "send an improper request", {
    ## skip_if_not(internet)
    expect_error(rtm_req("xyz", auth = FALSE), regexp = "RTM error 112")
  })
