context("Working with Task Lists")

test_that("lists are being read properly", {
  expect_equal(get_rtm_list_name("39823464"), "zzz_rtmr_test_list")
})
