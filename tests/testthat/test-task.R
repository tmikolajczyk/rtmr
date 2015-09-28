context("Manipulating Tasks")

test_that(
  "add a task", {
    name <- sprintf("testing %s #zzz_rtmr_test_add", sample.int(1e7, 1))
    expect_output(rtm_add_task(name), "Added task")
  })

test_that(
  "complete a task", {
    skip()
  })

test_that(
  "postpone a task", {
    skip()
  })

test_that(
  "get a list of tasks", {
    expect_is(rtm_search("list:zzz_rtmr_test_list")[[1]], "data.frame")
  })
