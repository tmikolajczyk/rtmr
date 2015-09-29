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

test_that(
  "add/edit/delete notes", {
    ## create a note
    r_int <- sample.int(1e7, 1)
    name <- sprintf("test task w note %s #zzz_rtmr_test_add", r_int)
    
    expect_output(rtm_add_task(name), "Added task")
    
    zzz_tasks <- rtm_search("list:zzz_rtmr_test_add")
    task <- zzz_tasks[grep(paste0(r_int), zzz_tasks[["name"]]),]
    note_title <- sprintf("test note title %s", r_int)
    note_text <- sprintf("this is a test note %s", r_int)
    
    expect_output(rtm_add_note(task, note_title, note_text),
                  "Method \"notes.add\" completed")
  })
