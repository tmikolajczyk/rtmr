taskseries <- function(series = NULL, tasks = NULL, notes = NULL, tags = NULL,
                       participants = NULL, rrule = NULL,
                       filter = ~is.na(completed) & is.na(deleted),
                       order = ~desc(due, name)) {
  as.taskseries(
    list(series = series, tasks = tasks, notes = notes,
         tags = tags, participants = participants, rrule = rrule),
    filter = filter, order = order)
}

as.taskseries <- function(x, ...) UseMethod("as.taskseries")
as.taskseries.list <- function(x, filter = ~is.na(completed) & is.na(deleted),
                               order = ~desc(due, name)) {
  structure(x, class = "taskseries", filter = filter, order = order,
            .ids = order_tasks_init(x$tasks, x$series, filter, order))
}
as.taskseries.taskseries <- function(x) x
as.taskseries.NULL <- function(x) taskseries()

as.taskseries.rtm_response <- function(x) {
  if (x$stat == "ok") {
    create_taskseries(x$tasks$list$taskseries)
  } else {
    stop("status not ok")
  }
}

is.taskseries <- function(x) inherits(x, "taskseries")

c.taskseries <- function(...) purrr::reduce(list(...), taskseries_combine)

create_taskseries <- function(tsk_df, svar = "series_id") {

  select_cols <- function(cols) {
    ## case with a single task id is a list, not a data.frame
    if (!is.data.frame(tsk_df)) {
      if (length(cols) > 1) {
        tsk_df <- quickdf(tsk_df[c("id", cols)])
      } else {
        tsk_df <- dplyr::bind_cols(quickdf(tsk_df["id"]),
                                   list(.col = list(quickdf(purrr::flatten(tsk_df[cols]))))) %>%
          dplyr::rename_(.dots = stats::setNames(".col", cols))
      }
    }
    dplyr::rename_(tsk_df, .dots = stats::setNames("id", svar)) %>%
      dplyr::select_(.dots = c(svar, cols))
  }

  bind_data_frame <- function(svar_col, df_col) {
    dplyr::bind_cols(list(.svar = svar_col), df_col) %>%
      dplyr::rename_(.dots = stats::setNames(".svar", svar))
  }

  unnest_list <- function(df, col, fun) {
    dplyr::mutate_(df, .dots = (~f(c)) %>%
                         lazyeval::interp(c = as.name(col), f = as.name(fun)) %>%
                         list %>% stats::setNames(col)) %>%
      tidyr::unnest_(col)
  }

  fix_unnest <- function(df, col, fun = NULL) {
    if (is.null(fun))
      df
    else if (is_list(df[[col]])) #!is.data.frame(df[[col]])) {
      unnest_list(df, col, fun)
    else if (is.data.frame(df[[col]][[1]]))
      bind_data_frame(df[[svar]], df[[col]][[1]])
    else
      bind_data_frame(df[[svar]], df[[col]])
  }

  ensure_named_df <- function(df, .names = NULL) {
    if (is.null(.names) || length(names(df)) > 1) df else named_quickdf(c("series_id", .names))
  }

  list2df <- function(col_nest) purrr::map(col_nest, quickdf)
  list2df2 <- function(col_nest) purrr::map(col_nest, ~ quickdf(purrr::flatten(.)))
  
  ts_df <- function(s_cols, fun = NULL, t_cols = NULL, final = NULL) {
    
    if (length(setdiff(s_cols, names(tsk_df))) == 0) {
      select_cols(s_cols) %>%
        fix_unnest(s_cols, fun) %>%
        ensure_named_df(final) %>%
        convert_time(t_cols) %>%
        as.taskseries_df
    } else {
      taskseries_df()
    }
  }

  taskseries(
    series = ts_df(c("created", "modified", "name", "source", "url",
                     "location_id"),
                   t_cols = c("created", "modified"),
                   final = c("created", "modified", "name", "source", "url",
                             "location_id")),
    tasks = ts_df("task", "list2df", c("due", "added", "completed", "deleted"),
                  c("due", "has_due_time", "added", "completed",
                    "deleted", "priority", "postponed", "estimate")),
    notes = ts_df("notes", "list2df2", c("created", "modified"),
                  c("id", "created", "modified", "title", "$t")),
    tags = ts_df("tags", "list2df", final = "tag"),
    participants = ts_df("participants", "list2df2",
                         final = c("fullname", "username")),
    rrule = ts_df("rrule", "list2df",
                  final = c("every", "$t"))
  )
}

convert_time <- function(df, cols) {
  if (length(intersect(cols, names(df))) == 0)
    df
  else
    dplyr::mutate_at(df, cols, rtm_date)
}

taskseries_combine <- function(x, y) do.call("taskseries", Map(rbind, x, y))

rtm_list <- function(list_name) {
  rtm_req("rtm.tasks.getList",
          list_id = rtm_lists()$id[match(list_name, rtm_lists()$name)]) %>%
    as.taskseries
}

  
current_lists <- function(smt = FALSE) {
  dplyr::filter_(rtm_lists(), ~ (archived == 0) & (smart == 0 | xor(!smt, smart == 1)))$name
}

store_lists <- function(rls) stats::setNames(purrr::map(rls, rtm_list), rls)

sample_lists <- function(n = 10) store_lists(sample(current_lists(), n))

tasks_due <- function(tsrs, .f) tasks_df_due(tsrs$tasks, .f)

tasks_order <- function(tsrs, .f = attr(tsrs, "filter"), .o = attr(tsrs, "order")) {
  tasks_due(tsrs, .f = .f) %>%
    join_name(tsrs$series, .o)
}

join_name <- function(df, tsrs_df, .o) {
  dplyr::left_join(df, dplyr::select_(tsrs_df, ~series_id, ~name),
                   by = "series_id") %>%
    dplyr::arrange_(.o)
}

tasks_df_due <- function(tsrs_df, .f) {
  dplyr::filter_(tsrs_df, .f) %>%
  dplyr::group_by_(~series_id, ~has_due_time) %>%
    dplyr::summarize_(due = ~max(due)) %>%
    dplyr::ungroup()
}

order_tasks_init <- function(tasks, series, .f, .o) {
  join_name(tasks_df_due(tasks, .f), series, .o) %>%
    dplyr::mutate_(name = ~`class<-`(name, c("taskseries_info", "character")))
}

named_quickdf <- function(.names = NULL) {
  quickdf(setNames(rep(list(character()), length(.names)), .names))
}

## library\(lintr\)\;try\(lint\(commandArgs\(TRUE\)\,\ cache\=TRUE\,\ default_linters\)\)
