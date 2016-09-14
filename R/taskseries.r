flatten_taskseries <- function(tsk_df, svar = "series_id") {

  select_cols <- function(cols) {
    ## case with a single task id
    if (!is.data.frame(tsk_df)) {
      if (length(cols) > 1) {
        tsk_df <- quickdf(tsk_df[c("id", cols)])
      } else {
        tsk_df <- bind_cols(quickdf(tsk_df["id"]),
                            list(.col = list(quickdf(purrr::flatten(tsk_df[cols]))))) %>%
          rename_(.dots = setNames(".col", cols))
      }
    }
    rename_(tsk_df, .dots = setNames("id", svar)) %>%
      select_(.dots = c(svar, cols))
  }

  fix_unnest <- function(df, col, fun) {
    if (is.data.frame(df[[col]])) {
      bind_cols(list(.svar = df[[svar]]), df[[col]]) %>% rename_(.dots = setNames(".svar", svar))
    } else {
      mutate_(df, .dots = (~f(c)) %>%
                    lazyeval::interp(c = as.name(col), f = as.name(fun)) %>%
                    list %>% setNames(col)) %>%
        tidyr::unnest_(col)
    }
  }

  fix_quickdf <- function(ncol) purrr::map(ncol, quickdf)
  fix_quickdf2 <- function(ncol) purrr::map(ncol, ~ purrr::flatten(.x) %>% quickdf)
  
  fix_ts <- function(s_cols, fun = NULL, t_cols = NULL) {
    
    if (length(setdiff(s_cols, names(tsk_df))) == 0) {
      select_cols(s_cols) %>%
        { if (is.null(fun)) . else fix_unnest(., s_cols, fun) } %>%
        { if (is.null(t_cols) || (length(intersect(t_cols, names(.))) == 0)) .
          else dplyr::mutate_at(., t_cols, rtm_date) } %>%
        as.taskseries_df
    } else {
      taskseries_df()
    }
  }
  
  taskseries(
    series = fix_ts(c("created", "modified", "name", "source", "url", "location_id"),
                    t_cols = c("created", "modified")),
    tasks = fix_ts("task", "fix_quickdf", c("due", "added", "completed", "deleted")),
    notes = fix_ts("notes", "fix_quickdf2", c("created", "modified")),
    tags = fix_ts("tags", "fix_quickdf"),
    participants = fix_ts("participants", "fix_quickdf"),
    rrule = fix_ts("rrule", "fix_quickdf")
  )
}

taskseries <- function(series = NULL, tasks = NULL, notes = NULL,
                       tags = NULL, participants = NULL, rrule = NULL)  {
  structure(
    list(
      series = series,
      tasks = tasks,
      notes = notes,
      tags = tags,
      participants = participants,
      rrule = rrule
    ),
    class = "taskseries"
  )
}

taskseries_df <- function(series_id = NULL, ...) {
  structure(
    data.frame(
      series_id = series_id,
      ...,
      stringsAsFactors = FALSE
    ),
    class = c("taskseries_df", "data.frame")
  )
}

as.taskseries_df <- function(x) UseMethod("as.taskseries_df")
as.taskseries_df.taskseries_df <- function(x) x
as.taskseries_df.data.frame <- function(x) {
  if (!exists("series_id", x)) {
    stop("no series_id")
  } else {
    structure(x, class = c("taskseries_df", "data.frame"))
  }
}
as.taskseries_df.NULL <- function(x) taskseries_df()
is.taskseries_df <- function(x) inherits(x, "taskseries_df")


taskseries_combine <- function(x, y) {
  do.call("taskseries", Map(rbind, x, y))
}

print.taskseries <- function(x, completed = FALSE, ...) {
  cat("<taskseries>\n")
  print_taskseries_info(x, ~toString(name, print_due_date(due, has_due_time)))
}

toString.taskseries_info <- function(x, ..., width = 37, min_width = 6) {
  other_str <- paste(..., sep = "")
  width_x <- pmax(min_width, width - nchar(as.character(other_str), type = "width"))

  paste(
    dplyr::if_else(nchar(x, type = "width") > width_x,
                   paste0(strtrim(x, width_x - 3), "..."),
                   as.character(x)),
    ..., sep = "")
}

print_taskseries_info <- function(x, fmla, width = 80) {
  if (!exists("series_id", x$tasks)) {
    print("\n")
  } else {
    filter_(x, ~is.na(tasks$completed) & is.na(tasks$deleted))[["tasks"]] %>%
      # x$tasks %>%
      #    filter_(~is.na(completed) & is.na(deleted)) %>%
      group_by_(~series_id, ~has_due_time) %>%
      summarize_(due = ~max(due)) %>%
      ungroup() %>%
      left_join(select_(x$series, ~series_id, ~name), by = "series_id") %>%
      arrange_(~desc(due)) %>%
      mutate(name = `class<-`(name, c("taskseries_info", "character"))) %>%
      lazyeval::f_eval(fmla, .) %>%
      { withr::with_options(list(width = width), print(., quote = FALSE)) }
  }
}

eval_tseries_info <- function(ti_df, f) {
}

as.taskseries <- function(x) UseMethod("as.taskseries")
as.taskseries.list <- function(x) structure(x, class = "taskseries")
as.taskseries.taskseries <- function(x) x
as.taskseries.NULL <- function(x) taskseries()

as.taskseries.rtm_response <- function(x) {
  if (x$stat == "ok") {
    flatten_taskseries(x$tasks$list$taskseries)
  } else {
    stop("status not ok")
  }
}

is.taskseries <- function(x) inherits(x, "taskseries")

c.taskseries <- function(...) purrr::reduce(list(...), taskseries_combine)

keep_ids <- function(tsrs, ids) {
  as.taskseries(purrr::map(tsrs, ~ .x[.x[["series_id"]] %in% ids, ]))
}

discard_ids <- function(tsksrs, ids) {
  as.taskseries(purrr::map(tsrs, ~ .x[!.x[["series_id"]] %in% ids, ]))
}

is_parenthesized <- function(expr) is.call(expr) && identical(expr[[1L]], quote(`(`))

is_logical_op2 <- function(op, ops) {
  identical(op, as.name(ops[1])) ||
  identical(op, as.name(ops[2]))
}

is_logical_op <- function(op, ops) {
  op == as.name(ops[1]) ||
    op == as.name(ops[2])
}

is_logical_op3 <- function(op, ops) {
  ## much slower, more generic
  Reduce(any, lapply(ops, function(op_i) op == as.name(op_i)))
}
  
## ## ## l()

## periodic_list <- rtm_req("rtm.tasks.getList", list_id = "39866813")
## shopping_list <- rtm_req("rtm.tasks.getList", list_id = "39819437")
## test_list <- rtm_req("rtm.tasks.getList", list_id = "39823464")

## test <- periodic_list$tasks$list$taskseries
## test2 <- shopping_list$tasks$list$taskseries
## test3 <- test_list$tasks$list$taskseries

## per_list <- periodic_list %>% as.taskseries
## saveRDS(per_list, "per_list.rds")
## keep_ids(per_list, c("269048767", "100326223"))
## evalq(series$name == "credit report", envir = list2env(per_list))

# eval_across_plist(~(r(series, "created") < as.POSIXct("2015-10-10")) %&% (r(tasks, "added") > as.POSIXct("2015-08-20")) %&% (r(tasks, "added") < as.POSIXct("2016-01-01")), per_list)

r_closure <- function() {
  
  series_id <- NULL
  
  function(tsdf, col) {
    if (is.null(series_id)) {
      series_id <<- tsdf[["series_id"]]
    } else if (any(series_id != tsdf[["series_id"]])) {
      stop("series_id's are ambiguous")
    }
    tsdf[[col]]
  }
}

eval_plist <- function(expr, plist, r_fun_name = "$.taskseries_df") {
  
  expr <- as_lazy(expr)
  e <- list2env(plist)
  e[[r_fun_name]] <- r_closure()

  lg <- eval(expr, envir = e)  # evaluate expression, (series_ids stored in closure)
  structure(get("series_id", envir = environment(e[[r_fun_name]]))[lg],
            class = "id")
}

eval_across_plist <- function(expr, plist,
                              id_ops = list(`&.id` = intersect, `|.id` = union),
                              rfun_name = "r") {
  expr <- as_lazy(expr)
  rhss <- list()
  ops <- list()

  eval_if_paren <- function(.expr) {
    if (is_parenthesized(.expr))
      .expr <- .expr[[-1L]]
    eval_plist(.expr, plist)
  }

  ops_seen <- gsub("\\..+$", "", names(id_ops))
  
  i <- 1L
  while (is.call(expr) && is_logical_op(expr[[1L]], ops = ops_seen)) {
    ops[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]

    # eval the expression if it's inside parens
    rhss[[i]] <- eval_if_paren(rhs)
    
    expr <- expr[[2L]]
    i <- i + 1L
  }

  rhss <- rev(rhss)
  ops <- rev(ops)
  lhs <- eval_if_paren(expr)
  
  fun_list <- lapply(seq_along(rhss), function(i) list(op = ops[[i]], arg = rhss[[i]]))
  id_env <- list2env(id_ops, parent = parent.frame())
  apply_fl <- function(x, y) list(arg = eval_infix(y$op, x$arg, y$arg, env = id_env))

  Reduce(apply_fl, fun_list, init = list(arg = lhs))$arg
}

eval_infix <- function(op, x, y, env) {
  eval(as.call(list(op, x, y)), envir = env)
}

is.halt_eval_condition <- function(x) inherits(x, "condition")

rtm_list <- function(list_name) {
  rtm_req("rtm.tasks.getList",
          list_id = rtm_lists()$id[match(list_name, rtm_lists()$name)]) %>%
    as.taskseries
}

  
format_due_date <- function(date) {
  this_year <- lubridate::year(lubridate::today())
  if_else(lubridate::year(date) == this_year,
          format(date, "%b %d"), format(date, "%m/%d/%y"))
}

print_due_date <- function(date, has_due_time) {
  if_else(has_due_time == 1, sprintf(" [%s]", format_due_date(date)), "")
}

current_lists <- function(smt = FALSE) {
  dplyr::filter(rtm_lists(), (archived == 0) & (smart == 0 | xor(!smt, smart == 1)))$name
}

store_lists <- function(rls) setNames(purrr::map(rls, rtm_list), rls)

filter_.taskseries <- function(tsrs, expr) keep_ids(tsrs, eval_across_plist(expr, tsrs))

sample_lists <- function(n = 10) store_lists(sample(current_lists(), n))
