##' Takes an element x of length 1 and a list that can be quickdf'd,
##' and returns a cbinded df.
##'
##' @param x an vector of length 1
##' @param l a list that can be quickdf'd
##' @param x_name the name of the x column
##' @return a data frame
cbind_quickdf <- function(x, l, var, x_name) {
  c(purrr::set_names(list(rep(x, length(l[[1]]))),
              x_name),
    if (is.character(l)) purrr::set_names(list(l), var) else l) %>%
    quickdf()
}


flatten_taskseries <- function(tsk_df, svar = "series_id") {

  ## cbind_id <- function (var) cbind(series_id = tsk_df$id, tsk_df[[var]]) %>% as.taskseries_df
  
  ## # flatten a nested column list/data.frame
  ## flatten_var <- function(xvar,
  ##                         f = ~ cbind_quickdf(.x, .y[[xvar]], xvar, svar),
  ##                         var = paste0(xvar, "s")) {
  ##   if (is.data.frame(tsk_df[[var]]))
  ##     cbind_id(var)
  ##   else
  ##     purrr::map2_df(tsk_df$id, tsk_df[[var]], f) %>% as.taskseries_df
  ## }

  fix_unnest <- function(df, col, fun) {
    if (is.data.frame(df[[col]])) {
      cbind(series_id = df$series_id, df[[col]])
    } else {
      mutate_(df, .dots = (~f(c)) %>%
                    lazyeval::interp(c = as.name(col), f = as.name(fun)) %>%
                    list %>% setNames(col)) %>%
        tidyr::unnest_(col)
    }
#      mut_unnest(df, col, fun)
  }

  fix_quickdf2 <- function(ncol) purrr::map(ncol, ~ purrr::flatten(.x) %>% quickdf)
  fix_quickdf <- function(ncol) purrr::map(ncol, quickdf)
  
  select_cols <- function(cols) {
    rename_(tsk_df, .dots = setNames("id", svar)) %>%
      select_(.dots = c(svar, cols))
  }

  ## mut_unnest <- function(df, col, fun) {
  ##   mutate_(df, .dots = (~f(c)) %>%
  ##                 lazyeval::interp(c = as.name(col),
  ##                                  f = as.name(fun)) %>%
  ##                 list %>% setNames(col)) %>%
  ##     unnest_(col)
  ## }
  
  fix_ts <- function(s_cols, fun = NULL, t_cols = NULL) {
    
    `if`(length(setdiff(s_cols, names(tsk_df))) == 0, {
      select_cols(s_cols) %>%
        { if (is.null(fun)) . else fix_unnest(., s_cols, fun) } %>%
        { if (is.null(t_cols) || (length(intersect(t_cols, names(.))) == 0)) .
          else dplyr::mutate_at(., t_cols, rtm_date) }
    }, taskseries_df()) %>%
      as.taskseries_df()
  }
  
  taskseries(
    series = fix_ts(c("created", "modified", "name", "source", "url", "location_id")),
    tasks = fix_ts("task", "fix_quickdf", c("due", "added", "completed", "deleted")),
    notes = fix_ts("notes", "fix_quickdf2", c("created", "modified")),
    tags = fix_ts("tags", "fix_quickdf"),
    participants = fix_ts("participants", "fix_quickdf"),
    rrule = fix_ts("rrule", "fix_quickdf")
  )
  
}

## taskseries(
  ##   series = tsk_df[, c("id", "created", "modified", "name", "source", "url", "location_id")] %>%
  ##     rename(series_id = "id") %>% as.taskseries_df,
  ##   tasks = flatten_var("task", f = ~ cbind_quickdf(.x, .y, "task", svar), "task"),
  ##   notes = flatten_var("note"),
  ##   tags = flatten_var("tag"),
  ##   participants = flatten_var("participant"),
  ##   rrule = if ("rrule" %in% names(tsk_df)) cbind_id("rrule") else NULL
  ## )
#}

convert_taskseries_time <- function(tsksrs) {
  convtime <- mutate_each_col_f(rtm_date)
  tsksrs[["series"]] <- convtime(tsksrs[["series"]], c("created", "modified"))
  tsksrs[["tasks"]] <- convtime(tsksrs[["tasks"]], c("due", "added", "completed", "deleted"))
  tsksrs[["notes"]] <- convtime(tsksrs[["notes"]], c("created", "modified"))
  tsksrs
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
  # filter_tsr_(x, ~!is.na(tasks$completed))
  cat("<taskseries>\n")
  cat(x$series$name, sep = " - ")
  cat("\n")
}

as.taskseries <- function(x) UseMethod("as.taskseries")
as.taskseries.list <- function(x) structure(x, class = "taskseries")
as.taskseries.taskseries <- function(x) x
as.taskseries.NULL <- function(x) taskseries()

as.taskseries.rtm_response <- function(x) {
  if (x$stat == "ok") {
    x$tasks$list$taskseries %>%
      flatten_taskseries %>%
      convert_taskseries_time
  } else {
    stop("status not ok")
  }
}

is.taskseries <- function(x) inherits(x, "taskseries")

c.taskseries <- function(...) {
  purrr::reduce(list(...), taskseries_combine)
}

keep_ids <- function(tsrs, ids) {
  purrr::map(tsrs, ~ .x[.x[["series_id"]] %in% ids, ]) %>%
    as.taskseries
}

discard_ids <- function(tsksrs, ids) {
  purrr::map(tsrs, ~ .x[!.x[["series_id"]] %in% ids, ]) %>%
    as.taskseries
}

filter_tsr <- function(tsrs, expr) {

  if (!is.taskseries(tsrs)) stop("not a taskseries")

  keep_ids(tsrs, eval_across_plist(expr, tsrs))
}

is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`(`))
}

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

halt_eval_condition <- function(info = NULL) {
  structure(
    class = c("halt_eval_condition", "condition"),
    list(message = "halt_eval_condition", call = sys.call(-1)),
    info = info
  )
}
is.halt_eval_condition <- function(x) inherits(x, "condition")

