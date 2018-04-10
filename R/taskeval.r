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

is_parenthesized <- function(expr) is.call(expr) && identical(expr[[1L]], quote(`(`))

eval_infix <- function(op, x, y, env) eval(as.call(list(op, x, y)), envir = env)

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
