print.taskseries <- function(x, completed = FALSE, ...) {
  cat("<taskseries>\n")
  print_taskseries_info(x, ~toString(name, p_tags(tags), p_pri(priority), due_time(due, has_due_time)))
}

toString.taskseries_info <- function(x, ..., width = 75, min_width = 6) {
  if (length(x) == 0) return(NULL)
  
  other_str <- paste(..., sep = "")
  width_x <- pmax(min_width, width - nchar(as.character(other_str), type = "width"))
  
  paste(
    dplyr::if_else(
      nchar(x, type = "width") > width_x,
      paste0(strtrim(x, width_x - 3), "..."),
      as.character(x)), ..., sep = "")
  
}

print_taskseries_info <- function(x, fmla, width = 80) {
  if (!exists("series_id", x$tasks)) {
    cat("taskseries with 0 entries\n")
  } else {
    select_(attr(x, ".ids"), ~series_id, ~name) %>%
      left_join(x$tasks, by = "series_id") %>%
      join_tags(x$tags) %>%
      mutate_(name = ~`class<-`(name, c("taskseries_info", "character"))) %>%
      lazyeval::f_eval(f = fmla, data = .) %>%
      print_info
  }
}

print_info <- function(x) if (is.null(x)) cat("...\n") else print(x, quote = FALSE)

due_time <- function(due, has_due_time) {
  dplyr::if_else(has_due_time == 1, sprintf(" [%s]", format_due_date(due)), "")
}

format_due_date <- function(due) {
  dplyr::case_when(
    is.na(due) ~ "",
    as.Date(due) == lubridate::today() ~ "Today",
    as.Date(due) == yesterday() ~ "Yesterday",
    as.Date(due) == tomorrow() ~ "Tomorrow",
    is_this_coming_week(due) ~ format(due, "%a"),
    lubridate::year(due) == this_year() ~ format(due, "%b %d"),
    TRUE ~ format(due, "%m/%d/%y"))
}

this_year <- function() lubridate::year(lubridate::today())
yesterday <- function() lubridate::today() - 1
tomorrow <- function() lubridate::today() + 1
is_this_coming_week <- function(due) dplyr::between(as.Date(due) - lubridate::today(), 1, 7)

p_pri <- function(priority) {
  dplyr::if_else(priority == "N", "", sprintf(" (%s)", priority))
}

p_tags <- function(tags) {
  if (is.list(tags))
    purrr::map_chr(tags, ~paste(sprintf(" #%s", .x), collapse = ""))
  else
    ""
}

join_tags <- function(x, tags_df) {
  left_join(
    x,
    dplyr::filter_(tags_df, ~series_id %in% x$series_id) %>%
      purrr::slice_rows("series_id") %>% {
        if (nrow(.) == 0) {
          named_quickdf(c("series_id", "tags"))
        } else {
          purrr::by_slice(., ~.x$tag, .to = "tags")
        }},
    by = "series_id")
}

fold_tsrs <- function(tsrs_df, nm) {
  if (nrow(tsrs_df) == 0)
    named_quickdf(c("series_id", nm))
  else {
    purrr::slice_rows(tsrs_df, "series_id") %>%
      purrr::by_slice(I, .to = nm)
  }
}

nest_tsrs <- function(tsrs) {
  tsrs[-1] %>%
    purrr:::lmap(~ list(fold_tsrs(.[[1]], names(.)))) %>%
    purrr::reduce(dplyr::left_join,
                  .init = tsrs[["series"]],
                  by = "series_id")
}

make_function <- function (args, body, env = parent.frame()) 
{
  args <- as.pairlist(args)
  stopifnot(is.call(body) || is.name(body) || is.atomic(body))
  env <- as.environment(env)
  eval(call("function", args, body), env)
}

## g <- function(.f, ...) {
##   make_function(alist(.x = , .y = , .= .x), .f[[2]], environment(.f))
## }

## h <- function(.f, ...) {
##   args <- lazyeval::f_lhs(.f)
##   make_function(
