##' A closure to create functions corresponding to "rtm.tasks.*"
##' methods that call apply_rtm_method
##'
##' @param method the RTM task method
##' @param args the names of the arguments to the RTM task method
##' @return a function that calls "rtm_method" with the specified method
##' @export
make_rtm_task_fun <- function(method, args) {
  ## creating the arguments to the "closure" function that is returned
  nargs <- length(args)

  closure_fun_args <- c(alist(task =), rep(alist(arg =), nargs))
  names(closure_fun_args)[2:(nargs + 1)] <- unlist(args)

  ## create the body of the function,
  ## which is just a call to "apply_rtm_method"

  ## first create the arguments to "apply_rtm_method"
  body_call_args <- list(method, as.name("task"),
                         as.name(as.character(unlist(args))))

  ## now create the call to "apply_rtm_method"
  body_call <- as.call(as.pairlist(c(list(as.name("rtm_task_method")),
                                     body_call_args)))

  ## now create the "closure" function
  eval(call("function", as.pairlist(closure_fun_args), call("{", body_call)))
}

##' Assign 
##'
##' @param method_args a named list of the methods and their arguments
##' @param fun_factory the function factory to generate task_method functions
##' @param envir environment to evaluate in
##' @export
assign_task_method_funs <- function(method_args = task_method_args(),
                                    fun_factory = make_rtm_task_fun,
                                    envir = globalenv()) {
  am <- function (m, a) assign(m, fun_factory(m, a), envir = envir)
  Map(am, names(method_args), method_args)
  invisible()
}  
