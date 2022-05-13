# #' Null-coalescing operator
# #' 
# #' Convenience operator to transform \code{NULL} values into something useful.
# #' 
# #' @param x,y an object \code{x} and replacement \code{y}.
# #' @return \code{y} if \code{x} is \code{NULL} else \code{x}.
# `%||%` <- function(x, y) {
#  if(is.null(x)) y else x
#}

#' Query operator \code{?}
#' 
#' @description 
#' \code{?} is a concise interface to query documentation, control flow, type conversion and type check
#' operations.
#' 
#' @usage \special{? topic}
#' @usage \special{test ? yes ~ no}
#' @usage \special{x ? type}
#' @usage \special{x ?~ type}
#' @usage \special{x ?~ type [\dots]}
#' 
#' @param topic a topic for which help is sought, usually a name or string.
#' 
#' @param test a \emph{query}, an object which can be coerced to logical mode.
#' @param yes value(s) if query is \code{TRUE}.
#' @param no value(s) if query is \code{FALSE}.
#' 
#' @param x a \emph{query}, can be a name, logical, 
#' @param type an abbreviated class to check for transform \code{\var{x}} into.
#' @param \dots passing optional arguments to further functions.
#' @return \code{NULL} if documentation, a Boolean if type check.
#' 
#' @details 
#' \code{ergo::`?`} re-purposes the second argument of the binary operator \code{?}.
#' \describe{
#' \item{\bold{Find documentation:} &emsp;\code{?\var{x}}}{
#'      A shortcut to a \bold{unary} call of \code{\link[utils:help]{utils::help()}}.
#'      The binary second argument \code{type?topic} is dropped. Use \code{
#'      utils::`?`(\var{type}, \var{topic)}} to find S4 method documentation.
#'      Other than type?pkg (help binary functionality) regular semantics apply.
#' }
#' \item{\bold{Execute control flow:} &emsp; \code{\var{test} ? \var{yes} ~ 
#' \var{no}} &emsp; and &emsp; \code{\var{x} ? \var{type}} - }{
#'       This uses \code{if(test) x else y} if the query is length 1, and 
#'       \code{ifelse(test, x, y)} otherwise. If \code{yes} or \code{no} are too
#'       short, their elements are recycled.
#' }
#' \item{\bold{Type checks and conversion:} &emsp;\code{\var{x} ?
#'  \var{type}} &emsp; and &emsp; \code{\var{x} ?~ \var{type}}}{
#'       Using the formula interface functions may be applied to the LHS of
#'       \code{?}. Function arguments can be passed on using \code{[\dots]}
#' }
#' }
#' 
#' @section Operator precedence:
#' \code{?} has the lowest operator precedence in R, different from that of
#' infix operators. \code{1>0 ? 'a'~'b'} gives \code{"a"}, but \code{x = 1>0 ?
#' 'a' ~ 'b'} assigns \code{TRUE} to \code{x}. Thus it is recommended to use
#' parentheses for operations.
#' 
#' For the full list of operator precedence see \link[base:Syntax]{base::Syntax()}.
#' 
#' @examples 
#' x = as.character(1:5)
#' # is x a character vector?
#' x ? chr
#' # if x is character, make it an integer
#' # old: 'if(is.character(x)) as.integer(x) else x
#' if(x?chr) (x?~int) else x
#' 
#' \dontrun{
#' # base R throws an error for if-statement on RHS in pipe
#' 1:4 |> if(sum() > 10) 10 else 5
#' }
#' 1:4 |> sum() > 10 ? 10 ~ 5
#' 
#' # passing arguments by \code{expr[...]}
#' e <- list(first = "a", second = FALSE) |> list2env()
#' as.list(e) ; as.list(e, sorted = TRUE)
#' e ?~ lst[sorted = TRUE]
#' 
#' # the ? operator can be used in the RHS of a pipe
#' (1:5 > 3) |> sum() > 3 ? 100 ~ 200
#' @seealso
#' \code{link[utils:help]{utils::help}} to help find documentation.
#' \code{link[base:ifelse]{base::ifelse}} and \code{link[base:Control]{base::Control}}
#' for details on base R control flow.
#' @export
`?` <- function(...){
  switch(...length(),
         `?_help`(...), # ?sum
         switch(length(..2),
                type_check(...), # 5 ? chr
                type_convert(...), # 5 ? ~ chr
                control_flow(...), # TRUE ? 3 ~ 5
                e("?", "Unsupported format")
         ),
         e("?", "Unsupported format")
  )
}
### TODO: check sys.call() when assigned, use <<- ?

control_flow <- function(query, do) {
  if(length(query) > 1L) {
    do.call("ifelse", list(query, do[[2L]], do[[3L]]), envir = parent.frame(2))
  } else {
    eval.parent(if(query) do[[2L]] else do[[3L]])
  }
}

type_check <- function(...) {
  # TODO insert length check for ifelse NA coercion
  y <- as.character(substitute(...())[[2L]])
  y <- get_function(y, "is.")
  do.call(y, list(..1))
}

type_convert <- function(...) {
  l <- ..2[[2L]]
  if(is.name(l)) {
    y <- as.character(l)
    y <- get_function(y, "as.")
    do.call(y, list(..1))
  } else { # is.call
    y <- as.character(l[[2L]])
    y <- get_function(y, "as.")
    l[[1L]] <- NULL
    l[[1L]] <- NULL
    do.call(y, c(list(..1), l))
  }
}

`?_help` <- function(...) {
  expr = substitute(...)
  if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
    package <- as.character(expr[[2L]])
    expr <- expr[[3L]]
  } else {
    package <- NULL
  }
  # double ??, i.e ??tryCatch parsed as  `?`(`?`(tryCatch))
  if (is.call(expr) && expr[[1L]] == "?") {
    eval(substitute(utils::help.search(TOPIC, package = PACKAGE),
                    list(TOPIC = as.character(expr)[2], PACKAGE = package)), 
         parent.frame())
  } else { # unary ? path, i.e ?sum
    #if (is.call(expr)) return(utils:::.helpForCall(expr, parent.frame()))
    if (is.call(expr)) return(helper(expr, parent.frame()))
    topic <- if (is.name(expr)) as.character(expr) else c(...)
    eval(substitute(utils::help(TOPIC, package = PACKAGE),
                    list(TOPIC = topic, PACKAGE = package)), parent.frame())
  }
}

#' @importFrom methods isGeneric
#' @importFrom methods getGeneric
#' @importFrom methods selectMethod
helper <- function (expr, envir, doEval = TRUE) {
  sigFormat <- function(sigNames, sigClasses) {
    paste(sprintf("%s = \"%s\"", sigNames, sigClasses), 
          collapse = ", ")
  }
  f <- expr[[1L]]
  if (is.call(f) && (f[[1L]] == "::" || f[[1L]] == ":::")) {
    package <- f[[2L]]
    where <- paste0("package:", package)
    if (!(where %in% search())) 
      where <- NULL
    f <- f[[3L]]
  }
  else {
    package <- NULL
    where <- topenv(envir)
  }
  if (is.name(f)) 
    f <- as.character(f)
  if (is.null(where) || !.isMethodsDispatchOn() || !methods::isGeneric(f, 
                                                                       where = where)) {
    if (!is.character(f) || length(f) != 1L) 
      stop(gettextf("the object of class %s in the function call %s could not be used as a documentation topic", 
                    dQuote(class(f)), sQuote(deparse(expr))), domain = NA)
    h <- try_help(f, package = package)
    if (is.null(h)) 
      stop(gettextf("no methods for %s and no documentation for it as a function", 
                    sQuote(f)), domain = NA)
  }
  else {
    if (methods::is(f, "genericFunction")) {
      fdef <- f
      f <- fdef@generic
    }
    else fdef <- methods::getGeneric(f, where = where)
    sigClasses <- sig_from_call(fdef, expr, envir, doEval)
    sigNames <- names(sigClasses)
    method <- methods::selectMethod(f, sigClasses, optional = TRUE, 
                                    fdef = fdef)
    if (methods::is(method, "MethodDefinition")) {
      sigClasses <- method@defined
      if (length(sigClasses) < length(sigNames)) 
        sigClasses <- c(sigClasses, rep.int("ANY", 
                                            length(sigNames) - length(sigClasses)))
    }
    else warning(gettextf("no method defined for function %s and signature %s", 
                          sQuote(f), sQuote(sigFormat(sigNames, sigClasses))), 
                 domain = NA)
    topic <- paste(paste(c(f, sigClasses), collapse = ","), "method", sep = "-")
    h <- try_help(topic, package = package)
    if (is.null(h)) 
      stop(gettextf("no documentation for function %s and signature %s", 
                    sQuote(f), sQuote(sigFormat(sigNames, sigClasses))), 
           domain = NA)
  }
  h
}

try_help <- function (topic, package = NULL) {
  h <- tryCatch(do.call("help", list(topic, package = package)), 
                error = identity)
  if (inherits(h, "error") || !length(h)) NULL else h
}

#' @importFrom methods is
#' @importFrom methods elNamed
sig_from_call <- function (fdef, expr, envir, doEval = TRUE) {
  args <- formals(fdef)
  call <- match.call(fdef, expr, expand.dots = FALSE)
  args[names(call[-1L])] <- call[-1L]
  if ("..." %in% names(call)) 
    args$... <- args$...[[1L]]
  sigNames <- fdef@signature
  sigClasses <- rep.int("missing", length(sigNames))
  names(sigClasses) <- sigNames
  for (arg in sigNames) {
    argExpr <- methods::elNamed(args, arg)
    if (!missing(argExpr) && !is.null(argExpr)) {
      simple <- (is.character(argExpr) || is.name(argExpr))
      if (doEval || !simple) {
        argVal <- try(eval(argExpr, envir))
        if (methods::is(argVal, "try-error")) 
          stop(gettextf("error in trying to evaluate the expression for argument %s (%s)", 
                        sQuote(arg), deparse(argExpr)), domain = NA)
        sigClasses[[arg]] <- class(argVal)[1L]
      }
      else sigClasses[[arg]] <- as.character(argExpr)
    }
  }
  sigClasses
}

#' Make abbreviations into full nouns.
#' 
#' @param x string from formula in `?`
#' @param ... dots
#' @keywords internal
get_function <- function(fun, prepend) {
  paste0(prepend,
         switch(fun,
                chr = "character", num = "numeric", int = "integer", 
                lst = "list", dfr = "data.frame", mtx = "matrix",
                lgl = "logical", fac = "factor", cmp = "complex",
                exp = "expression", arr = "array", env = "environment",
                fun = "function", fml = "formula",
                # ..., # Dynamic load library
                return(fun)) # call function on query
  )
}
# full2 <- function(x, ...) {
#   #s = endsWith(x, ")")
#   out <- switch(x,
#    # if(s) stringi::stri_extract(x, regex = ".+?(?=\\()") else x,
#     chr = "as.character", num = "as.numeric", int = "as.integer", 
#     lst = "as.list", dfr = "as.data.frame", mtx = "as.matrix",
#     lgl = "as.logical", fac = "as.factor", cmp = "as.complex",
#     exp = "as.expression", arr = "as.array", env = "as.environment",
#     fun = "as.function", fml = "as.formula",
#     ..., # Dynamic load library
#     x)
#   #if(!s) out else {
#    # stringi::stri_replace_all_regex(x,
#     #                                c(".+?(?=\\()", "((?<=\\().*(?=\\)))"),
#      #                               c(out, "..1, $1"), FALSE)
#   #}
# }

