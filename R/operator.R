#' @export
. <- NULL

#' Ergo protoype kickstarter - wrapper for \code{ergo::tv()}.
#' 
#' @param x an expression.
#' @param y void
#' 
#' @export
`:=` <- function(x, y = NULL) {
  deparse1(substitute(x)) |>
    p0("|> tv()") |>
    str2expression() |>
    eval(envir = globalenv())
}

#' binary ? can now used for type checking in control flow
#' 
#' @description 
#' The package?packagename functionality is dropped in favor of concise
#' type checking. The ? occupies valuable real estate on the keyboard, and is
#' given high operator precedence in R. It can then be used to make chain actions
#' that make code concise yet humanly readable.
#' 
#' @details 
#' Base R has as of version 4.2 introuced a hard error for supplying a vector to
#'a regular if-statement. In \code{ergo} this is instead evaluated as an \code{ifelse}
#' @param ... conditions
#' 
#' @export
#' @return If conditional input, a vector or scalar. If help is sought, silent.
#' @examples 
#' \dontrun{
#' x = as.character(1:5)
#' # is x a character vector?
#' x ? chr
#' # if x is character, make it an integer
#' # old: 'if(is.character(x)) as.integer(x) else x
#' if(x?chr) (x?~int) else x
#' 
#' # base R throws an error
#' 1:4 |> if(sum() > 10) 10 else 5
#' 
#' 1:4 |> sum() > 10 ? 10 ~ 5
#' 
#' # passing arguments by \code{expr[...]}
#' e <- list(first = "a", second = F) |> list2env()
#' as.list(e) ; as.list(e, sorted = TRUE)
#' e ?~ lst[sorted = TRUE]
#' }
`?` <- function(...){
  switch(...length(),
         `?_help`(...), # regular help for unary ?
         switch(length(..2),
                type_check(...), # no ..2 or no formula 5 ? 3
                type_convert(...), # ..2 starts with formula 5 ? ~ 3
                control_flow(...), # formula in body 5 ? 3 ~ 5, nested 5 ~ 3
                e("?", "Unsupported format")
         ),
         e("?", "Unsupported format")
  )
}

control_flow <- function(query, ...){
  #browser()
  if(length(query) > 1L){
    do.call("ifelse", list(query, ..1[[2]], ..1[[3]]), envir = parent.frame(2))
  } else {
    eval.parent(if(query) ..1[[2]] else ..1[[3]])
  }
}

type_check <- function(...){
  #browser()
  y <- as.character(substitute(...())[[2]])
  do.call(paste0("is.", full(y), collapse = ""), list(..1)) 
}

type_convert <- function(...){
  #browser()
  #return(.map(..1, ..2))
  #return(fun(..2[[2]])(..1))
  # y <- as.character(..2[[2]])
  if(is.name(..2[[2]])) {
    y <- as.character(..2[[2]])
    y <- full(y)
    y %||% return(1000)
    do.call(paste0("as.", y, collapse = ""), list(..1))
  } else {
    l <- as.list(..2[[2]])
    y <- as.character(l[[2]])
    y <- full(y)
    y %||% return(1000)
    do.call(paste0("as.", y, collapse = ""), c(..1, l[-c(1,2)]))
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
