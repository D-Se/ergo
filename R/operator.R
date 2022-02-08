#' Ergo protoype kickstarter - wrapper for \code{ergo::tv()}.
#' 
#' @param x an expression.
#' 
#' @export
`:=` <- function(x) {
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
#' that make code much more concise yet humanly readable.
#' 
#' @param x scalar, vector, name or call.
#' @param y scalar or vector.
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
#' }
`?` <- function(x, y){
  if(!xor(missing(x), missing(y))) {
    y = deparse(substitute(y))
    `?_controlflow`(x, y)
  } else { # trimmed help function
    expr = substitute(x)
    # substitute(x)[[1]]
    if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
      package <- as.character(expr[[2L]])
      expr <- expr[[3L]]
    } else {
      package <- NULL
    }
    if (is.call(expr) && expr[[1L]] == "?") { # double ??, i.e ??tryCatch parsed as  `?`(`?`(tryCatch))
      eval(substitute(help.search(TOPIC, package = PACKAGE),
                      list(TOPIC = as.character(expr)[2], PACKAGE = package)))
    } else { # unary ? path, i.e ?sum
      #if (is.call(expr)) return(utils:::.helpForCall(expr, parent.frame()))
      if (is.call(expr)) return(helper(expr, parent.frame()))
      topic <- if (is.name(expr)) (expr ?~ chr) else x
      eval(substitute(help(TOPIC, package = PACKAGE),
                      list(TOPIC = topic, PACKAGE = package)))
    }
    #x = deparse(substitute(x))
    #`?_help`(x)
  }
}

#' Control flow component for ? operator
#' 
#' @param x a logical vector or scalar.
#' @param y a vector, scalar or compound expression using :.
`?_controlflow` <- function(x, y) {
  #if(mlgl(y, ":", fixed = T)) { # ternary operator, i.e. if(x) y else z OR ifelse(x, y, z)
  if(stringi::stri_detect_fixed(y, ":", max_count = 1L)) {
    l = strsplit(y, ":", fixed = TRUE)[[1]]
    if(len(x) > 1) { # vectorized
      y1 <- eval(str2expression(l[1]))
      y2 <- eval(str2expression(l[2]))
      stopifnot("x not a multiple of y" = len(x) %% len(y1) == 0)
      return(eval.parent(ifelse(x, y1, y2)))
    } else { # normal if-else statement
      return(eval.parent(if(x) str2expression(l[1]) else str2expression(l[2])))
      }
  }
  string <- if(startsWith(y, "~")) {
    p0("as.", full(y, 2L), "(x)")
  } else {
    p0("is.", full(y, 1L), "(x)")
  }
  eval.parent(str2expression(string))
}

# `?_help` <- function(x) {
#   expr <- deparse(substitute(x))
#   search <- is.call(expr) && expr[[1L]] == "?"
#   if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
#     package <- as.character(expr[[2L]])
#     expr <- expr[[3L]]
#   } else {
#     package <- NULL
#   }
#   if (search) { # double ??, i.e ??tryCatch parsed as  `?`(`?`(tryCatch))
#     eval(substitute(help.search(TOPIC, package = PACKAGE),
#                     list(TOPIC = as.character(expr)[2], PACKAGE = package)))
#   } else { # unary ? path, i.e ?sum
#     if (is.call(expr)) return(.helpForCall(expr, parent.frame()))
#     topic <- if (is.name(expr)) (expr ?~ chr) else x
#     eval(substitute(help(TOPIC, package = PACKAGE),
#                     list(TOPIC = topic, PACKAGE = package)))
#   }
# }

#' 
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
