#' @export
`:=` <- function(x, ...) {
  deparse1(substitute(x)) |>
    p0("|> tv()") |>
    str2expression() |>
    eval(envir = globalenv())
}

#' @export
`%||%` <- function(x, y) if(is.null(x)) y else x

#' @export
`%nin%` <- Negate(`%in%`)


#' binary ? can now used for type checking in control flow
#' 
#' @description 
#' The package?packagename functionality is dropped in favor of concise
#' type checking. The ? occupies valuable real estate on the keyboard, and is
#' given high operator precedence in R. It can then be used to make chain actions
#' that make code much more concise yet humanly readable.
#' 
#' @export
#' @examples 
#' \dontrun{
#' x = as.character(1:5)
#' # is x a character vector?
#' x ? chr
#' # if x is character, make it an integer
#' # old: 'if(is.character(x)) as.integer(x) else x
#' if(x?chr) (x?~int) else x
#' }
# `?` <- function(x, y, ...){
#   if(!xor(missing(x), missing(y))) {
#     y = deparse(substitute(y))
#     # ternary operator (if(x) y else z
#     if(mlgl(y, ":")) {
#       cat(len(x))
#       return(eval(sap(strsplit(y, ":"), \(e) parse(text = e))[[2-+(x)]]))
#     }
#     (if(startsWith(y, "~")) {
#       p0("as.", full(y, 2L), "(x, ...)")
#     } else {
#       p0("is.", full(y, 1L), "(x, ...)")
#     }) |> str2expression() |> eval()
#   } else {
#     # prevent zero argument case
#     expr <- substitute(x)
#     search <- is.call(expr) && expr[[1L]] == "?"
#     if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
#       package <- expr[[2L]] ?~ chr
#       expr <- expr[[3L]]
#     } else {
#       package <- NULL
#     }
#     if (search) { # double ??, i.e ??tryCatch parsed as  `?`(`?`(tryCatch))
#       eval(substitute(help.search(TOPIC, package = PACKAGE),
#                              list(TOPIC = as.character(expr)[2], PACKAGE = package)))
#     } else { # unary ? path, i.e ?sum
#         if (is.call(expr)) return(.helpForCall(expr, parent.frame()))
#         topic <- if (is.name(expr)) (expr ?~ chr) else x
#         eval(substitute(help(TOPIC, package = PACKAGE),
#                                list(TOPIC = topic, PACKAGE = package)))
#     }
#   }
# }
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
      if (is.call(expr)) return(utils:::.helpForCall(expr, parent.frame()))
      topic <- if (is.name(expr)) (expr ?~ chr) else x
      eval(substitute(help(TOPIC, package = PACKAGE),
                      list(TOPIC = topic, PACKAGE = package)))
    }
    #x = deparse(substitute(x))
    #`?_help`(x)
  }
}

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

`?_help` <- function(x) {
  expr <- deparse(substitute(x))
  search <- is.call(expr) && expr[[1L]] == "?"
  if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
    package <- as.character(expr[[2L]])
    expr <- expr[[3L]]
  } else {
    package <- NULL
  }
  if (search) { # double ??, i.e ??tryCatch parsed as  `?`(`?`(tryCatch))
    eval(substitute(help.search(TOPIC, package = PACKAGE),
                    list(TOPIC = as.character(expr)[2], PACKAGE = package)))
  } else { # unary ? path, i.e ?sum
    if (is.call(expr)) return(.helpForCall(expr, parent.frame()))
    topic <- if (is.name(expr)) (expr ?~ chr) else x
    eval(substitute(help(TOPIC, package = PACKAGE),
                    list(TOPIC = topic, PACKAGE = package)))
  }
}

# xnor <- Negate(xor)
# 
# xor(TRUE, FALSE)
# xor(FALSE, TRUE)
# !xor(T, T)

# ?sum

# `%!%` <- function(x, y) {
#   y = deparse(substitute(y))
#   p0("as.", complete(y), "(x)") |>
#     str2expression() |>
#     eval()
# }