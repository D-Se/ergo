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
`?` <- function(x, y){
  if(sum(missing(x), missing(y)) == 0) {
    y = deparse(substitute(y))
    (if(startsWith(y, "~")) {
      p0("as.", full(y, 2L), "(x)")
    } else {
      p0("is.", full(y, 1L), "(x)")
    }) |> str2expression() |> eval()
  } else {
    expr <- substitute(x)
    search <- is.call(expr) && expr[[1L]] == "?"
    if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
      package <- expr[[2L]] ?~ chr
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
}


# ?sum

# `%!%` <- function(x, y) {
#   y = deparse(substitute(y))
#   p0("as.", complete(y), "(x)") |>
#     str2expression() |>
#     eval()
# }