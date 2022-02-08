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


#' binary ? can now used for typechecking in control flow
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
  if(sum(missing(x), missing(y)) == 0){
    y = deparse(substitute(y))
    string <- if(startsWith(y, "~")){
      p0("as.", complete(substring(y, 2)), "(x)")
    } else{
      p0("is.", complete(substring(y, 1)), "(x)")
    }
    eval(str2expression(string))
  } else {
    #type <- NULL
    expr <- substitute(x)
    search <- (is.call(expr) && expr[[1L]] == "?")
    if (is.call(expr) && (expr[[1L]] == "::" || expr[[1L]] == ":::")) {
      package <- as.character(expr[[2L]])
      expr <- expr[[3L]]
    } else {
      package <- NULL
    }
    if (search) {
      return(eval(substitute(help.search(TOPIC, package = PACKAGE),
                             list(TOPIC = as.character(expr), PACKAGE = package))))
    } else {
        if (is.call(expr))
          return(.helpForCall(expr, parent.frame()))
        topic <- if (is.name(expr))
          as.character(expr)
        else x
        return(eval(substitute(help(TOPIC, package = PACKAGE),
                               list(TOPIC = topic, PACKAGE = package))))
    }
  }
}

# 
# `%!%` <- function(x, y) {
#   y = deparse(substitute(y))
#   p0("as.", complete(y), "(x)") |>
#     str2expression() |>
#     eval()
# }