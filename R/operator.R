#' @export
`:=` <- function(x, ...) {
  deparse1(substitute(x)) |>
    p0("|> tv()") |>
    str2expression() |>
    eval(envir = globalenv())
}
