#' lapply, but ergonomic
#'
#' @param x vector or an expression or coercible by \code{as.list}.
#' @param fun a function to be applied to each element.
#' @inheritParams base::lapply
#' @export
#' @keywords list
lap <- function(x, fun, ...) {
  lapply(X = x, FUN = fun, ...)
}

#'vapply, but ergonomic
#'
#' @param x vector or an expression or coercible by \code{as.list}.
#' @param fun a function to be applied to each element.
#' @param use_names should the output contain names?
#' @param fun_value a template for the return value from fun.
#' @inheritParams base::vapply
#'
#' @export
vap <- function(x, fun, fun_value, ..., use_names = TRUE) {
  vapply(x, fun, fun_value, ...)
}

#' sapply, but ergonomic
#'
#' @param x vector or an expression or coercible by \code{as.list}.
#' @param fun a function to be applied to each element.
#' @param ... dynamic dots
#' @inheritParams base::sapply
#' @param use_names should the output contain names?
#'
#' @export
sap <- function(x, fun, ..., simplify = TRUE, use_names = TRUE) {
  sapply(X = x, FUN = fun, ..., simplify = simplify, USE.NAMES = use_names)
}

#' unlist, but opiniated
#'
#' @param x a vector.
#'
#' @export
ul <- function(x) unlist(x, recursive = T, use.names = F)