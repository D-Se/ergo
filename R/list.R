#' @export
lap <- function (x, fun, ..., parallel = FALSE) {
  if(parallel) return(future.apply::future_lapply(x, fun, ...))
  fun <- match.fun(fun)
  if (!is.vector(x) || is.object(x)) 
    x <- as.list(x)
  .Internal(lapply(x, fun))
}

#' @export
vap <- function (x, fun, fun_value, ...,
                 use_names = TRUE, parallel = FALSE) {
  if(parallel) return(future.apply::future_vapply(x, fun, fun_value, ...))
  fun <- match.fun(fun)
  if (!is.vector(x) || is.object(x)) x <- as.list(x)
  .Internal(vapply(x, fun, fun_value, use_names))
}

#' @export
sap <- function (x, fun, ..., simplify = TRUE,
                 use_names = TRUE, parallel = FALSE) {
  if(parallel) return(future.apply::future_sapply(x, fun, ...,
                                                  simplify, use_names))
  fun <- match.fun(fun)
  ans <- lap(x = x, fun = fun, ...)
  if (use_names && is.character(x) && is.null(names(ans))) 
    names(ans) <- x
  if (!isFALSE(simplify)) 
    simplify2array(ans, higher = (simplify == "array"))
  else ans
}

#' @export
ul <- function(x) unlist(x, use.names = F)




