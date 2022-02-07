#' @export
lap <- function (x, fun, ...) {
  fun <- match.fun(fun)
  if (!is.vector(x) || is.object(x)) 
    x <- as.list(x)
  .Internal(lapply(x, fun))
}

#' @export
vap <- function (x, fun, fun_value, ..., use_names = TRUE) {
  fun <- match.fun(fun)
  if (!is.vector(x) || is.object(x)) x <- as.list(x)
  .Internal(vapply(x, fun, fun_value, use_names))
}

#' @export
sap <- function (x, fun, ..., simplify = TRUE, use_names = TRUE) {
  fun <- match.fun(fun)
  ans <- lap(x = x, fun = fun, ...)
  if (use_names && is.character(x) && is.null(names(ans))) 
    names(ans) <- x
  if (!isFALSE(simplify)) 
    simplify2array(ans, higher = (simplify == "array"))
  else ans
}

#' for-loop prototyping
#' 
#' @export
#' @examples 
#' \dontrun{
#' x = as.list(1:5)
#' loop({
#'  y[[i]] <- x[[i]] + 1
#' }, x)
#' 
#' x = mtx(1:9, ncol = 3)
#' loop({
#'  y[i] <- sum(x[i,])
#' }, x, type = "row")
#' }
loop <- function(expr, ..., type = "along"){
  expr <- deparse1(substitute(expr))
  y = switch(class(..1)[1],
         "matrix" = vector("integer", nrow(..1)),
         vector(class(..1), length(..1))
         )
  `for`(i,
        switch(type, 
               "along" = seq_along(..1),
               "row" = 1:nrow(..1),
               stop("undefined type")
               ),
        eval(str2expression(expr)))
  y
}


#' @export
seed <- set.seed


