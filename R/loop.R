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