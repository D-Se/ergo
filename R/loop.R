#' for-loop prototyping
#' 
#' @param expr an expression of the body of a loop.
#' @param ... dynamic dots that include objects necessary for the body of the loop.
#' @param type the type of loop to construct. along for a sequence alona vector, row for a sequence along the rows of an object.
#' 
#' @export
#' @examples 
#' x = as.list(1:5)
#' loop({ y[[i]] <- x[[i]] + 1}, x)
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
