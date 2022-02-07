#' @export
`%||%` <- function(x, y) if(is.null(x)) y else x

#' @export
`%nin%` <- Negate(`%in%`)

#' @export
lib <- function(...) {
  invisible(
    lapply(
      vapply(substitute(...()), deparse, ""),
      require,
      character.only = T
    )
  )
}
#' @export
chr <- character

#' @export
num <- numeric

#' @export
int <- integer

#' @export
lgl <- logical

#' @export
lst <- list

#' @export
fct <- factor

#' @export
dfr <- data.frame

#' @export
mtx <- matrix

#' @export
arr <- array

#' @export
vec <- vector

## methods

#' @export
as.chr <- as.character
#' @export
as.num <- as.numeric
#' @export
as.int <- as.integer
#' @export
as.lgl <- as.logical

#' @export
as.lst <- as.list
#' @export
as.fct <- as.factor

#' @export
as.dfr <- as.data.frame

#' @export
as.mtx <- as.matrix

#' @export
as.arr <- as.array

#' @export
as.vec <- as.vector