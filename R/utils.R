#' @keywords internal
full <- function(x, ind) {
  switch(substring(x, ind),
         "dfr" = "data.frame",
         "int" = "integer", "chr" = "character", "num" = "numeric",
         "lgl" = "logical", "lst" = "list", "mtx" = "matrix",
         stop("invalid atomic type"))
}








# sub <- substitute

# dep <- deparse


#' @export
seed <- set.seed

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
clean <- function() rm(list = ls(envir = globalenv()))


#' @export
here <- function() getwd()


# rep = replicate
#' @export
dup <- duplicated

#' @export
len <- length

#' @export
nms <- names