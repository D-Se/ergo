#' @export
seed <- set.seed

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