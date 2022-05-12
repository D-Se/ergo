#' Make abbreviations into full nouns.
#' 
#' @param x string from formula in `?`
#' @param ... dots
#' @keywords internal
full <- function(x, ...) {
  s = endsWith(x, ")")
  out <- switch(
    if(s) stringi::stri_extract(x, regex = ".+?(?=\\()") else x,
    chr = "character", num = "numeric", int = "integer", 
    lst = "list", dfr = "data.frame", mtx = "matrix",
    lgl = "logical", fac = "factor", cmp = "complex",
    exp = "expression", arr = "array", env = "environment",
    fun = "function", fml = "formula",
    ..., # Dynamic load library
    stop("invalid atomic type"))
  if(!s) out else {
    stringi::stri_replace_all_regex(x,
      c(".+?(?=\\()", "((?<=\\().*(?=\\)))"),
      c(out, "..1, $1"), FALSE)
  }
}

# sub <- substitute

# dep <- deparse

#' Setting seed
#' 
#' @inheritParams base::set.seed
#' @param normal_kind character string or NULL. If it is a character string, set the method of Normal generation. Use "default" to return to the R default. NULL makes no change.
#' @param sample_kind character string or NULL. If it is a character string, set the method of discrete uniform generation (used in sample, for instance). Use "default" to return to the R default. NULL makes no change.
#' @export
seed <- function(seed, kind = NULL, normal_kind = NULL, sample_kind = NULL){
  set.seed(seed = seed, kind = kind,
           normal.kind = normal_kind, sample.kind = sample_kind)
}

#' Multiple argument library call
#' 
#' @param ... packages to be attached.
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

#' Clean the global environment.
#'
#' @export
clean <- function() rm(list = ls(envir = globalenv()))

#' Get the current working directory
#'
#' @export
here <- function() getwd()

#' Abbreviated duplicated.
#' 
#' @inheritParams base::duplicated
#' @export
dup <- function(x, incomparables = FALSE){
  duplicated(x = x, incomparables = incomparables)
}

#' Abbreviated length
#' 
#' @inheritParams base::length
#' @export
len <- function(x) {
  length(x)
}

#' Abbreviated names function
#' 
#' @inheritParams base::names
#' 
#' @export
nms <- function(x){
  names(x)
}
