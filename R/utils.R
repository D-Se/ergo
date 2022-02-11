#' Make abbreviations into full nouns.
#' 
#' @keywords internal
# full <- function(x, ind) {
#   switch(substring(x, ind),
#          "dfr" = "data.frame",
#          "int" = "integer", "chr" = "character", "num" = "numeric",
#          "lgl" = "logical", "fct" = "factor", "sym" = "symbol",
#          "lst" = "list", "mtx" = "matrix", "arr" = "array", "dbl" = "double",
#          "fun" = "function", "vec" = "vector",
#          stop("invalid atomic type"))
# }
full <- function(x) {
  #if(ind != 0L) x <- substring(x, ind)
  #s = stringi::stri_detect_regex(x, "\\(", max_count = 1L)
  s = endsWith(x, ")")
  #string = if(s) stringi::stri_extract(x, regex = ".+?(?=\\()") else x
  out <- switch(
    if(s) stringi::stri_extract(x, regex = ".+?(?=\\()") else x,
    "int" = "integer", "chr" = "character", "num" = "numeric",
    "dfr" = "data.frame", "lst" = "list", "mtx" = "matrix",
    "lgl" = "logical", "fct" = "factor", "sym" = "symbol",
    "arr" = "array", "dbl" = "double", "fun" = "function", "vec" = "vector",
    stop("invalid atomic type"))
  if(s){
    #out <- sub(".+?(?=\\()", out, x, perl = TRUE)
    #sub("((?<=\\().*(?=\\)))", "..1, \\1", out, perl = TRUE)
    stringi::stri_replace_all_regex(x,
      pattern = c(".+?(?=\\()", "((?<=\\().*(?=\\)))"),
      replacement = c(out, "..1, $1"), vectorize_all = FALSE)
  } #else if (dots) {
    #paste0(out, "(..1)", collapse = "")
  #} else {
  else{
    out
  }
}
# x <- "mtx(ncol = 3)"
# 
# library(stringi)
# out = "matrix"
# 
# stri_replace_all_regex(x,
#                        pattern = c(".+?(?=\\()", "((?<=\\().*(?=\\)))"),
#                        replacement = c(out, "..1, $1"), vectorize_all = FALSE)

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
