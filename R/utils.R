# 
# type_convert <- function(...){
#   #browser()
#   switch(class(..2[[2]]),
#          name = {
#            y <- as.character(..2[[2]])
#            do.call(paste0("as.", full(y), collapse = ""), list(..1))
#          },
#          call = {
#            l = as.list(..2[[2]])
#            y <- as.character(l[[2]])
#            if(is.character(..1) & is.character(l[[2]])) {
#              return(do.call("paste", c(..1, l[-1])))
#            }
#            do.call(paste0("as.", full(y), collapse = ""), c(..1, l[-c(1,2)]))
#          },
#          character = {
#            paste0(..1, ..2[[2]])
#          }
#   )
# }



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


# shortcut for tidyverse map
# #' @keywords w internal
# .map <- function (.x, .f, ...) {
#   .f <- purrr::as_mapper(.f, ...)
#   .Call(purrr:::map_impl, environment(), ".x", ".f", "list")
# }

# fun <- function(expr) {
#   function(x = quote(..1), y = quote(..2), ...) eval(expr)
# }
# 


