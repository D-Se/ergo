#' @export
do_red <- function (x, f, init, right = FALSE, accumulate = FALSE) {
  mis <- missing(init)
  len <- length(x)
  if (len == 0L) 
    return(if (mis) NULL else init)
  f <- match.fun(f)
  if (!is.vector(x) || is.object(x)) 
    x <- as.list(x)
  ind <- seq_len(len)
  if (mis) {
    if (right) {
      init <- x[[len]]
      ind <- ind[-len]
    }
    else {
      init <- x[[1L]]
      ind <- ind[-1L]
    }
  }
  if (!accumulate) {
    if (right) {
      for (i in rev(ind)) init <- forceAndCall(2, f, x[[i]], 
                                               init)
    }
    else {
      for (i in ind) init <- forceAndCall(2, f, init, x[[i]])
    }
    init
  }
  else {
    len <- length(ind) + 1L
    out <- vector("list", len)
    if (mis) {
      if (right) {
        out[[len]] <- init
        for (i in rev(ind)) {
          init <- forceAndCall(2, f, x[[i]], init)
          out[[i]] <- init
        }
      }
      else {
        out[[1L]] <- init
        for (i in ind) {
          init <- forceAndCall(2, f, init, x[[i]])
          out[[i]] <- init
        }
      }
    }
    else {
      if (right) {
        out[[len]] <- init
        for (i in rev(ind)) {
          init <- forceAndCall(2, f, x[[i]], init)
          out[[i]] <- init
        }
      }
      else {
        for (i in ind) {
          out[[i]] <- init
          init <- forceAndCall(2, f, init, x[[i]])
        }
        out[[len]] <- init
      }
    }
    if (all(lengths(out) == 1L)) 
      out <- unlist(out, recursive = FALSE)
    out
  }
}

#' @export
do_fil <- function (x, f) {
  ind <- as.logical(unlist(lapply(x, f)))
  x[which(ind)]
}

#' @export
do_find <- function (x, f, right = FALSE, no_match = NULL) {
  f <- match.fun(f)
  if ((pos <- Position(f, x, right, no_match = 0L)) > 0L) 
    x[[pos]]
  else no_match
}

#' @export
do_pos <- function (f, x, right = FALSE, no_match = NA_integer_) {
  ind <- if (right) 
    rev(seq_along(x))
  else seq_along(x)
  for (i in ind) if (f(x[[i]])) 
    return(i)
  no_match
}

