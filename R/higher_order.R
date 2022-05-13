#' Reduce but humanly readable
#'
#' @inheritParams base::Reduce
#' @export
do_red <- function(x, f, init, right = FALSE, accumulate = FALSE) {
  Reduce(f = f, x = x, init = init, right = right, accumulate = accumulate)
}

#' Filter, but humanly readable
#'
#' @inheritParams base::Filter
#' @export
do_fil <- function(x, f) {
  Filter(f = f, x = x)
}

#' Find, but humanly readable
#'
#' @inheritParams base::Find
#' @param no_match the value to be returned in the case when “no match”
#' (no element satisfying the predicate) is found.
#'
#' @export
do_find <- function(x, f, right = FALSE, no_match = NULL) {
  Find(f = f, x = x, right = right, nomatch = no_match)
}

#' Position, but humanly readable
#'
#' @inheritParams base::Position
#' @param no_match the value to be returned in the case when “no match”
#'  (no element satisfying the predicate) is found.
#' @export
do_pos <- function(f, x, right = FALSE, no_match = NA_integer_) {
  Position(f, x, right = right, nomatch = no_match)
}
