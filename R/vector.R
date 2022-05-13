#' Abbreviated scalar
#'
#' @inheritParams base::character
#'
#' @export
chr <- function(length = 0) {
  character(length)
}

#' Abbreviated scalar
#'
#' @inheritParams base::numeric
#' @export
num <- function(length = 0) {
  numeric(length)
}

#' Abbreviated scalar
#'
#' @inheritParams base::integer
#' @export
int <- function(length = 0) {
  integer(length)
}

#' Abbreviated scalar
#'
#' @inheritParams base::logical
#' @export
lgl <- function(length = 0) {
  logical(length)
}

#' Abbreviated scalar
#'
#' @inheritParams base::list
#' @param names a character vector of list element names
#' @importFrom stats setNames
#' @export
lst <- function(..., names = NULL) {
  list(...) |> setNames(names)
}

#' Abbreviated scalar
#'
#' @inheritParams base::factor
#' @export
fct <- function(x = character(), levels, labels = levels,
                exclude = NA, ordered = is.ordered(x), nmax = NA) {
  factor(x = x, levels = levels, labels = labels, exclude = exclude,
         ordered = ordered, nmax = nmax)
}

#' data.frame, but with consistent argument formatting.
#'
#' @inheritParams base::data.frame
#' @param row_names NULL or a single integer or character string specifying a
#' column to be used as row names, or a character or integer vector giving
#' the row names for the data frame.
#' @param check_rows if TRUE then the rows are checked for consistency of
#'  length and names.
#' @param check_names logical. If TRUE then the names of the variables in the
#'  data frame are checked to ensure that they are syntactically valid variable
#'   names and are not duplicated. If necessary they are adjusted (by
#'   make.names) so that they are.
#' @param fix_empty_names logical indicating if arguments which are “unnamed”
#' (in the sense of not being formally called as someName = arg) get an
#'  automatically constructed name or rather name "". Needs to be set to FALSE
#'   even when check.names is false if "" names should be kept.
#' @param strings_as_factors logical should character vectors be converted to
#'  factors? The ‘factory-fresh’ default has been TRUE previously but has been
#'  changed to FALSE for R 4.0.0.
#'
#' @export
dfr <- function(..., row_names = NULL, check_rows = FALSE, check_names = TRUE,
                fix_empty_names = TRUE, strings_as_factors = FALSE) {
  data.frame(..., row.names = row_names, check.rows = check_rows,
             check.names = check_names, fix.empty.names = fix_empty_names,
             stringsAsFactors = strings_as_factors)
}

#' Abbreviated scalar
#'
#' @inheritParams base::matrix
#' @param by_row logical. If FALSE (the default) the matrix is filled by
#'  columns, otherwise the matrix is filled by rows.
#' @export
mtx <- function(data = NA, nrow = 1, ncol = 1,
                by_row = FALSE, dimnames = NULL) {
  matrix(data = data, nrow = nrow, ncol = ncol, byrow = by_row,
         dimnames = dimnames)
}

#' Abbreviated scalar
#'
#' @inheritParams base::array
#' @export
arr <- function(data = NA, dim = length(data), dimnames = NULL) {
  array(data = data, dim = dim, dimnames = dimnames)
}

#' Abbreviated scalar
#' @inheritParams base::vector
#"
#' @export
vec <- function(mode = "logical", length = 0) {
  vector(mode = mode, length = length)
}