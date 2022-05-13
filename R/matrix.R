#' rowSums, but consistent with terminology
#'
#' @inheritParams base::rowSums
#' @export
rsum <- function(x, na_rm = FALSE, dims = 1L) {
  rowSums(x = x, na.rm = na_rm, dims = dims)
}

#' rowMeans, but consistent with terminology
#'
#' @inheritParams base::rowMeans
#' @export
rmean <- function(x, na_rm = FALSE, dims = 1L) {
  rowMeans(x = x, na.rm = na_rm, dims = dims)
}

#' cowSums, but consistent with terminology
#'
#' @inheritParams base::colSums
#' @export
csum <- function(x, na_rm = FALSE, dims = 1L) {
  colSums(x = x, na.rm = na_rm, dims = dims)
}

#' colMeans, but consistent with terminology
#'
#' @inheritParams base::colMeans
#' @export
cmean <- function(x, na_rm = FALSE, dims = 1L) {
  colMeans(x = x, na.rm = na_rm, dims = dims)
}

#' rownames, but consistent with terminology
#'
#' @inheritParams base::rownames
#' @param do_NULL logical. If FALSE and names are NULL, names are created.
#' @export
rnms <- function(x, do_NULL = TRUE, prefix = "row") {
  rownames(x = x, do.NULL = do_NULL, prefix = prefix)
}

#' colnames, but consistent with terminology
#'
#' @inheritParams base::colnames
#' @param do_NULL logical. If FALSE and names are NULL, names are created.
#' @export
cnms <- function(x, do_NULL = TRUE, prefix = "col") {
  rownames(x = x, do.NULL = do_NULL, prefix = prefix)
}
