#' @export
expand <- expand.grid

#' detailed run length encoding in data.frame format
#' 
#' @export
rlen <- function (x) {
  if (!is.vector(x) && !is.list(x)) stop("'x' must be a vector of an atomic type")
  n <- length(x)
  if (n == 0L) return(data.frame(lengths = integer(), values = x))
  y <- x[-1L] != x[-n]
  i <- c(which(y | is.na(y)), n)
  within(
    data.frame(
      lengths = diff(c(0L, i)),
      values = x[i]), {
        end = cumsum(lengths)
        start = c(1, end)[1:length(end)]
      })
}

#' rapid tidyverse prototyping
#' 
#' @export
#' @examples
#' \dontrun{
#' iris %>%
#'   m(n = 1:n()) %>%
#'   f(Sepal.Length > 5) |>
#'   tv()
#' }
tv <- function(expr){
  eval(load_tv()) # load package abbreviations
  expr <- paste0("{", deparse1(substitute(expr)), "}", collapse = "")
  eval(str2expression(expr))
}