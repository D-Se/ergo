#' detailed run length encoding in data.frame format
#'
#' @param x An atomic vector
#' @export
rlen <- function(x) {
  if (!is.vector(x) && !is.list(x)) stop("'x' must be a vector")
  n <- length(x)
  if (n == 0L) return(data.frame(lengths = integer(), values = x))
  y <- x[-1L] != x[-n]
  i <- c(which(y | is.na(y)), n)
  df = within(
    data.frame(
      lengths = diff(c(0L, i)),
      values = x[i]), {
        end <- cumsum(lengths)
        start <- c(1, end)[1:length(end)]
      })
  df
}