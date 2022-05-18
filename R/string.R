#' paste0 but concise
#'
#' @inheritParams base::paste0
#' @export
p0 <- function(..., collapse = "", recycle0 = FALSE) {
  paste0(..., collapse = collapse, recycle0 = recycle0)
}

#' strsplit
#'
#' @inheritParams base::strsplit
#' @param use_bytes logical. If TRUE the matching is done byte-by-byte rather
#'  than character-by-character, and inputs with marked encodings are not
#'   converted.
#' @param unlist logical. Should the result ve a vector?
#' @export
chrsplit <- function(x, split = "", unlist = TRUE, fixed = FALSE,
                     perl = FALSE, use_bytes = FALSE) {
  if (unlist) {
    ul(strsplit(x = x, split = split, fixed = fixed,
                perl = perl, useBytes = use_bytes))
  } else {
    strsplit(x = x, split = split, fixed = fixed,
             perl = perl, useBytes = use_bytes)
  }
}