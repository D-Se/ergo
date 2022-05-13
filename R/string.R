#' sub but named wrapped for consistency
#'
#' @inheritParams base::sub
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes logical. If TRUE the matching is done byte-by-byte rather
#' than character-by-character, and inputs with marked encodings are not
#' converted.
#' @export
msub <- function(x, pattern, replacement, ignore_case = FALSE, perl = FALSE,
                 fixed = FALSE, use_bytes = FALSE) {
  sub(pattern = pattern, replacement = replacement, x = x,
      ignore.case = ignore_case, perl = perl, fixed = fixed,
      useBytes = use_bytes)
}

#' gsub but named for consistency
#'
#' @inheritParams base::gsub
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes logical. If TRUE the matching is done byte-by-byte rather
#'  than character-by-character, and inputs with marked encodings are not
#'   converted.
#' @export
masub <- function(x, pattern, replacement, ignore_case = FALSE, perl = FALSE,
                  fixed = FALSE, use_bytes = FALSE) {
  gsub(pattern = pattern, replacement = replacement, x = x,
       ignore.case = ignore_case, perl = perl, fixed = fixed,
       useBytes = use_bytes)
}

#' gregexpr, but wrapped for consistency
#'
#' @inheritParams base::gregexpr
#' @param ignore_case should lettercase be ignored?
#' @export
mind <- function(x, pattern, ignore_case = FALSE, perl = FALSE, fixed = FALSE) {
  gregexpr(pattern = pattern, text = x, ignore.case = ignore_case,
           perl = perl, fixed = fixed)
}

#' grepl, but wrapped for consistency
#'
#' @inheritParams base::grepl
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes logical. If TRUE the matching is done byte-by-byte rather
#'  than character-by-character, and inputs with marked encodings are not
#'   converted.
#' @export
mlgl <- function(x, pattern, ignore_case = FALSE, perl = FALSE, fixed = FALSE,
                   use_bytes = FALSE) {
  grepl(pattern = pattern, x = x, ignore.case = ignore_case,
        perl = perl, fixed = fixed, useBytes = use_bytes)
}

#' grep, but wrapped for consistency
#'
#' @inheritParams base::grep
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes logical. If TRUE the matching is done byte-by-byte rather
#'  than character-by-character, and inputs with marked encodings are not
#'   converted.
#' @export
mpos <- function(x, pattern, ignore_case = FALSE, perl = FALSE, value = FALSE,
                  fixed = FALSE, use_bytes = FALSE, invert = FALSE) {
  grep(pattern = pattern, x = x, ignore.case = ignore_case, perl = perl,
       value = value, fixed = fixed, useBytes = use_bytes, invert = invert)
}

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