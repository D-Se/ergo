#' sub but named wrapped for consistency
#' 
#' @inheritParams base::sub
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes should bytes be used?
#' @export
msub <- function (x, pattern, replacement, ignore_case = FALSE, perl = FALSE, 
                 fixed = FALSE, use_bytes = FALSE) {
  sub(pattern = pattern, replacement = replacement, x = x,
      ignore.case = ignore_case, perl = perl, fixed = fixed, useBytes = use_bytes)
}

#' gsub but named for consistency
#' 
#' @inheritParams base::gsub
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes should bytes be used?
#' @export
masub <- function (x, pattern, replacement, ignore_case = FALSE, perl = FALSE, 
                  fixed = FALSE, use_bytes = FALSE) {
  gsub(pattern = pattern, replacement = replacement, x = x,
       ignore.case = ignore_case, perl = perl, fixed = fixed, useBytes = use_bytes)
}

#' gregexpr, but wrapped for consistency
#' 
#' @inheritParams base::gregexpr
#' @param ignore_case should lettercase be ignored?
#' 
#' @export
mind <- function (x, pattern, ignore_case = FALSE, perl = FALSE, fixed = FALSE) {
  gregexpr(pattern = pattern, text = x, ignore.case = ignore_case,
           perl = perl, fixed = fixed)
}

#' grepl, but wrapped for consistency
#' 
#' @inheritParams base::grepl
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes should bytes be used?
#' 
#' @export
mlgl <- function (x, pattern, ignore_case = FALSE, perl = FALSE, fixed = FALSE, 
                   use_bytes = FALSE) {
  grepl(pattern = pattern, x = x, ignore.case = ignore_case,
        perl = perl, fixed = fixed, useBytes = use_bytes)
}

#' grep, but wrapped for consistency
#' 
#' @inheritParams base::grep
#' @param ignore_case should lettercase be ignored?
#' @param use_bytes should bytes be used?
#' 
#' @export
mpos <- function (x, pattern, ignore_case = FALSE, perl = FALSE, value = FALSE,
                  fixed = FALSE, use_bytes = FALSE, invert = FALSE) {
  grep(pattern = pattern, x = x, ignore.case = ignore_case, perl = perl,
       value = value, fixed = fixed, useBytes = use_bytes, invert = invert)
}

#' paste0 but concise
#' 
#' @inheritParams base::paste0
#' @export
p0 <- function (..., collapse = "", recycle0 = FALSE) {
  paste0(..., collapse = collapse, recycle0 = recycle0)
}
