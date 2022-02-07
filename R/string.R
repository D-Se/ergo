#' @export
msub <- function (x, pattern, replacement, ignore.case = FALSE, perl = FALSE, 
                 fixed = FALSE, useBytes = FALSE) {
  if (is.factor(x) && length(levels(x)) < length(x)) {
    sub(pattern, replacement, levels(x), ignore.case, perl, 
        fixed, useBytes)[x]
  }
  else {
    if (!is.character(x)) 
      x <- as.character(x)
    .Internal(sub(as.character(pattern), as.character(replacement), 
                  x, ignore.case, perl, fixed, useBytes))
  }
}

#' @export
masub <- function (x, pattern, replacement, ignore.case = FALSE, perl = FALSE, 
                  fixed = FALSE, useBytes = FALSE) {
  if (is.factor(x) && length(levels(x)) < length(x)) {
    gsub(pattern, replacement, levels(x), ignore.case, perl, 
         fixed, useBytes)[x]
  }
  else {
    if (!is.character(x)) 
      x <- as.character(x)
    .Internal(gsub(as.character(pattern), as.character(replacement), 
                   x, ignore.case, perl, fixed, useBytes))
  }
}

#' @export
mpos <- function (x, pattern, ignore.case = FALSE, perl = FALSE, value = FALSE, 
                  fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  pattern <- as.character(pattern)
  if (is.factor(x) && length(levx <- levels(x)) < length(x) && 
      !is.na(pattern[1L])) {
    value <- is.character(idxna <- suppressWarnings(grep(pattern, 
                                                         NA_character_, ignore.case, perl, value, fixed, useBytes, 
                                                         invert)))
    idx <- logical(length(levx))
    idx[grep(pattern, levx, ignore.case, perl, FALSE, fixed, 
             useBytes, invert)] <- TRUE
    idx <- idx[x]
    if (length(idxna)) 
      idx[is.na(x)] <- TRUE
    idx <- which(idx)
    if (value) {
      idx <- x[idx]
      structure(as.character(idx), names = names(idx))
    }
    else idx
  }
  else {
    if (!is.character(x)) 
      x <- structure(as.character(x), names = names(x))
    .Internal(grep(pattern, x, ignore.case, value, perl, 
                   fixed, useBytes, invert))
  }
}

#' @export
mlgl <- function (x, pattern, ignore.case = FALSE, perl = FALSE, fixed = FALSE, 
                   useBytes = FALSE) {
  if (is.factor(x) && length(levels(x)) < length(x)) {
    out <- grepl(pattern, c(levels(x), NA_character_), ignore.case, 
                 perl, fixed, useBytes)
    outna <- out[length(out)]
    out <- out[x]
    out[is.na(x)] <- outna
    out
  }
  else {
    if (!is.character(x)) 
      x <- as.character(x)
    .Internal(grepl(as.character(pattern), x, ignore.case, 
                    FALSE, perl, fixed, useBytes, FALSE))
  }
}

#' @export
mind <- function (x, pattern, ignore.case = FALSE, perl = FALSE, fixed = FALSE, 
                     useBytes = FALSE) {
  if (is.factor(x) && length(levels(x)) < length(x)) {
    out <- regexpr(pattern, levels(x), ignore.case, perl, 
                   fixed, useBytes)
    structure(out[x], match.length = attr(out, "match.length")[x], 
              index.type = attr(out, "index.type"), useBytes = attr(out, 
                                                                    "useBytes"), capture.start = attr(out, 
                                                                                                      "capture.start")[x, , drop = FALSE], 
              capture.length = attr(out, "capture.length")[x, 
                                                           , drop = FALSE], capture.names = attr(out, "capture.names"))
  }
  else {
    if (!is.character(x)) 
      x <- as.character(x)
    .Internal(regexpr(as.character(pattern), x, ignore.case, 
                      perl, fixed, useBytes))
  }
}

