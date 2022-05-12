# Code by Hadley Wickham Adv. R, ch 8.
e <- function(subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(subclass, "error", "condition")
  )
  stop(err)
}