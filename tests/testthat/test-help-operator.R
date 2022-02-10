test_that("?-operator returns correctly", {
  x <- c(T, T, F)
  y <- c(9, 5, 1)
  z <- c(1, 2, 3)
  res <- c(9, 5, 3)
  expect_equal(ifelse(x, y, z), c(T, T, F) ? c(9, 5, 1) ~ c(1, 2, 3))
})
