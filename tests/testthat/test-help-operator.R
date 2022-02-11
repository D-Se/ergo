test_that("?-operator returns correctly", {
  x <- c(T, T, F)
  y <- c(9, 5, 1)
  z <- c(1, 2, 3)
  # res <- c(9, 5, 3)
  # ternary ? equals to vectorized ifelse
  expect_equal(
    ifelse(x, y, z),
    c(T, T, F) ? c(9, 5, 1) ~ c(1, 2, 3)
    )
  
  # ternary ? equals to single if .. else
  expect_equal(
    if(5 > 3) 10 else 3,
    5 > 3 ? 10 ~ 3
    )
  
  # binary ? converts types without dots passed
  expect_equal(
    as.character(y),
    y ?~ chr
    )
  
  # binary ? converts types with dots passed
  #expect_equal(
   # as.integer(y, 2, 3),
    #y ?~ int(2, 3)
    #)
})
