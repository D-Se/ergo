box::use(purrr[map_dbl])

#### Equal ####
test_that("?-op output equals single valid if-statement", {
  expect_equal(
    if (5 > 3) 10 else 3,
    5 > 3 ? 10 ~ 3
  )
  expect_equal(
    if (5 > 3) 10,
    5 > 3 ? 10 ~ .
  )
  expect_equal(
    if (5 < 3) 10,
    5 < 3 ? 10 ~ .
  )
})

test_that("?-op ternary returns correctly", {
  x <- c(T, T, F)
  y <- c(9, 5, 1)
  z <- 1:3

  expect_equal(ifelse(x, y, z), x ? y ~ z)
  # expressions are evaluated
  expect_equal(
    ifelse(x, sum(y), sum(z)),
    x ? sum(y) ~ sum(z)
  )

  df <- cbind.data.frame(l = c(letters[1:5], NA), L = c(LETTERS[1:5], NA))
  expect_equal(
    df |> transform(new = l %in% c("a", "d", "f") ? 1 ~ {
      L == "B" ? 2 ~ 3
    }),
    df |> transform(new = ifelse(l %in% c("a", "d", "f"), 1,
      ifelse(L == "B", 2, 3)
    ))
  )
})

test_that("?-op converts types correctly", {
  # binary ? converts types without dots passed
  v <- 5
  l <- list(a = 1, b = 2, "x")

  expect_equal(as.character(v), v ?~chr)
  expect_equal(as.numeric(v), v ?~num)
  expect_equal(as.list(v), v ?~lst)
  expect_equal(as.integer(v), v ?~int)
  expect_equal(as.matrix(v), v ?~mtx)
  expect_equal(as.array(v), v ?~arr)

  ### FIXME: https://github.com/D-Se/ergo/issues/2
  ### FIXME expect_equal(as.data.frame(v), v ?~ dfr)

  expect_equal(as.environment(list(x = 5)), list(x = 5) ?~env)

  expect_equal(as.data.frame(l), l ?~dfr)
})

test_that("?-op converts types passes arguments", {
  # binary ? converts types without dots passed
  e <- list(x = 1, y = 2, .z = 10) |> list2env()
  expect_equal(as.list(e, sorted = TRUE), e ?~ lst[sorted = TRUE])
  expect_equal(as.list(e, sorted = FALSE), e ?~ lst[sorted = FALSE])
  expect_equal(
    as.list(e, sorted = TRUE, all.names = TRUE),
    e ?~ lst[sorted = TRUE, all.names = TRUE]
  )
})


test_that("?-operator is pipe friendly", {
  
  expect_equal(1:5 |> sum() > 10 ? 10 ~ 5, 10)
  expect_equal(1:4 |> sum() > 10 ? 10 ~ 5, 5)
})

test_that("?-operator substitutes correctly in tidyverse", {
  expect_equal(
    map_dbl(1:5, ~ {
      .x + 1 > 5 ? 10 ~ 5
    }),
    c(5, 5, 5, 5, 10)
  )
})

test_that("?-operator throws errors within tidyverse mapper", {
  expect_error(
    map_dbl(1:5, ~ {
      .x + 1 > 5 ? 10 ~ .
    }),
    class = "purrr_error_bad_element_vector"
  )
})

test_that("passing arguments to ?-op works correctly", {
  l <- list(1, 2, list(c = 3, 4))
  expect_equal(
    l ?~ lapply[\(.) length(.)],
    lapply(l, \(.) length(.))
  )
  expect_equal(
    l ?~ unlist[use.names = T],
    unlist(l, use.names = T)
  )
  expect_equal(
    l ?~ unlist[use.names = F],
    unlist(l, use.names = F)
  )
  expect_equal(
    l ?~ unlist[use.names = F, recursive = F],
    unlist(l, recursive = F, use.names = F)
  )
  expect_equal(
    l ?~ unlist[, F],
    unlist(l, , F)
  )
})

test_that("?-op checks types correctly", {
  x <- 1:3
  expect_equal(
    is.character(x),
    x ? chr
  )
  expect_equal(
    x ? num,
    is.numeric(x)
  )
})

#### Errors ####
test_that("?-op throws the same errors as single if-statement", {
  expect_error(NA ? 5 ~ 3, "missing value where TRUE/FALSE needed")
  expect_error(NaN ? 3 ~ 3, "argument is not interpretable as logical")
  expect_error(NULL ? 3 ~ 3, "argument is of length zero")
})
