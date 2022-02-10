test_that("multiplication works", {
  string <- "a test"
  expect_equal(string |> msub("test", "t"), "a t")
  expect_equal(string |> masub("t", "g"), "a gesg")
})
