context("Joining strings")

test_that("basic case works", {
  test <- c("a", "b", "c")

  expect_that(str_c(test), equals(test))
  expect_that(str_c(test, sep = " "), equals(test))
  expect_that(str_c(test, collapse = ""), equals("abc"))
})

test_that("NULLs are dropped", {
  test <- letters[1:3]

  expect_equal(str_c(test, NULL), test)
  expect_equal(str_c(test, NULL, "a", sep = " "), c("a a", "b a", "c a"))
})
