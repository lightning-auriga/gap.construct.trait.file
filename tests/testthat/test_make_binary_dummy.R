vec <- factor(c("A", "A", "B", "A", "C", "C", "C", "C", "D", NA, "D", "D", NA, "E"))

test_that("make.binary.dummy creates a binary variable", {
  expect_identical(
    make.binary.dummy(vec[!is.na(vec)], "C"),
    c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  )
})

test_that("make.binary.dummy collapses levels correctly", {
  expect_identical(
    make.binary.dummy(vec[!is.na(vec)], c("A", "D")),
    c(1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0)
  )
})

test_that("make.binary.dummy handles NA values in input correctly", {
  expect_identical(
    make.binary.dummy(vec, "A"),
    c(1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
})
