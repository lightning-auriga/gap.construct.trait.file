df <- data.frame(
  TV1 = 1:7,
  TV2 = c("pineapple", "banana", "banana", "pineapple", "apple", "kiwi", NA),
  ID = c("A", "B", "C", "D", "E", "F", "G"),
  TV3_derived = c(1.23, 3.45, 5.67, 6.78, 7.89, 8.90, 9.10)
)
config <- list(
  variables = list(
    TV1 = list(type = "numeric"),
    TV2 = list(
      type = "categorical",
      levels = list(
        "0" = list(name = "pineapple"),
        "1" = list(name = "banana"),
        "2" = list(name = "mango"),
        "3" = list(name = "apple"),
        "4" = list(name = "kiwi")
      )
    ),
    ID = list(
      type = "string",
      subject_id = TRUE
    )
  ),
  derived = list(TV3_derived = list(type = "numeric"))
)

test_that("restructure.variable checks input types", {
  expect_error(restructure.variable("x", list(), "y", as.integer(20), FALSE))
  expect_error(restructure.variable(data.frame(), 12, "y", as.integer(20), FALSE))
  expect_error(restructure.variable(data.frame(), list(), 123, as.integer(20), FALSE))
  expect_error(restructure.variable(data.frame(), list(), c("x", "y"), as.integer(20), FALSE))
  expect_error(restructure.variable(data.frame(), list(), "y", "x", FALSE))
  expect_error(restructure.variable(data.frame(), list(), "y", as.integer(12), 234))
})

test_that("restructure.variable complains about invalid variable types", {
  expect_error(restructure.variable(df, config, "TV2", as.integer(2), TRUE))
})

test_that("restructure.variable creates binary dummies with predictable headers", {
  expected.df <- data.frame(
    TV2.ref0.cmp1 = c(0, 1, 1, 0, 0, 0, 0),
    TV2.ref0.cmpOTHER = c(0, 0, 0, 0, 1, 1, 0)
  )
  expect_identical(
    restructure.variable(df, config, "TV2", as.integer(1), FALSE),
    expected.df
  )
})

test_that("restructure.variable distinguishes between factor and non-factor variables", {
  expected.df <- data.frame(TV1 = df$TV1)
  expect_identical(
    restructure.variable(df, config, "TV1", as.integer(1), FALSE),
    expected.df
  )
})

test_that("restructure.variable understands variables in derived variable configuration block", {
  expected.df <- data.frame(TV3_derived = df$TV3_derived)
  expect_identical(
    restructure.variable(df, config, "TV3_derived", as.integer(1), FALSE),
    expected.df
  )
})