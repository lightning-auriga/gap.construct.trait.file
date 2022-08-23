test_that("transform.variable returns unmodified input when transform type is null", {
  input.vec <- 1:10
  output.vec <- transform.variable(input.vec, NULL, list())
  expect_identical(output.vec, input.vec)
})

test_that("transform.variable returns transformed variable with no stratification", {
  input.vec <- 1:10
  output.vec <- transform.variable(input.vec, "inverse_normal_transform", list())
  expected.vec <- c(
    -1.6448536,
    -1.0364334,
    -0.6744898,
    -0.3853205,
    -0.1256613,
    0.1256613,
    0.3853205,
    0.6744898,
    1.0364334,
    1.6448536
  )
  expect_equal(output.vec, expected.vec, tolerance = 1e-6)
})

test_that("transform.variable returns transformed variable with stratification", {
  input.vec <- 1:9
  strat.var <- factor(c(rep(1, 5), rep(2, 4)))
  strat.vars <- list(strat.var)
  output.vec <- transform.variable(input.vec, "inverse_normal_transform", strat.vars)
  expected.vec <- c(
    -1.5932188,
    -0.5894558,
    0.0,
    0.5894558,
    1.5932188,
    -0.9674216,
    -0.2822161,
    0.2822161,
    0.9674216
  )
  expect_equal(output.vec, expected.vec, tolerance = 1e-6)
})

test_that("transform.variable correctly errors out when unrecognized transformation is provided", {
  input.vec <- 1:10
  expect_error(transform.variable(input.vec, "fake_transform", list()))
})
