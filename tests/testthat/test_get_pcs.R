eig.data <- data.frame(
  ID = c("A", "B", "C", "D", "E"),
  PC1 = as.numeric(1:5),
  PC2 = as.numeric(6:10),
  PC3 = as.numeric(11:15),
  stat = c("unrel", "unrel", "unrel", "rel", "unrel")
)
rownames(eig.data) <- eig.data[, 1]

test_that("get.pcs detects input types", {
  expect_error(get.pcs("x", as.integer(10), c("A")))
  expect_error(get.pcs(eig.data, 10, c("A")))
  expect_error(get.pcs(eig.data, as.integer(10), 10))
})

test_that("get.pcs prohibits selecting more PCs than are actually present", {
  expect_error(get.pcs(eig.data, as.integer(10), c("A", "B")))
})

test_that("get.pcs selects the correct columns in the correct order", {
  expected.df <- data.frame(
    PC1 = as.numeric(3:4),
    PC2 = as.numeric(8:9)
  )
  rownames(expected.df) <- c("C", "D")
  expect_identical(
    get.pcs(eig.data, as.integer(2), c("C", "D")),
    expected.df
  )
})

test_that("get.pcs sets subjects absent from eigenvector data to NA", {
  expected.df <- data.frame(
    PC1 = c(3, NA, 4),
    PC2 = c(8, NA, 9)
  )
  rownames(expected.df) <- c("C", "X", "D")
  expect_identical(
    get.pcs(eig.data, as.integer(2), c("C", "X", "D")),
    expected.df
  )
})
