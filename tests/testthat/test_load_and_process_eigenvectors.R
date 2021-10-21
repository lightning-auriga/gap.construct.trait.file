filename <- "files/load_and_process_eigenvectors/smartpca.evec"
filename.bad1 <- "files/load_and_process_eigenvectors/smartpca.evec.mal1"
filename.bad2 <- "files/load_and_process_eigenvectors/smartpca.evec.mal2"

test_that("load.and.process.eigenvectors checks input type", {
  expect_error(load.and.process.eigenvectors(data.frame(A = 1:4, B = 5:8)))
  expect_error(load.and.process.eigenvectors(c(filename, "other_filename.tsv")))
})

test_that("load.and.process.eigenvectors enforces smartpca format", {
  expect_error(load.and.process.eigenvectors(filename.bad1))
  expect_error(load.and.process.eigenvectors(filename.bad2))
})

test_that("load.and.process.eigenvectors performs transformation with unrelated subset metrics", {
  expected.df <- data.frame(
    V1 = c("A", "B", "C", "D", "E"),
    V2 = c(-0.2793503, -1.3906119, 0.8339578, -0.0501398, 0.6067939),
    V3 = c(-0.2262726, 1.0707862, 0.6194287, -0.7013857, -0.9888292),
    V4 = c(0.40933356, 0.07050218, -0.92073744, -0.51982205, 1.37005731),
    V5 = c("rel", "unrel", "unrel", "unrel", "unrel"),
    stringsAsFactors = FALSE
  )
  rownames(expected.df) <- expected.df[, 1]
  expect_equal(load.and.process.eigenvectors(filename),
    expected.df,
    tolerance = 1e-5
  )
})
