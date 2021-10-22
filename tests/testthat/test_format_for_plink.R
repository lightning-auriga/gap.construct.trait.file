df <- data.frame(
  IID = c("A", "B", "C", "D", "E"),
  V1 = as.numeric(1:5),
  V2 = as.numeric(6:10)
)

test_that("format.for.plink detects input type", {
  expect_error(format.for.plink("filename"))
})

test_that("format.for.plink results in FID,IID as first two columns", {
  res <- format.for.plink(df)
  expect_identical(
    colnames(res)[1:2],
    c("FID", "IID")
  )
})

test_that("format.for.plink results in FID that is exclusively 0", {
  res <- format.for.plink(df)
  expect_identical(
    res[, 1],
    rep("0", nrow(res))
  )
})

test_that("format.for.plink removes duplicate subject IDs", {
  df2 <- rbind(df, df[1:3, ])
  df2[6, 2] <- 4.5
  res <- format.for.plink(df2)
  expected.df <- df[4:5, ]
  expected.df <- cbind(rep("0", 2), expected.df)
  colnames(expected.df)[1] <- "FID"
  expect_identical(res, expected.df)
})
