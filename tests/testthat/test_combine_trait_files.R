phenodata1 <- testthat::test_path("files", "combine_trait_files", "phenodata1.tsv")
phenodata1.forint <- testthat::test_path("files", "combine_trait_files", "phenodata1_forint.tsv")
phenoconfig1 <- testthat::test_path("files", "combine_trait_files", "phenodata1.yaml")
phenodata2 <- testthat::test_path("files", "combine_trait_files", "phenodata2.tsv")
phenodata2.forint <- testthat::test_path("files", "combine_trait_files", "phenodata2_forint.tsv")
phenoconfig2 <- testthat::test_path("files", "combine_trait_files", "phenodata2.yaml")
phenodata3 <- testthat::test_path("files", "combine_trait_files", "phenodata3.tsv")
phenoconfig3 <- testthat::test_path("files", "combine_trait_files", "phenodata3.yaml")
analysisconfig <- testthat::test_path("files", "combine_trait_files", "analysis.yaml")
analysisconfig.withint <- testthat::test_path("files", "combine_trait_files", "analysis_withint.yaml")
analysisconfig.withint.strat <- testthat::test_path("files", "combine_trait_files", "analysis_withint_strat.yaml")
eigenvectors <- testthat::test_path("files", "combine_trait_files", "eigenvectors.tsv")

test_that("combine.trait.files checks input types", {
  expect_error(combine.trait.files(
    c("", ""), "", "", "",
    TRUE, TRUE, TRUE,
    "", "", as.integer(0), "", FALSE
  ))
  expect_error(combine.trait.files(
    "", c("", ""), "", "",
    TRUE, TRUE, TRUE,
    "", "", as.integer(0), "", FALSE
  ))
  expect_error(combine.trait.files(
    "", "", c("", ""), "",
    TRUE, TRUE, TRUE,
    "", "", as.integer(0), "", FALSE
  ))
  expect_error(combine.trait.files(
    "", "", "", c("", ""),
    TRUE, TRUE, TRUE,
    "", "", as.integer(0), "", FALSE
  ))
  expect_error(combine.trait.files(
    "", "", "", "",
    TRUE, TRUE, TRUE,
    "", "", as.integer(0), c("", ""), FALSE
  ))
  expect_error(combine.trait.files(
    "", "", "", "",
    "", TRUE, TRUE,
    "", "", as.integer(0), "", FALSE
  ))
  expect_error(combine.trait.files(
    1, "", "", "",
    TRUE, TRUE, TRUE,
    "", "", as.integer(0), "", "step"
  ))
})

test_that("combine.trait.files conducts a simple merge of phenotypes", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
    V1 = c(
      0.01, 0.02,
      0.01, -0.1,
      -0.5, 0.03,
      0.1, 0.2,
      0.1, -0.01,
      -0.05, 0.3
    ),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1, phenodata2),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    TRUE,
    FALSE,
    analysisconfig,
    "a1",
    as.integer(0),
    c(NA, NA),
    TRUE
  )
  expect_identical(res, expected.df)
})

test_that("combine.trait.files conducts a simple merge of covariates", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
    V2 = c(0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1),
    gap.merge.batch = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1, phenodata2),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    FALSE,
    TRUE,
    analysisconfig,
    "a1",
    as.integer(0),
    c(NA, NA),
    FALSE
  )
  expect_identical(res, expected.df)
})

test_that("combine.trait.files respects user suppression of batch variable", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
    V2 = c(0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1, phenodata2),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    FALSE,
    TRUE,
    analysisconfig,
    "a1",
    as.integer(0),
    c(NA, NA),
    TRUE
  )
  expect_identical(res, expected.df)
})

test_that("combine.trait.files finds and removes duplicates introduced by merging", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("C", "D", "E", "F", "G", "H", "I", "J"),
    V1 = c(
      0.01, -0.1,
      -0.5, 0.03,
      0.1, -0.01,
      -0.05, 0.3
    ),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1, phenodata3),
    c(phenoconfig1, phenoconfig3),
    c(NA, NA),
    eigenvectors,
    TRUE,
    TRUE,
    FALSE,
    analysisconfig,
    "a1",
    as.integer(0),
    c(NA, NA),
    TRUE
  )
  expect_identical(res, expected.df)
})

test_that("combine.trait.files deals with sporadic binarization seamlessly", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
    V2 = c(0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1),
    V3.ref0.cmp1 = c(0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0),
    V3.ref0.cmp2 = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0),
    gap.merge.batch = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1, phenodata2),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    FALSE,
    TRUE,
    analysisconfig,
    "a2",
    as.integer(0),
    c(NA, NA),
    FALSE
  )
  expect_identical(res, expected.df)
})

test_that("combine.trait.files deals with invariant pre-merge phenotypes seamlessly", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
    V4 = rep(c(0, 1), each = 6),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1, phenodata2),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    TRUE,
    FALSE,
    analysisconfig,
    "a3",
    as.integer(0),
    c(NA, NA),
    FALSE
  )
  expect_identical(res, expected.df)
})

test_that("combine.trait.files applies inverse normal transform across files", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
    V1 = c(
      -1.6906216,
      -0.7478586,
      -0.2298841,
      0.2298841,
      0.7478586,
      1.6906216,
      -1.0968036,
      -0.4727891,
      0.0,
      0.4727891,
      1.0968036
    ),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1.forint, phenodata2.forint),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    TRUE,
    FALSE,
    analysisconfig.withint,
    "a1",
    as.integer(0),
    c(NA, NA),
    TRUE
  )
  expect_equal(res, expected.df, tolerance = 1e-6)
})

test_that("combine.trait.files applies inverse normal transform across files", {
  expected.df <- data.frame(
    FID = "0",
    IID = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
    V1 = c(
      -1.6906216,
      -0.4727891,
      0.4727891,
      -0.2298841,
      0.2298841,
      1.6906216,
      -1.0968036,
      0.0,
      -0.7478586,
      1.0968036,
      0.7478586
    ),
    stringsAsFactors = FALSE
  )
  res <- combine.trait.files(
    c(phenodata1.forint, phenodata2.forint),
    c(phenoconfig1, phenoconfig2),
    c(NA, NA),
    eigenvectors,
    TRUE,
    TRUE,
    FALSE,
    analysisconfig.withint.strat,
    "a1",
    as.integer(0),
    c(NA, NA),
    TRUE
  )
  expect_equal(res, expected.df, tolerance = 1e-6)
})
