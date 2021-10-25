output.df <- data.frame(
  IID = c("A", "B", "C", "D", "E", "F", "G"),
  TV5 = 8:14
)
pheno.df <- data.frame(
  TV1 = 1:7,
  TV2 = c("pineapple", "banana", "banana", "pineapple", "apple", "kiwi", NA),
  ID = c("A", "B", "C", "D", "E", "F", "G"),
  TV3_derived = c(1.23, 3.45, 5.67, 6.78, 7.89, 8.90, 9.10)
)
pheno.config <- list(
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

eigenvectors <- data.frame(
  V1 = c("B", "C", "D", "E", "G"),
  V2 = c(1, 3, 5, 7, 9),
  V3 = c(2.2, 4.4, 6.6, 8.8, 10.1),
  V4 = c(-2.1, -1, -0.4, 0.4, 0.44),
  V5 = c(-0.3, -0.3, 0.3, 0.3, 0.01),
  V6 = c("rel", "unrel", "unrel", "unrel", "unrel"),
  stringsAsFactors = FALSE
)
rownames(eigenvectors) <- eigenvectors[, 1]

analysis.config <- list(analyses = list(
  c1 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c1-test",
    "phenotype" = "TV2",
    pcs = 3
  ),
  c2 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c2-test",
    "phenotype" = "TV3_derived",
    "covariates" = c("TV1", "TV2"),
    pcs = 0
  ),
  c3 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c3-test",
    "phenotype" = "TV4",
    "covariates" = c("TV1", "TV2"),
    pcs = 1
  ),
  c4 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c4-test",
    "phenotype" = "TV2",
    "covariates" = c("TV3_derived"),
    pcs = 0
  ),
  c5 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c5-test",
    "phenotype" = "TV2",
    "covariates" = c("TV1", "TV4_derived"),
    pcs = 0
  )
))
analysis.name <- "c2"
collapse.limit <- as.integer(1)

test_that("construct.covariate.output checks input types", {
  expect_error(construct.covariate.output(
    "x", pheno.df,
    pheno.config, eigenvectors,
    analysis.config, analysis.name,
    collapse.limit
  ))
  expect_error(construct.covariate.output(
    output.df, "x",
    pheno.config, eigenvectors,
    analysis.config, analysis.name,
    collapse.limit
  ))
  expect_error(construct.covariate.output(
    output.df, pheno.df,
    "x", eigenvectors,
    analysis.config,
    analysis.name, collapse.limit
  ))
  expect_error(construct.covariate.output(
    output.df, pheno.df,
    pheno.config, "x",
    analysis.config, analysis.name,
    collapse.limit
  ))
  expect_error(construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    "x", analysis.name,
    collapse.limit
  ))
  expect_error(construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, list(),
    collapse.limit
  ))
  expect_error(construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, analysis.name,
    10
  ))
  expect_error(construct.covariate.output(
    output.df[1:5, ], pheno.df,
    pheno.config, eigenvectors,
    analysis.config, analysis.name
  ))
})

test_that("construct.covariate.output adds single covariate", {
  expected.df <- output.df
  expected.df <- cbind(expected.df, pheno.df[, "TV3_derived"])
  colnames(expected.df)[ncol(expected.df)] <- "TV3_derived"
  res <- construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, "c4",
    collapse.limit
  )
  expect_identical(res, expected.df)
})

test_that("construct.covariate.output adds multiple covariates", {
  expected.df <- output.df
  expected.df <- cbind(
    expected.df,
    pheno.df[, "TV1"],
    c(0, 1, 1, 0, 0, 0, 0),
    c(0, 0, 0, 0, 1, 1, 0)
  )
  colnames(expected.df)[seq(ncol(expected.df) - 2, ncol(expected.df))] <-
    c("TV1", "TV2.ref0.cmp1", "TV2.ref0.cmpOTHER")
  res <- construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, "c2", collapse.limit
  )
  expect_identical(res, expected.df)
})

test_that("construct.covariate.output detects missing covariates", {
  expect_error(construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, "c5",
    collapse.limit
  ))
})

test_that("construct.covariate.output adds PCs", {
  expected.df <- output.df
  eigendata <- data.frame(
    c(NA, eigenvectors[1:4, 2], NA, eigenvectors[5, 2]),
    c(NA, eigenvectors[1:4, 3], NA, eigenvectors[5, 3]),
    c(NA, eigenvectors[1:4, 4], NA, eigenvectors[5, 4])
  )
  expected.df <- cbind(
    expected.df,
    eigendata
  )
  colnames(expected.df)[seq(ncol(expected.df) - 2, ncol(expected.df))] <-
    paste("PC", 1:3, sep = "")
  res <- construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, "c1", collapse.limit
  )
  expect_identical(res, expected.df)
})

test_that("construct.covariate.output adds covariates and PCs", {
  expected.df <- output.df
  expected.df <- cbind(
    expected.df,
    pheno.df[, "TV1"],
    c(0, 1, 1, 0, 0, 0, 0),
    c(0, 0, 0, 0, 1, 1, 0),
    c(NA, eigenvectors[1:4, 2], NA, eigenvectors[5, 2])
  )
  colnames(expected.df)[seq(ncol(expected.df) - 3, ncol(expected.df))] <- c(
    "TV1",
    "TV2.ref0.cmp1",
    "TV2.ref0.cmpOTHER",
    "PC1"
  )
  res <- construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, "c3", collapse.limit
  )
  expect_identical(res, expected.df)
})

test_that("construct.covariate.output respects disable.binarization flag", {
  expected.df <- output.df
  expected.df <- cbind(
    expected.df,
    pheno.df[, "TV1"],
    factor(c("0", "1", "1", "0", "3", "4", NA), levels = as.character(0:4))
  )
  colnames(expected.df)[seq(ncol(expected.df) - 1, ncol(expected.df))] <-
    c("TV1", "TV2")
  res <- construct.covariate.output(
    output.df, pheno.df,
    pheno.config, eigenvectors,
    analysis.config, "c2", collapse.limit,
    TRUE
  )
  expect_identical(res, expected.df)
})
