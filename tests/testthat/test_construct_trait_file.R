# set default parameters for function construct.trait.file
phenotype.file <- testthat::test_path("files", "construct_trait_file", "testCV.tsv")
phenotype.config <- testthat::test_path("files", "construct_trait_file", "testCV.yaml")
phenotype.shared.models <- testthat::test_path("files", "construct_trait_file", "shared-models.yaml")
eigenvectors <- testthat::test_path("files", "construct_trait_file", "transformed_eigenvalues.tsv")
plink.format <- TRUE
phenotype.output <- TRUE
covariate.output <- FALSE
analysis.config <- testthat::test_path("files", "construct_trait_file", "analysis.yaml")
analysis.name <- "testcv"
collapse.limit <- as.integer(0)
id.linker <- testthat::test_path("files", "construct_trait_file", "id_linker.tsv")

test_that("construct.trait.file checks input types", {
  expect_error(construct.trait.file(
    testthat::test_path("files", "construct_trait_file", "doesntexist.tsv"),
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    testthat::test_path("files", "construct_trait_file", "doesntexist.yaml"),
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    testthat::test_path("files", "construct_trait_file", "doesntexist.yaml"),
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    testthat::test_path("files", "construct_trait_file", "doesntexist.yaml"),
    analysis.name,
    collapse.limit,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    11.2,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    NA,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    NA,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    "string",
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ))
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    "nope",
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ))
})

test_that("construct.trait.file applies shared models to phenotype config", {
  expected.df <- data.frame(
    FID = "0", IID = c("A001", "B001", "C001"),
    phenotype3 = c(0, 1, 0)
  )
  expect_identical(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    "testshared",
    collapse.limit,
    id.linker
  ), expected.df)
})

test_that("construct.trait.file detects analysis name absent from analysis config", {
  expect_error(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    "testmissing",
    collapse.limit,
    id.linker
  ))
})

test_that("construct.trait.file maps subject IDs", {
})

test_that("construct.trait.file behaves as expected without ID linker", {
  expected.df <- data.frame(FID = "0", IID = c("A", "B", "C"), phenotype1 = c(0, 1, 1.6))
  expect_identical(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    NA
  ), expected.df)
})

test_that("construct.trait.file drops subject IDs that are NA", {
  expected.df <- data.frame(FID = "0", IID = c("A001", "B001"), phenotype1 = c(0, 1))
  expect_identical(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    testthat::test_path("files", "construct_trait_file", "id_linker_with_NA.tsv")
  ), expected.df)
})

test_that("construct.trait.file drops subject IDs that are duplicates from plink output", {
  expected.df <- data.frame(FID = "0", IID = c("A001"), phenotype1 = c(0))
  expect_identical(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    testthat::test_path("files", "construct_trait_file", "id_linker_with_dups.tsv")
  ), expected.df)
})

test_that("construct.trait.file formats non-plink-style output as instructed", {
  # when we support something other than plink, test that here
})

test_that("construct.trait.file returns covariate data only on request", {
  expected.df <- data.frame(
    FID = "0", IID = c("A001", "B001", "C001"),
    phenotype2 = c(NA, "crocodile", "llama"),
    phenotype3 = c(0, 1, 0),
    PC1 = c(-0.0006505122, -0.7071067812, 0.7071067812),
    PC2 = c(-3.3568964, 0.7071067812, -0.70710678123)
  )
  expect_equal(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    FALSE,
    TRUE,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ), expected.df, tolerance = 1e-5)
})

test_that("construct.trait.file returns phenotype data only on request", {
  expected.df <- data.frame(FID = "0", IID = c("A001", "B001", "C001"), phenotype1 = c(0, 1, 1.6))
  expect_identical(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    covariate.output,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ), expected.df)
})

test_that("construct.trait.file returns covariate and phenotype data only on request", {
  expected.df <- data.frame(
    FID = "0", IID = c("A001", "B001", "C001"),
    phenotype1 = c(0, 1, 1.6),
    phenotype2 = c(NA, "crocodile", "llama"),
    phenotype3 = c(0, 1, 0),
    PC1 = c(-0.0006505122, -0.7071067812, 0.7071067812),
    PC2 = c(-3.3568964, 0.7071067812, -0.70710678123)
  )
  expect_equal(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    phenotype.output,
    TRUE,
    analysis.config,
    analysis.name,
    collapse.limit,
    id.linker
  ), expected.df, tolerance = 1e-5)
})

test_that("construct.trait.file respects disable.binarization flag", {
  expected.df <- data.frame(
    FID = "0", IID = c("A001", "B001", "C001"),
    phenotype4 = factor(c("20", "30", "99"), levels = c("20", "30", "99"))
  )
  expect_identical(construct.trait.file(
    phenotype.file,
    phenotype.config,
    phenotype.shared.models,
    eigenvectors,
    plink.format,
    FALSE,
    TRUE,
    analysis.config,
    "testfactor",
    collapse.limit,
    id.linker,
    TRUE
  ), expected.df)
})
