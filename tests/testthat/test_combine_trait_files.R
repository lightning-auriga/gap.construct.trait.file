phenodata1 <- testthat::test_path("files", "combine_trait_files", "phenodata1.tsv")
phenoconfig1 <- testthat::test_path("files", "combine_trait_files", "phenodata1.yaml")
phenodata2 <- testthat::test_path("files", "combine_trait_files", "phenodata2.tsv")
phenoconfig2 <- testthat::test_path("files", "combine_trait_files", "phenodata2.yaml")
analysisconfig <- testthat::test_path("files", "combine_trait_files", "analysis.yaml")
eigenvectors <- testthat::test_path("files", "combine_trait_files", "eigenvectors.tsv")

test_that("combine.trait.files checks input types", {

})

test_that("combine.trait.files conducts a simple merge of phenotypes", {
  res <- combine.trait.file(
    c(phenodata1, phenodata2),
    c(phenoconfig1, phenoconfig2),
    c("NA", "NA"),
    eigenvectors,
    TRUE,
    TRUE,
    FALSE,
    analysisconfig,
    "a1",
    0,
    c("NA", "NA"),
    TRUE
  )
})

test_that("combine.trait.files conducts a simple merge of covariates", {

})

test_that("combine.trait.files respects user suppression of batch variable", {

})

test_that("combine.trait.files finds and removes duplicates introduced by merging", {

})
