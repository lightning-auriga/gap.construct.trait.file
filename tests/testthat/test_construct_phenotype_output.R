output.df <- data.frame(IID = c("A", "B", "C", "D", "E", "F", "G"))
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

analysis.config <- list(analyses = list(
  c1 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c1-test",
    "phenotype" = "TV2",
    "covariates" = c("TV3_derived"),
    pcs = 10
  ),
  c2 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c2-test",
    "phenotype" = "TV3_derived",
    "covariates" = c("TV1", "TV2"),
    pcs = 5
  ),
  c3 = list(
    "genotype-dataset" = "gwas-test",
    "phenotype-dataset" = "c3-test",
    "phenotype" = "TV4",
    "covariates" = c("TV1", "TV2"),
    pcs = 4
  )
))
analysis.name <- "c2"

test_that("construct.phenotype.output checks input types", {
  expect_error(construct.phenotype.output("x", pheno.df, pheno.config, analysis.config, analysis.name))
  expect_error(construct.phenotype.output(output.df, "x", pheno.config, analysis.config, analysis.name))
  expect_error(construct.phenotype.output(output.df, pheno.df, "x", analysis.config, analysis.name))
  expect_error(construct.phenotype.output(output.df, pheno.df, pheno.config, "x", analysis.name))
  expect_error(construct.phenotype.output(output.df, pheno.df, pheno.config, analysis.config, list()))
  expect_error(construct.phenotype.output(output.df[1:5, ], pheno.df, pheno.config, analysis.config, analysis.name))
})

test_that("construct.phenotype.output adds a single phenotype", {
  expected.df <- output.df
  expected.df[, "TV3_derived"] <- pheno.df[, "TV3_derived"]
  res <- construct.phenotype.output(output.df, pheno.df, pheno.config, analysis.config, analysis.name)
  expect_identical(
    res,
    expected.df
  )
})

test_that("construct.phenotype.output detects invalid phenotype type", {
  expect_error(construct.phenotype.output(output.df, pheno.df, pheno.config, analysis.config, "c1"))
})

test_that("construct.phenotype.output detects absent phenotype", {
  expect_error(construct.phenotype.output(output.df, pheno.df, pheno.config, analysis.config, "c3"))
})
