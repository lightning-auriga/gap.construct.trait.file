subject.ids <- letters[1:20]
phenotypes <- c(rep(1, 5), rep(0, 15))
pc1 <- rep(1:5, 4)

phenotype.df <- data.frame(
  FID = subject.ids,
  IID = subject.ids,
  phenoname = phenotypes
)

covariate.df <- data.frame(
  FID = subject.ids,
  IID = subject.ids,
  PC1 = pc1
)

test_that("match.controls performs basic control matching", {
  pheno.filename <- tempfile("match_controls_phenofile", fileext = ".tsv")
  covar.filename <- tempfile("match_controls_covarfile", fileext = ".tsv")
  output.filename <- tempfile("match_controls_outputfile", fileext = ".tsv")
  write.table(phenotype.df, pheno.filename,
    row.names = FALSE, col.names = TRUE,
    quote = FALSE, sep = "\t"
  )
  write.table(covariate.df, covar.filename,
    row.names = FALSE, col.names = TRUE,
    quote = FALSE, sep = "\t"
  )
  match.controls(
    pheno.filename,
    covar.filename,
    output.filename,
    1,
    2
  )
  observed <- read.table(output.filename,
    header = TRUE, stringsAsFactors = FALSE,
    sep = "\t", comment.char = "", quote = ""
  )
  print(observed)
  ## expect: each case should have exactly two of their three paired controls included,
  ## and a single remaining paired control set to NA.
  ## however, the exact identity of the residual subject is unclear.
  expect_equal(length(which(observed[, 3] == 1 & !is.na(observed[, 3]))), 5)
  expect_equal(length(which(is.na(observed[, 3]))), 5)
  for (i in seq_len(5)) {
    expect_equal(length(which(!is.na(observed[seq(i + 5, 20, 5), 3]))), 2)
  }
})

test_that("match.controls understands when invalid covariates are requested", {
  pheno.filename <- tempfile("match_controls_phenofile", fileext = ".tsv")
  covar.filename <- tempfile("match_controls_covarfile", fileext = ".tsv")
  output.filename <- tempfile("match_controls_outputfile", fileext = ".tsv")
  write.table(phenotype.df, pheno.filename,
    row.names = FALSE, col.names = TRUE,
    quote = FALSE, sep = "\t"
  )
  write.table(covariate.df, covar.filename,
    row.names = FALSE, col.names = TRUE,
    quote = FALSE, sep = "\t"
  )
  expect_error(match.controls(
    pheno.filename,
    covar.filename,
    output.filename,
    2,
    2
  ))
})
