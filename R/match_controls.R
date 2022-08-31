#' @title
#' Perform ad hoc control matching for a phenotype.
#'
#' @description
#' Given a pregenerated set of cases and controls, and their associated
#' principal components, perform control matching for cases. Attempts
#' to reach a fixed ratio of controls per included case.
#'
#' @param phenotype.file Character name of input phenotype dataset.
#' @param covariate.file Character name of input covariate dataset.
#' @param output.file Character name of output phenotype dataset.
#' @param n.pcs.match Integer or numeric number of principal components
#' to use in matching model.
#' @param n.controls.per.case Integer or numeric number of controls to
#' include in the output dataset per case included.
#' @return NULL
#' @export
match.controls <- function(phenotype.file,
                           covariate.file,
                           output.file,
                           n.pcs.match,
                           n.controls.per.case) {
  stopifnot(
    length(phenotype.file) == 1,
    length(covariate.file) == 1,
    length(output.file) == 1,
    length(n.pcs.match) == 1,
    length(n.controls.per.case) == 1
  )
  stopifnot(
    is.character(phenotype.file),
    is.character(covariate.file),
    is.character(output.file)
  )
  stopifnot(
    file.exists(phenotype.file),
    file.exists(covariate.file)
  )
  stopifnot(is.numeric(n.pcs.match) || is.integer(n.pcs.match))
  stopifnot(is.numeric(n.controls.per.case) || is.integer(n.controls.per.case))
  stopifnot(
    n.pcs.match >= 1,
    n.controls.per.case >= 1
  )

  phenotype.data <- read.table(phenotype.file,
    header = TRUE, stringsAsFactors = FALSE,
    sep = "\t", comment.char = "", quote = ""
  )
  covariate.data <- read.table(covariate.file,
    header = TRUE, stringsAsFactors = FALSE,
    sep = "\t", comment.char = "", quote = ""
  )
  stopifnot(ncol(phenotype.data) == 3)
  selected.pcs <- paste("PC", seq_len(n.pcs.match), sep = "")
  stopifnot(length(which(selected.pcs %in% colnames(covariate.data))) == n.pcs.match)
  data <- data.frame(
    phenotype.data[, 3],
    covariate.data[, selected.pcs]
  )
  rownames(data) <- phenotype.data[, 2]
  colnames(data) <- c("outcome", selected.pcs)

  n.cases <- length(which(!is.na(data[, 1]) & data[, 1] == 1))
  n.controls <- length(which(!is.na(data[, 1]) & data[, 1] == 0))
  effective.max.controls <- floor(min(n.controls / n.cases, n.controls.per.case))
  stopifnot(effective.max.controls >= 1)
  form <- as.formula(paste("outcome", paste(selected.pcs, collapse = " + "), sep = " ~ "))
  matched.on <- optmatch::match_on(form, data = data)
  result <- optmatch::fullmatch(matched.on,
    min.controls = 1,
    max.controls = effective.max.controls,
    data = data
  )
  phenotype.data[is.na(result), 3] <- NA
  write.table(phenotype.data, output.file,
    row.names = FALSE,
    col.names = TRUE, quote = FALSE, sep = "\t"
  )
}
