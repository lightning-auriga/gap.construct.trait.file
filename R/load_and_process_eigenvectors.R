#' Read smartpca-format eigenvectors from file and apply
#' transformations for downstream use
#'
#' @description this function expects raw output from smartpca,
#' and applies sufficient transformations to prevent plink2
#' from overly complaining about their distribution
#'
#' @details the transformation requested by plink2 is quite
#' standard for regression models, so for the moment it is
#' unconditionally enabled. however, it is quite possible some
#' people may eventually object to this happening for some tools,
#' so future patches may introduce the ability to disable
#' the transformation on user request
#'
#' @param eigenvectors character vector; name of input
#' smartpca eigenvector file
#' @return loaded and transformed smartpca eigenvector data
load.and.process.eigenvectors <- function(eigenvectors) {
  ## input sanity checking
  stopifnot(is.vector(eigenvectors, mode = "character"), length(eigenvectors) == 1)
  eigenvectors <- read.table(eigenvectors,
    header = FALSE,
    skip = 1,
    comment.char = "",
    stringsAsFactors = FALSE
  )
  stopifnot(ncol(eigenvectors) > 2)
  ## first column must be subject IDs
  rownames(eigenvectors) <- eigenvectors[, 1]
  ## enforce that the last column actually contains rel/unrel calls
  stopifnot(length(which(eigenvectors[, ncol(eigenvectors)] %in% c("rel", "unrel"))) == nrow(eigenvectors))
  stopifnot(length(which(eigenvectors[, ncol(eigenvectors)] == "unrel")) > 0)
  ## apply plink-requested transformation of data to mean 0 and sd 1
  ## note that for consistency with PC projection, this transformation is performed
  ## using the mean and standard deviation of the unrelated subset of the data.
  ## eventually, plink will probably find reason to complain about this too.
  for (i in seq(2, ncol(eigenvectors) - 1)) {
    eigenvectors[, i] <- (eigenvectors[, i] - mean(eigenvectors[eigenvectors[, ncol(eigenvectors)] != "rel", i])) /
      sd(eigenvectors[eigenvectors[, ncol(eigenvectors)] != "rel", i])
  }
  eigenvectors
}
