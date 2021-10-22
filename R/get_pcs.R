#' Extract PCs from a smartpca-format eigenvector file
#'
#' @description this simple function just inspects an
#' eigenvector data frame for valid sets of entries and
#' emits the results for binding into a model matrix
#'
#' @details this may not really justify its own function,
#' but the mapping is just finicky enough to suggest
#' it may benefit from specific testing. note that subjects
#' in query may be absent from PC data, and will simply
#' return NA for all PCs
#'
#' @param df data frame; input eigenvector data in smartpca
#' style format, subject IDs in first column and set as rownames
#' @param n.pcs integer; number of requested principal components
#' for extraction
#' @param ids character vector; subject IDs for which PC data
#' are requested, in desired output order
#' @return requested PC data, in correct order, as a data frame
get.pcs <- function(df, n.pcs, ids) {
  stopifnot(is.data.frame(df))
  stopifnot(is.vector(ids, mode = "character"))
  stopifnot(is.integer(n.pcs), length(n.pcs) == 1)
  stopifnot(n.pcs > 0)
  stopifnot(n.pcs <= ncol(df) - 2)
  res <- data.frame(df[ids, seq(2, n.pcs + 1)])
  colnames(res) <- colnames(df)[seq(2, n.pcs + 1)]
  rownames(res) <- NULL
  res
}
