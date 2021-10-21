#' Apply formatting to data frame output in preparation
#' for use with plink2
#'
#' @description given a prepared model matrix, this function
#' will add a leading (all "0") FID column, and drop subjects
#' with duplicated IIDs, likely due to strange discrepancies
#' with ID mapping
#'
#' @details this function is basically a stub, as the upstream
#' processing is largely performed with the idea that downstream
#' tools will accept plink-style format. other formatters for
#' other software tools (e.g. snptest) may be more extensive
#'
#' @param output.df data frame; partially formatted model matrix
#' @return reformatted input with plink-specific modifications
format.for.plink <- function(output.df) {
  output.df <- cbind(
    rep("0", nrow(output.df)),
    output.df
  )
  colnames(output.df)[1] <- "FID"
  ## drop subjects with duplicate IDs. plink strongly enforces this,
  ## and indeed this is probably indicative of a mapping failure
  output.df <- output.df[!duplicated(output.df[, 2]) & !duplicated(output.df[, 2], fromLast = TRUE), ]
  output.df
}
