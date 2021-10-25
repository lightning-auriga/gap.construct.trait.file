#' Append requested covariate information to a partially
#' constructed model matrix
#'
#' @description given a partially constructed output data frame
#' with selected subject IDs in the first column, use analysis
#' and phenotype configuration information to construct a
#' model matrix
#'
#' @details this should aggregate information into a model matrix
#' regardless of whatever else may be present in the dataset
#' originally, so it can be used to build on top of an existing
#' phenotype data frame if desired. eigenvectors are provided
#' as input but only appended if a valid number of principal
#' components have been requested in analysis configuration.
#' categorical covariates are automatically converted into
#' binary dummies with category collapse as appropriate
#'
#' @param output.df data frame; partially constructed model
#' matrix with, minimally, minimally, a single first column
#' containing subject IDs
#' @param phenotype.data data frame; loaded phenotype
#' data with informative headers. needs minimally a subject ID
#' column (determined based on configuration) and columns
#' corresponding to requested covariates
#' @param phenotype.config list; loaded phenotype
#' configuration yaml. needs minimally a variable flagged
#' as subject ID (see documentation of process.phenotypes)
#' @param eigenvectors data frame; loaded eigenvector data.
#' first column is subject ID; next N-2 columns are eigenvectors
#' starting with PC1; last column is a categorical variable
#' flagging populations for use with PC projection. by convention,
#' last column entries should be either "unrel" or "rel"
#' (though the "rel" category is allowed to be absent)
#' @param analysis.config list; loaded analysis-specific
#' configuration data. used to collect requested covariate
#' names and number of principal components. see documentation
#' of gwas-analysis-pipeline for more information
#' @param analysis.name character vector; unique name of analysis
#' currently being processed. this should correspond to one of
#' the block names under the "analyses:" tag in the analysis
#' configuration
#' @param collapse.limit integer; number of observations of a
#' category level above which the category is allowed to be a
#' distinct binary predictor in binarized categorical covariates.
#' many downstream models will fail if categories with extremely
#' low observation counts (e.g. 10 or fewer, or something like that)
#' are included in the regression model. levels for any categorical
#' predictor will be collapsed into a single combined predictor
#' that serves as a catchall for low count categories. note that
#' there is no guarantee that the combined variable will have
#' more observations than this limit, so it still may not converge.
#' in a future patch, this behavior may be modified to set subjects
#' falling into this catchall to NA
#' @param disable.binarization logical; whether to return
#' N level categorical and ordinal variables split into N-1
#' binary variables. defaults to FALSE. this should only be
#' set to TRUE if the downstream application has some method
#' for dealing with categorical data that doesn't involve
#' treating them as continuous values
#' @return input data frame with requested covariate columns appended
construct.covariate.output <- function(output.df,
                                       phenotype.data,
                                       phenotype.config,
                                       eigenvectors,
                                       analysis.config,
                                       analysis.name,
                                       collapse.limit,
                                       disable.binarization = FALSE) {
  ## input sanity checking
  stopifnot(is.data.frame(output.df))
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(is.list(phenotype.config))
  stopifnot(is.data.frame(eigenvectors))
  stopifnot(is.list(analysis.config))
  stopifnot(is.vector(analysis.name, mode = "character"), length(analysis.name) == 1)
  stopifnot(is.integer(collapse.limit), length(collapse.limit) == 1)
  stopifnot(is.logical(disable.binarization), length(disable.binarization) == 1)
  ## add the covariates to the variable queries
  for (covariate.name in analysis.config$analyses[[analysis.name]]$covariates) {
    added.df <- gap.construct.trait.file:::restructure.variable(
      phenotype.data,
      phenotype.config,
      covariate.name,
      collapse.limit,
      FALSE,
      disable.binarization
    )
    output.df <- cbind(output.df, added.df)
    colnames(output.df)[seq(ncol(output.df) - ncol(added.df) + 1, ncol(output.df))] <- colnames(added.df)
  }
  if (!is.null(analysis.config$analyses[[analysis.name]]$pcs)) {
    n.pcs <- as.integer(analysis.config$analyses[[analysis.name]]$pcs)
    stopifnot(n.pcs >= 0, n.pcs <= ncol(eigenvectors) - 2)
    if (n.pcs > 0) {
      added.df <- gap.construct.trait.file:::get.pcs(eigenvectors, n.pcs, output.df[, 1])
      output.df <- cbind(output.df, added.df)
      colnames(output.df)[seq(ncol(output.df) - ncol(added.df) + 1, ncol(output.df))] <- paste("PC", 1:n.pcs, sep = "")
    }
  }
  output.df
}
