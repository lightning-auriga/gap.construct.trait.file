#' Append requested phenotype information to a partially
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
#' covariate data frame if desired. note that certain data types
#' as flagged in the phenotype configuration are explicitly
#' forbidden from emission as phenotypes due to the lack of
#' intentional downstream support. this may be updated in future
#' patches, but if current functionality is desired, consider
#' using construct.covariate.output instead
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
#' @param analysis.config list; loaded analysis-specific
#' configuration data. used to collect requested covariate
#' names and number of principal components. see documentation
#' of gwas-analysis-pipeline for more information
#' @param analysis.name character vector; unique name of analysis
#' currently being processed. this should correspond to one of
#' the block names under the "analyses:" tag in the analysis
#' configuration
#' @return input data frame with requested phenotype column appended
construct.phenotype.output <- function(output.df,
                                       phenotype.data,
                                       phenotype.config,
                                       analysis.config,
                                       analysis.name) {
  stopifnot(is.data.frame(output.df))
  stopifnot(is.data.frame(phenotype.data))
  stopifnot(is.list(phenotype.config))
  stopifnot(is.list(analysis.config))
  stopifnot(is.vector(analysis.name, mode = "character"), length(analysis.name) == 1)
  stopifnot(nrow(output.df) == nrow(phenotype.data))
  phenotype.name <- analysis.config$analyses[[analysis.name]]$phenotype
  ## add the phenotype to the variable queries
  added.df <- gap.construct.trait.file:::restructure.variable(
    phenotype.data,
    phenotype.config,
    phenotype.name,
    0,
    TRUE,
    TRUE
  )
  output.df <- cbind(output.df, added.df)
  colnames(output.df)[-1] <- colnames(added.df)
  output.df
}
