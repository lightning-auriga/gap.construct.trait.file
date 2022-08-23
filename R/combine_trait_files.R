#' Build a gwas-ready trait file from phenotype configuration,
#' potentially combining across multiple datasets
#'
#' @description Takes in assorted phenotype configuration information
#' and run parameters and emits the requested formatted output as
#' needed for downstream tools, e.g. plink2 --glm, regenie, etc. Combines
#' across multiple datasets when they are each specified and contain
#' compatible phenotype and covariate names.
#'
#' @details This package is designed as a downstream supplement
#' to process.phenotypes, in that it receives output from that
#' tool and same-format configuration information to create
#' model matrices. As initially configured, this function receives
#' information managed by Snakemake. Snakemake should guarantee
#' some characteristics of the data (e.g. files exist when specified),
#' but for sanity reasons all checks will be repeated. In particular,
#' it is possible that the Snakemake object that's originally providing
#' this information may not have the requested entries, in which case
#' some things could be unexpectedly NULL.
#'
#' This is a utility function to combine data across multiple datasets,
#' provided that the datasets have identically named phenotypes
#' and covariates. Optionally, an additional batch variable
#' is added to flag the contributing datasets as possible
#' sources of batch effects.
#'
#' @param phenotype.file character vector; path to and name of
#' file containing phenotype information. file should be tab-delimited,
#' with cells quoted in "..." as needed. this is expected to be the
#' processed output of process.phenotypes::create.phenotype.report.
#' when combining multiple datasets with compatible variable names,
#' this should be a vector of the same length as the other multiple
#' entry parameters
#' @param phenotype.config character vector; path to and name of
#' file containing phenotype configuration data. file should be
#' yaml format. this file is expected to be the corresponding
#' configuration that led to the generation of the phenotype
#' matrix, and the yaml tags expected and recognized in this file
#' are documented with that package. when combining multiple
#' datasets with compatible variable names, this should be a
#' vector of the same length as the other multiple entry parameters
#' @param phenotype.shared.models character vector; path to and
#' name of file containing shared model configuration data. file should
#' be yaml format. this file is expected to be the corresponding
#' configuration that led to the generation of the phenotype matrix,
#' and the yaml tags expected and recognized in this file are documented
#' with that package. note that this file is not required; NA
#' is permitted, in which case the phenotype config file itself
#' must explicitly specify "type:" for all variables, and never
#' use the "shared_model:" tag. when combining multiple datasets
#' with compatible variable names, this should be a vector of the
#' same length as the other multiple entry parameters
#' @param eigenvectors character vector; path to and name
#' of file containing subject-level eigenvector data. file should
#' be the output format of smartpca, with a single, strange header
#' containing eigenvalues, one row per subject, subject ID in first
#' column, last column containing population labels as used for
#' PCA projection
#' @param plink.format logical; whether output format should
#' be plink1-style: plaintext, FID and IID in first columns,
#' FID set to 0 for all subjects (may be modified in future patches),
#' header present, one row per subject, tab delimited. currently
#' this is the only supported format, and the function will error
#' out if this is not TRUE
#' @param phenotype.output logical, whether output file should
#' contain phenotype information. some downstream applications
#' want model matrices presented as phenotype and covariate matrices
#' separately, while some want them combined
#' @param covariate.output logical, whether output file should
#' contain covariate information. some downstream applications
#' want model matrices presented as phenotype and covariate matrices
#' separately, while some want them combined
#' @param analysis.config character vector; path to and name of
#' configuration file for target downstream analysis. file should
#' be yaml format. recognized configuration options are specified
#' in the documentation for gwas-analysis-pipeline. even when
#' combining multiple datasets with compatible variable names,
#' this should still only be a single file
#' @param analysis.name character vector; unique name of analysis
#' currently being processed. this should correspond to one of
#' the block names under the "analyses:" tag in the analysis
#' configuration file. even when combining multiple datasets
#' with compatible variable names, this should still
#' only be a single file
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
#' @param id.linker character vector; path to and name of file
#' specifying subject ID linking. format should be: plaintext,
#' two columns, with header, tab delimited, first column
#' contains exact ID present in phenotype file, second column
#' contains corresponding ID that should be reported in output.
#' this linker can be used to map subjects into IDs that
#' are present in corresponding genotype data. IDs that
#' are duplicated after mapping will *all* be removed, to
#' allow compatibility with downstream tools that object
#' to duplicate IDs' presence. note that this argument
#' is optional, and NA here will be handled correctly
#' @param suppress.merge.batch logical; whether to prevent
#' batch variable injection when combining multiple datasets.
#' note that when this function is called on a single dataset only,
#' this merge batch variable is unconditionally suppressed
#' @param apply.transformations logical; whether configured
#' phenotype transformations should be applied before returning.
#' if this function is called directly, transformations are probably
#' desired; however, if this is called within the combination function
#' that merges datasets, the transformations should likely be done
#' on the datasets post-merge
#' @return data frame; formatted output corresponding to
#' the specified run parameters. exact format depends on the
#' specified format flags. with initial configuration, this
#' will be plink format with headers. output can be emitted
#' to file with write.table(..., row.names=FALSE, col.names=TRUE, sep="\\t").
#' note that this output will potentially contain an additional
#' set of batch binary variables indicating the source of
#' the input data for dataset merge, if that batch variable
#' was enabled and the output requested covariates
#' @export combine.trait.files
combine.trait.files <- function(phenotype.file,
                                phenotype.config,
                                phenotype.shared.models,
                                eigenvectors,
                                plink.format,
                                phenotype.output,
                                covariate.output,
                                analysis.config,
                                analysis.name,
                                collapse.limit,
                                id.linker,
                                suppress.merge.batch,
                                apply.transformations = TRUE) {
  ## input sanity checks
  stopifnot(length(phenotype.config) == length(phenotype.file))
  stopifnot(length(phenotype.config) == length(phenotype.shared.models))
  stopifnot(length(phenotype.config) == length(id.linker))
  stopifnot(is.logical(plink.format), length(plink.format) == 1)
  stopifnot(is.logical(covariate.output), length(covariate.output) == 1)
  stopifnot(is.logical(suppress.merge.batch), length(suppress.merge.batch) == 1)
  ## if the output style is covariates,
  ## and the user hasn't suppressed this behavior,
  ## and there are at least two datasets being combined
  add.batch <- covariate.output & !suppress.merge.batch & length(phenotype.config) > 1
  ## iterate across all possible provided files
  res <- data.frame()
  for (i in seq_len(length(phenotype.config))) {
    res.partial <- construct.trait.file(
      phenotype.file[i],
      phenotype.config[i],
      phenotype.shared.models[i],
      eigenvectors,
      plink.format,
      phenotype.output,
      covariate.output,
      analysis.config,
      analysis.name,
      collapse.limit,
      id.linker[i],
      TRUE,
      FALSE
    )
    if (add.batch) {
      res.partial[, ncol(res.partial) + 1] <- factor(i, levels = seq_len(length(phenotype.config)))
      colnames(res.partial)[ncol(res.partial)] <- "gap.merge.batch"
    }
    ## combine this information back into the aggregated dataset
    if (i == 1) {
      res <- res.partial
    } else {
      ## detect merge problems
      if (!identical(colnames(res), colnames(res.partial))) {
        stop(paste("merging of multiple datasets failed. this is most likely due",
          " to different patterns of categorical level merging in smaller ",
          "and bigger datasets. please consider changing the value of ",
          "collapse.limit to a smaller number; otherwise, conduct manual ",
          "merge, or request a patch to this software",
          sep = ""
        ))
      }
      res <- rbind(res, res.partial)
    }
  }
  ## apply transformations
  if (apply.transformations & !covariate.output) {
    ## this is harder here, because we don't necessarily have all the data available
    analysis.config <- yaml::read_yaml(analysis.config)
    strat.vars <- list()
    if (!is.null(analysis.config$analyses[[analysis.name]]$transformation$stratification)) {
      build.df <- NULL
      for (i in seq_len(length(phenotype.file))) {
        phenotype.data <- read.table(phenotype.file[i],
          header = TRUE,
          stringsAsFactors = FALSE, sep = "\t",
          quote = "\"", comment.char = ""
        )
        current.pheno.config <- yaml::read_yaml(phenotype.config[i])
        subject.id.varname <- colnames(phenotype.data)[seq_len(length(current.pheno.config$variables))][unname(sapply(
          current.pheno.config$variables,
          function(i) {
            ifelse(!is.null(i[["subject_id"]]),
              i[["subject_id"]],
              FALSE
            )
          }
        ))]
        varnames <- c(
          subject.id.varname,
          analysis.config$analyses[[analysis.name]]$transformation$stratification
        )
        stopifnot(length(which(varnames %in% colnames(phenotype.data))) ==
          length(varnames))
        phenotype.data <- phenotype.data[, varnames]
        phenotype.data[, 1] <- remap.ids(
          phenotype.data[, 1],
          id.linker[i]
        )
        ## harmonize column names so rbind won't complain
        colnames(phenotype.data) <- paste("var", seq_len(ncol(phenotype.data)), sep = "")
        if (is.null(build.df)) {
          build.df <- data.frame(phenotype.data)
        } else {
          build.df <- rbind(
            build.df,
            data.frame(phenotype.data)
          )
        }
      }
      my.colnames <- colnames(build.df)
      build.df <- build.df[!is.na(build.df[, 1]), ]
      build.df <- build.df[!duplicated(build.df[, 1]), ]
      rownames(build.df) <- build.df[, 1]
      build.df <- build.df[res[, ifelse(plink.format, 2, 1)], ]
      strat.vars <- as.list(data.frame(build.df[, -1]))
      names(strat.vars) <- my.colnames[-1]
    }
    res[, ifelse(plink.format, 3, 2)] <- transform.variable(
      res[, ifelse(plink.format, 3, 2)],
      analysis.config$analyses[[analysis.name]]$transformation$type,
      strat.vars
    )
  }

  ## deal with factor variable expansion now that the merge is complete
  expanded.factors <- data.frame(res[, 1])
  colnames(expanded.factors) <- colnames(res)[1]
  for (i in seq(2, ncol(res))) {
    if (is.factor(res[, i])) {
      ## create a placeholder factor level mapping
      factor.levels <- list()
      for (lvl in levels(res[, i])) {
        factor.levels[[lvl]] <- list("name" = lvl)
      }
      expanded.factors <- cbind(
        expanded.factors,
        construct.binary.dummies(
          res[, i],
          colnames(res)[i],
          factor.levels,
          collapse.limit
        )
      )
    } else {
      expanded.factors <- cbind(expanded.factors, res[, i])
      colnames(expanded.factors)[i] <- colnames(res)[i]
    }
  }
  res <- expanded.factors
  ## deal with the possibility that duplicates have been introduced by merging
  if (plink.format) {
    res <- res[!duplicated(res[, 2]) & !duplicated(res[, 2], fromLast = TRUE), ]
  }
  ## return the combined data
  rownames(res) <- NULL
  res
}
