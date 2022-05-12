#' Build a gwas-ready trait file from phenotype configuration
#'
#' @description Takes in assorted phenotype configuration information
#' and run parameters and emits the requested formatted output as
#' needed for downstream tools, e.g. plink2 --glm, regenie, etc.
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
#' @param phenotype.file character vector; path to and name of
#' file containing phenotype information. file should be tab-delimited,
#' with cells quoted in "..." as needed. this is expected to be the
#' processed output of process.phenotypes::create.phenotype.report
#' @param phenotype.config character vector; path to and name of
#' file containing phenotype configuration data. file should be
#' yaml format. this file is expected to be the corresponding
#' configuration that led to the generation of the phenotype
#' matrix, and the yaml tags expected and recognized in this file
#' are documented with that package
#' @param phenotype.shared.models character vector; path to and
#' name of file containing shared model configuration data. file should
#' be yaml format. this file is expected to be the corresponding
#' configuration that led to the generation of the phenotype matrix,
#' and the yaml tags expected and recognized in this file are documented
#' with that package. note that this file is not required; NA
#' is permitted, in which case the phenotype config file itself
#' must explicitly specify "type:" for all variables, and never
#' use the "shared_model:" tag
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
#' in the documentation for gwas-analysis-pipeline
#' @param analysis.name character vector; unique name of analysis
#' currently being processed. this should correspond to one of
#' the block names under the "analyses:" tag in the analysis
#' configuration file
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
#' @param disable.binarization logical; whether to return
#' N level categorical and ordinal variables split into N-1
#' binary variables. defaults to FALSE. this should only be
#' set to TRUE if the downstream application has some method
#' for dealing with categorical data that doesn't involve
#' treating them as continuous values
#' @return data frame; formatted output corresponding to
#' the specified run parameters. exact format depends on the
#' specified format flags. with initial configuration, this
#' will be plink format with headers. output can be emitted
#' to file with write.table(..., row.names=FALSE, col.names=TRUE, sep="\\t")
#' @export construct.trait.file
construct.trait.file <- function(phenotype.file,
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
                                 disable.binarization = FALSE) {
  ## input sanity checks, and load yaml configuration data for shared models, if specified
  ## this is combined for complexity reasons
  stopifnot(is.vector(phenotype.file, mode = "character"), length(phenotype.file) == 1)
  stopifnot(file.exists(phenotype.file))
  stopifnot(is.vector(phenotype.config, mode = "character"), length(phenotype.config) == 1)
  stopifnot(file.exists(phenotype.config))
  if (!isTRUE(is.na(phenotype.shared.models))) {
    stopifnot(
      is.vector(phenotype.shared.models, mode = "character"),
      length(phenotype.shared.models) == 1
    )
    stopifnot(file.exists(phenotype.shared.models))
    ## merge this information into the phenotype data
    phenotype.config <- process.phenotypes:::load.configuration(
      phenotype.config,
      phenotype.shared.models
    )
  } else {
    ## load yaml configuration data for the phenotype data
    phenotype.config <- yaml::read_yaml(phenotype.config)
  }
  stopifnot(is.vector(eigenvectors, mode = "character"), length(eigenvectors) == 1)
  stopifnot(is.logical(plink.format), length(plink.format) == 1)
  stopifnot(is.logical(phenotype.output), length(phenotype.output) == 1)
  stopifnot(is.logical(covariate.output), length(covariate.output) == 1)
  stopifnot(is.vector(analysis.config, mode = "character"), length(analysis.config) == 1)
  stopifnot(file.exists(analysis.config))
  stopifnot(is.vector(analysis.name, mode = "character"), length(analysis.name) == 1)
  stopifnot(is.integer(collapse.limit), length(collapse.limit) == 1)
  if (!isTRUE(is.na(id.linker))) {
    stopifnot(is.vector(id.linker, mode = "character"), length(id.linker) == 1)
    stopifnot(file.exists(id.linker))
  }
  stopifnot(is.logical(disable.binarization), length(disable.binarization) == 1)
  ## load yaml configuration data for the analysis
  analysis.config <- yaml::read_yaml(analysis.config)
  ## confirm that the requested analysis is present in the config
  stopifnot(analysis.name %in% names(analysis.config$analyses))
  ## load actual phenotype data
  phenotype.data <- read.table(phenotype.file,
    header = TRUE,
    stringsAsFactors = FALSE, sep = "\t",
    quote = "\"", comment.char = ""
  )
  sex.specific <- analysis.config$analyses[[analysis.name]][["sex-specific"]]
  if (!is.null(sex.specific)) {
    if (sex.specific != "all") {
      sex.varname <- analysis.config$analyses[[analysis.name]][["sex-varname"]]
      stopifnot("sex variable name required with sex-specific analysis" = !is.null(sex.varname))
      stopifnot(
        "sex variable must be present in phenotype data" =
          length(which(colnames(phenotype.data) == sex.varname)) == 1
      )
      phenotype.data <- phenotype.data[as.character(phenotype.data[, sex.varname]) == as.character(sex.specific) &
        !is.na(phenotype.data[, sex.varname]), ]
      stopifnot("sex-specific analysis cannot remove all subjects" = nrow(phenotype.data) > 0)
    }
  }
  ## read in eigenvectors and apply simple transformations
  ## to prevent plink from complaining about distributions and variance
  eigenvectors <- load.and.process.eigenvectors(eigenvectors)

  ## locate and add the subject ID column
  subject.id.index <- unlist(sapply(seq_len(length(phenotype.config$variables)), function(i) {
    if (!is.null(phenotype.config$variables[[i]]$subject_id)) {
      i
    }
  }))
  stopifnot(length(subject.id.index) == 1, phenotype.config$variables[[subject.id.index]]$subject_id)

  ## if id.linker is NA, this will just return the original ID list unchanged
  phenotype.data[, subject.id.index] <- remap.ids(
    phenotype.data[, subject.id.index],
    id.linker
  )
  ## remove instances where IDs fail to link
  phenotype.data <- phenotype.data[!is.na(phenotype.data[, subject.id.index]), ]

  output.df <- data.frame(IID = phenotype.data[, subject.id.index], stringsAsFactors = FALSE)

  if (phenotype.output) {
    output.df <- construct.phenotype.output(
      output.df,
      phenotype.data,
      phenotype.config,
      analysis.config,
      analysis.name
    )
  }

  if (covariate.output) {
    output.df <- construct.covariate.output(
      output.df,
      phenotype.data,
      phenotype.config,
      eigenvectors,
      analysis.config,
      analysis.name,
      collapse.limit,
      disable.binarization
    )
  }

  ## report this information to file
  if (plink.format) {
    output.df <- format.for.plink(output.df)
  } else {
    stop("only plink output format is currently supported, and it wasn't requested :(")
  }
  output.df
}
