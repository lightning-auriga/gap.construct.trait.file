% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restructure_variable.R
\name{restructure.variable}
\alias{restructure.variable}
\title{Reformat categorical variables to match configuration}
\usage{
restructure.variable(
  phenotype.data,
  config,
  variable.name,
  collapse.limit,
  error.on.unsupported.models,
  disable.binarization = FALSE
)
}
\arguments{
\item{phenotype.data}{data frame; loaded phenotype information
with informative column headers}

\item{config}{list; loaded configuration data for phenotype
dataset, including most importantly factor level configuration
for categorical and ordinal variables}

\item{variable.name}{character vector; name of requested
variable as it appears in phenotype dataset header and
configuration list}

\item{collapse.limit}{integer; number of observations of a
category level above which the category is allowed to be a
distinct binary predictor in binarized categorical covariates.
many downstream models will fail if categories with extremely
low observation counts (e.g. 10 or fewer, or something like that)
are included in the regression model. levels for any categorical
predictor will be collapsed into a single combined predictor
that serves as a catchall for low count categories. note that
there is no guarantee that the combined variable will have
more observations than this limit, so it still may not converge.
in a future patch, this behavior may be modified to set subjects
falling into this catchall to NA}

\item{error.on.unsupported.models}{logical; whether this
function should abort with error if the requested
variable is categorical. this is designed to be set TRUE
when the variable is a phenotype and FALSE when the variable
is a covariate, as the binarization hack only works directly
for covariate data}

\item{disable.binarization}{logical; whether to return
N level categorical and ordinal variables split into N-1
binary variables. defaults to FALSE. this should only be
set to TRUE if the downstream application has some method
for dealing with categorical data that doesn't involve
treating them as continuous values}
}
\value{
data frame; formatted variable(s) corresponding
to requested variable from input phenotype data, in same
order as input phenotype data
}
\description{
collects a variable from a phenotype dataset.
if the variable's configured type involves factor levels,
those levels are split out and handled in various contextually
appropriate ways
}
\details{
categorical variables require special handling
in this tool, as the storage format in the upstream
process.phenotypes package does not emulate the format required
by downstream analysis tools. as currently targeted downstream
tools only support categorical data as covariates, this function
catches situations where categorical (N>2) variables are requested
as phenotypes. it then remaps categorical data onto the factor
names configured in the phenotype config yaml from process.phenotypes,
and then splits the variable out into N-1 binary dummy variables
for use with tools that don't support categorical covariates.
in cases where insufficiently many observations are present for
certain factor levels, those levels are collapsed into a single
catchall indicator variable to attempt to rescue the model
}
