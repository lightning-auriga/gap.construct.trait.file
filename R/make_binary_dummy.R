#' Create a binary dummy variable for a set of categorical variable levels
#'
#' @description flags all entries of a categorical variable that are
#' members of potentially a set of target categories as 1. all other
#' levels are set to 0. this is assumed to be used in conjunction
#' with other binary variables to flag all N-1 levels of a factor
#'
#' @details many downstream applications do not correctly handle
#' categorical covariates, or ultimately implement a model that
#' looks like this but without equivalently fine-grained control.
#' specifically, this function can be used to collapse levels,
#' notably when the constituent factor levels do not have
#' sufficiently many observations to provide good convergence
#' behavior.
#'
#' note that the format of the inputs is not strongly restricted
#' here: input does not need to be a factor or ordered, comparison
#' levels can be any type, etc. this can potentially be used
#' very generally to split out variables, or can just be provided
#' character vectors for all values if preferred
#'
#' @param var vector; input categorical data variable,
#' though potentially stored as another format
#' @param cmp vector; set of values to match against
#' input data
#' @return numeric vector; same length as input data,
#' all values 1 where input matched comparison levels, 0 otherwise
make.binary.dummy <- function(var,
                              cmp) {
  res <- rep(0, length(var))
  res[var %in% cmp] <- 1
  res
}
