#' using an N level factor variable, construct
#' an N-1 data frame of binary dummy variables
#'
#' @description this function can be used to
#' expand factor (categorical or ordinal)
#' variables into binary dummies for downstream
#' regression applications that do not provide
#' direct support for categorical encoded data
#'
#' @details this is designed for integration
#' with upstream use of process.phenotypes.
#' note that using these binary dummies is
#' substantially preferable to using regression
#' directly on a continuous valued encoding
#' variable, as it allows independent effects
#' for different factor levels, whereas the
#' single variable encoding makes drastic
#' assumptions about the relative effects
#' of the different levels depending on the
#' specific units chosen
#'
#' @param vec input categorical data,
#' in either factor or native format
#' @param factor.levels list; expected to
#' be configuration data from process.phenotypes.
#' exact format is: list, where keys are desired
#' level encodings for the variable, and
#' factor.levels[[key]]$name is the corresponding
#' value observed in the input data
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
#' @return data frame, representing the binary conversion of
#' the input data. column names will either be in the format
#' "variablename.refREFLEVEL.cmpCMPLEVEL" or
#' "variablename.refREFLEVEL.cmpOTHER" for the catchall collapsed
#' factor level, if created
construct.binary.dummies <- function(vec,
                                     variable.name,
                                     factor.levels,
                                     collapse.limit) {
  out.df <- data.frame()
  collapsed.levels <- c()
  for (i in 2:length(factor.levels)) {
    if (length(which(vec == names(factor.levels)[i])) <= collapse.limit) {
      collapsed.levels <- c(collapsed.levels, names(factor.levels)[i])
      next
    }
    new.var <- gap.construct.trait.file::make.binary.dummy(
      vec,
      names(factor.levels)[i]
    )
    new.var.name <- paste(variable.name,
      ".ref", names(factor.levels)[1],
      ".cmp", names(factor.levels)[i],
      sep = ""
    )
    if (i == 2) {
      out.df <- data.frame(new.var)
      colnames(out.df) <- new.var.name
    } else {
      out.df[, new.var.name] <- new.var
    }
  }
  if (length(collapsed.levels) > 0) {
    out.df[, paste(variable.name,
      ".ref", names(factor.levels)[1],
      ".cmpOTHER",
      sep = ""
    )] <- gap.construct.trait.file::make.binary.dummy(vec, collapsed.levels)
  }
  ## override naming scheme if there were only two levels
  if (length(factor.levels) == 2) {
    colnames(out.df) <- variable.name
  }
  out.df
}
