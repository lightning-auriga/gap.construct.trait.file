#' Apply requested transformation to a variable
#'
#' @param vec numeric vector; input data for transformation
#' @param transform.type character vector; name of requested transformation
#' @param transform.strat list; set of variables on which to stratify
#' transformation, when relevant
#' @return transformed input vector
transform.variable <- function(vec,
                               transform.type,
                               transform.strat) {
  if (is.null(transform.type)) {
    ## just echo what was input
    vec
  } else if (transform.type == "inverse_normal_transform") {
    process.phenotypes::derive.rank.normal.transform(vec,
      stratification.vars = transform.strat
    )
  } else {
    stop("requested transformation \"", transform.type, "\" not supported by gap.construct.trait.file", sep = "")
  }
}
