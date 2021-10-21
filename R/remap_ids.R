#' Map input subject IDs based on a provided linker
#'
#' @description read a file linking phenotype dataset
#' subject IDs to some other ID set, and apply the linking
#'
#' @details this function will return NA for IDs that are
#' not present in the input linker. those probably need to
#' be filtered downstream, but to preserve row ordering that
#' filtering is not applied here. note that the file can be
#' NA, in which case the input is returned unchanged
#'
#' @param vec character vector; input subject IDs in need
#' of linking, in order
#' @param filename character vector; path to and name of file
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
#' @return input data, either mapped to linked ID set
#' if the provided linker existed, or as-is otherwise
remap.ids <- function(vec,
                      filename) {
  if (!is.na(filename)) {
    stopifnot(is.vector(filename, mode = "character"), length(filename) == 1)
    stopifnot(file.exists(filename))
    linker <- read.table(filename, header = TRUE, stringsAsFactors = FALSE)
    linker <- linker[!duplicated(linker[, 1]), ]
    rownames(linker) <- linker[, 1]
    linker[vec, 2]
  } else {
    vec
  }
}
