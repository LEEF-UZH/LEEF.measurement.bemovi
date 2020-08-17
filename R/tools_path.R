#' Read and write tools_path
#'
#' Read or write the tools path where the tools are located in. If no parameter
#' is given, the path will be returned as a list.
#' @param tools_path  \code{character} \code{vector} of length one containing the
#'   directory where the tools are located
#'
#' @return tools directory. If values have set, the value before the change.
#'
#'
#' @export
#'
#' @examples
#' tools_path()
#'
#' tools_path("./temp")
#'
tools_path <- function(
  tools_path
){
  opt <- getOption("LEEF.measurement.bemovi")
  if (is.null(opt)) {
    stop("Something is wrong - Options not initialized!")
  }
  ##
  tp <- opt$tools_path
  read <- TRUE
  ##
  if (!missing(tools_path)) {
    if (length(tools_path) != 1) {
      stop("length of the vector has to be one!")
    }
    opt$tools_path <- tools_path
    options(LEEF.measurement.bemovi = opt)
    return(invisible( tp ))
  } else {
    return( tp )
  }
}
