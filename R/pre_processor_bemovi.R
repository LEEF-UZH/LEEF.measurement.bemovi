#' Preprocessor bemovi data
#'
#' convert all \code{.cxd} files in \code{bemovi} folder to non-proprietory avi format and delete \code{.cxd} file.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom R.utils bzip2
#' @export
#'
pre_processor_bemovi <- function( input, output ) {
  message("\n########################################################\n")
  message("\nProcessing bemovi...\n")
  cxd <- list.files(
    path = file.path( input, "bemovi" ),
    pattern = "*.cxd",
    full.names = TRUE
  )
  for (fn in cxd) {
    cmd <- file.path( system.file(package = utils::packageName()), "tools", "bftools", "bfconvert" )
    arguments = paste(
      "-overwrite",
      "-no-upgrade",
      fn,
      gsub(".cxd", ".avi", fn),
      sep = " "
    )
    system2(
      command = cmd,
      args = arguments
    )
    unlink(fn)
  }
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
