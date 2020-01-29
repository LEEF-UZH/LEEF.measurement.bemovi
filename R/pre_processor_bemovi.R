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
#' @importFrom parallel mclapply
#'
#' @export
#'
pre_processor_bemovi <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("Processing bemovi...\n")
  ##
  cxds <- list.files(
    path = file.path( input, "bemovi" ),
    pattern = "*.cxd"
  )
  ##
  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  ##
  parallel::mclapply(
    cxds,
    function(cxd) {
      cmd <- file.path( system.file(package = "LEEF.measurement.bemovi", "tools", "bftools", "bfconvert" ))
      arguments = paste(
        "-overwrite",
        "-no-upgrade",
        file.path( input, "bemovi", cxd ),
        file.path( output, "bemovi", gsub(".cxd", ".avi", cxd) ),
        sep = " "
      )
      system2(
        command = cmd,
        args = arguments
      )
    }
  )
  ##
  file.copy(
    from = file.path( input,  "bemovi", "bemovi_extract.yml"),
    to   = file.path( output, "bemovi", "bemovi_extract.yml"),
    overwrite = TRUE
  )
  message("\ndone\n")
  message("########################################################\n")

  invisible(TRUE)
}
