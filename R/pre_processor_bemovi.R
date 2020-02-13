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
  tmpdir <- tempfile()
  dir.create( tmpdir, recursive = TRUE )
  ##
  parallel::mclapply(
    cxds,
    function(cxd) {
      cmd <- file.path( file.path( tools_path(), "bftools", "bfconvert" ))
      if (is.null(cmd)) {
        stop("bftools not available in expected path!")
      }
      arguments <-  paste(
        "-overwrite",
        "-no-upgrade",
        file.path( input, "bemovi", cxd ),
        file.path( tmpdir, gsub(".cxd", ".avi", cxd) ),
        sep = " "
      )
      if (options()$LEEF.measurement.bemovi$debug) {
        system2(
          command = cmd,
          args = arguments
        )
      } else {
        system2(
          command = cmd,
          args = arguments,
          stdout = NULL
        )

      }
    }
  )
  ##
  file.copy(
    from = file.path( input,  "bemovi", "bemovi_extract.yml"),
    to   = file.path( tmpdir, "bemovi_extract.yml"),
    overwrite = TRUE
  )
  ##
  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  file.copy(
    from   = file.path( tmpdir, "."),
    to = file.path( output,  "bemovi"),
    recursive = TRUE,
    overwrite = TRUE
  )
  unlink( tmpdir, recursive = TRUE )
  ##
  message("\ndone\n")
  message("########################################################\n")

  invisible(TRUE)
}
