#' Preprocessor bemovi data
#'
#' convert all \code{.cxd} files in \code{bemovi} folder to non-proprietory avi format and delete \code{.cxd} file.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom utils write.table
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
  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  ##
  cxds <- list.files(
    path = file.path( input, "bemovi" ),
    pattern = "*.cxd",
    full.names = FALSE,
    recursive = FALSE
  )
  ##
  tmpdir <- tempfile()
  dir.create( tmpdir, recursive = TRUE )
  ##
  parallel::mclapply(
    cxds,
    function(cxd) {
      processing <- file.path(output, "bemovi", paste0("PROCESSING.", cxd, ".PROCESSING"))
      error <- file.path(output, "bemovi", paste0("ERROR.", cxd, ".ERROR"))
      on.exit(
        {
          if (file.exists(processing)) {
            unlink(processing)
            file.create(error)
          }
        }
      )
      ##
      file.create( processing )
      ##
      outfile <- file.path( tmpdir, gsub(".cxd", ".avi", cxd) )
      cmd <- file.path( file.path( tools_path(), "bftools", "bfconvert" ))
      if (is.null(cmd)) {
        stop("bftools not available in expected path!")
      }
      arguments <-  paste(
        "-overwrite",
        "-no-upgrade",
        file.path( input, "bemovi", cxd ),
        outfile,
        sep = " "
      )
      message( "Processing ", cxd )
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
      if (file.exists(outfile)) {

        # Compress ----------------------------------------------------------------

        cmd <- file.path( file.path( tools_path(), "ffmpeg" ))
        if (is.null(cmd)) {
          stop("ffmpeg not available in expected path!")
        }
        arguments <-  paste(
          "-i", file.path( outfile ),
          "-vcodec png",
          "-compression_level 10",
          "-vtag 'PNG '",
          file.path( output,  "bemovi", basename(outfile) ),
          sep = " "
        )
        message( "Compressing ", outfile )
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

        # Extract Metadata --------------------------------------------------------

        cmd <- file.path( file.path( tools_path(), "bftools", "showinf" ))
        if (is.null(cmd)) {
          stop("bftools not available in expected path!")
        }
        arguments <-  paste(
          "-nopix",
          "-no-upgrade",
          file.path( input, "bemovi", cxd ),
          sep = " "
        )
        message( "Extracting Metadata ", outfile )
        system2(
          command = cmd,
          args = arguments,
          stdout = file.path( output,  "bemovi", paste0(basename(cxd), ".metadata") )
        )

      } else {
        file.create( error )
      }
      unlink(processing)
    },
    mc.preschedule = FALSE
  )

  ##
  unlink( tmpdir, recursive = TRUE )
  ##

  file.copy(
    from = file.path(input, "sample_metadata.yml"),
    to = file.path(output, "bemovi", "sample_metadata.yml")
  )

  fn <- list.files(
    path = file.path( input,  "bemovi" ),
    recursive = FALSE,
    full.names = TRUE
  )
  fn <- grep(
    pattern = "\\.cxd$",
    x = fn,
    invert = TRUE,
    value = TRUE
  )

  file.copy(
    from = fn,
    to = file.path( output, "bemovi" ),
    overwrite = TRUE
  )

  ##
  message("\ndone\n")
  message("########################################################\n")

  invisible(TRUE)
}
