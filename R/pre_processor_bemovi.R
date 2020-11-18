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
  file.copy(
    from = file.path( input,  "bemovi", "bemovi_extract.yml"),
    to   = file.path( output, "bemovi", "bemovi_extract.yml"),
    overwrite = TRUE
  )
  unlink( tmpdir, recursive = TRUE )
  ##
  if (file.exists(file.path( input, "bemovi", "video.description.txt" ))) {
    file.copy(
      from = file.path( input,  "bemovi", "video.description.txt" ),
      to   = file.path( output, "bemovi", "video.description.txt" )
    )
  } else {
    avis <- list.files(
      file.path(output, "bemovi"),
      pattern = "\\.avi"
    )
    fn <- basename(avis)
    fn <- gsub(".avi", "", fn)
    tmp <- strsplit(fn, "_")
    d <- sapply(tmp, "[[", 1)
    d <- as.Date(d, "%Y%m%d")
    no <- sapply(tmp, "[[", 2)
    no <- as.integer(no)
    vd <- data.frame(
      file = fn,
      date = d,
      no = no
    )
    utils::write.table(
      x = vd,
      file = file.path( output, "bemovi", "video.description.txt"),
      row.names = FALSE,
      sep = "\t"
    )

  }
  file.copy(
    from = file.path(input, "sample_metadata.yml"),
    to = file.path(output, "sample_metadata.yml")
  )
  ##
  message("\ndone\n")
  message("########################################################\n")

  invisible(TRUE)
}
