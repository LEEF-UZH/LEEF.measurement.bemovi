#' Preprocessor bemovi data
#'
#' Convert all \code{.cxd} files in the \code{input/bemovi} folder to non-proprietary `avi` format.
#'
#' This function **requires** the following files and directories:
#'
#' - one `input` directory which contains
#'    - a folder named `bemovi` with
#'       - the `.cxd` files
#'       - a file `bemovi_extract.yml` containing all the parameter for the analysis.
#'         This parameter file will be loaded for the analysis.
#'    - a folder named `00.general.parameter` at the same level as `input`
#'
#' This function **creates** the following folder if it does not exist:
#' - `output\bemovi` in which will contain
#'    - the `.avi` files (converted `.cxd` files)
#'    - the `.metadata` text files containing the extracted metadata from the `.cxd` files
#'    - the files recursively copied from the `00.general.parameter` folder
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @md
#'
#' @import loggit
#'
#' @export
#'
pre_processor_bemovi <- function(
  input,
  output
) {
  if ( length( list.files( file.path(input, "bemovi") ) ) == 0 ) {
    message("\nEmpty or missing bemovi directory - nothing to do.\n")
    message("\ndone\n")
    message("########################################################\n")
    return(invisible(TRUE))
  }

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  loggit::set_logfile(file.path(output, "bemovi", "bemovi.log"))

  message("########################################################")
  message("Processing bemovi...")
  ##

  # Load bemovi_extract.yml parameter ---------------------------------------
  # bemovi.LEEF::par_showinf(file.path(input, "..", LEEF::opt_directories()$tools, "bftools", "showinf"))
  bemovi.LEEF::par_showinf(normalizePath(file.path(input, "..", "tools", "bftools", "showinf")))
  # bemovi.LEEF::par_bfconvert(file.path(input, "..", LEEF::opt_directories()$tools, "bftools", "bfconvert"))
  bemovi.LEEF::par_bfconvert(normalizePath(file.path(input, "..", "tools", "bftools", "bfconvert")))
  # bemovi.LEEF::par_ffmpeg(file.path(input, "..", LEEF::opt_directories()$tools, "ffmpeg"))
  bemovi.LEEF::par_ffmpeg(normalizePath(file.path(input, "..", "tools", "ffmpeg")))
  bemovi.LEEF::par_mc.cores(getOption("mc.cores", 1))

  if (length( list.files( file.path(input, "bemovi"))) == 0) {
    message("Empty or missing bemovi directory - nothing to do.")
    message("END")
    message("########################################################")
    return(invisible(TRUE))
  }

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  ##

  bemovi.LEEF::convert_cxd_to_avi(
    cxd_file = file.path(input, "bemovi"),
    avi_dir = file.path(output, "bemovi"),
    compression_level = 4,
    delete_cxd = FALSE,
    mc.cores = getOption("mc.cores")
  )

  file.copy(
    file.path(input, "..", "00.general.parameter", "."),
    file.path(output, "bemovi"),
    recursive = TRUE,
    overwrite = TRUE
  )

  fn <- list.files(
    path = file.path(input,  "bemovi"),
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
    to = file.path(output, "bemovi"),
    overwrite = TRUE
  )

  ##
  message("END")
  message("########################################################")

  invisible(TRUE)
}
