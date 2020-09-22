#' Check if data in input folder is OK and move to raw data folder
#'
#' @param input The folder, where a folder \code{bemovi} is located which
#'   contains the new files.
#' @param output A folder, which contains a subfolder called \code{bemovi}, i.e.
#'   the usually the raw data folder, into which the fioles will be moved to.
#'
#' @return a \code{list} which contains the individual reseults for each file.
#'   \code{TRUE} if moved, \code{FALSE} if an error occured. Details of the eror
#'   re in the error files in the \code{input/bemovi} directory.
#' @importFrom parallel mclapply
#' @export
#'
add_new_data <- function(input, output) {
  ##
  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Copy ALL other files ----------------------------------------------------

  others <- grep(
    list.files(
      path = input,
      full.names = TRUE
    ),
    pattern='.cxd',
    invert=TRUE,
    value=TRUE
  )
  file.copy(
    from = others,
    to = file.path(output, "bemovi"),
    overwrite = TRUE
  )
  unlink( others )

  # Check and move cxd ------------------------------------------------------

  cxds <- list.files(
    path = input,
    pattern = ".cxd",
    full.names = TRUE
  )



  cmd <- file.path( file.path( tools_path(), "bftools", "showinf" ))
  if (is.null(cmd)) {
    stop("bftools not available in expected path!")
  }
  ##
  ok <- parallel::mclapply(
    cxds,
    function(cxd) {
      processing <- file.path(input, paste0("CHECKING.", basename(cxd), ".CHECKING"))
      error <- file.path(input, paste0("ERROR.", basename(cxd), ".txt"))

      on.exit(
        {
          if (file.exists(processing)) {
            unlink(processing)
            capture.output(print(result), file = error)
          }
        }
      )
      ##
      file.create( processing )
      ##
      message("checking ", cxd)
      result <- list(
        ok = TRUE
      )

      # Check Filesize ----------------------------------------------------------

      result$filesize <-  file.size(cxd) == 525639680
      result$ok <- result$filesize & result$ok

      # Read metadata -----------------------------------------------------------

      arguments <-  paste(
        "-nopix",
        "-no-upgrade",
        file.path( cxd ),
        sep = " "
      )
      result$metadata <- system2(
        command = cmd,
        args = arguments,
        stdout = TRUE
      )

      # Check Framerate ---------------------------------------------------------

      if (result$filesize) {
        tfl <- grep("Field \\d+ Time_From_Last", result$metadata, value = TRUE)
        tfl <- read.delim(text = tfl, sep = " ", header = FALSE)

        result$noframes = nrow(tfl) == 125
        meantfl <- sum(tfl$V4) / (nrow(tfl)-1)
        result$framerate <-  meantfl > 0.035 & meantfl <= 0.045
        result$ok <- result$filesize & result$ok
      } else {
        result$ok <- FALSE
      }

      if ( result$ok ) {
        file.copy(
          from = cxd,
          to = file.path(output, "bemovi"),
          overwrite = TRUE
        )
        unlink( cxd )
        unlink(processing)
      }
      return(result)
    }
  )
  names(ok) <- cxds
  return(ok)
}
