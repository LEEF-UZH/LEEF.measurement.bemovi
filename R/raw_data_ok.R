#' Check if data in raw data folder is OK
#'
#' @param input raw data folder containing bemovi data, i.e usually is \code{some/path/bemovi}
#'
#' @return \code{TRUE} if ok, \code{FALSE} or \code{list} of problems if not
#' @importFrom utils read.delim
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data_ok()
#' }
raw_data_ok <- function(input) {
  ok <- list()

  on.exit(
    if (all(unlist(ok))) {
      return(TRUE)
    } else {
      return(ok)
    }
  )

  ok$bemovi_extract <- file.exists( file.path(input, "bemovi_extract.yml") )
  ok$video_description <- file.exists( file.path(input, "video.description.txt") )

  cxds <- list.files(
    path = input,
    pattern = ".cxd",
    full.names = TRUE
  )

  ok$cxd <- lapply(
    cxds,
    function(cxd) {
      result <- list()

      cmd <- file.path( file.path( tools_path(), "bftools", "showinf" ))
      if (is.null(cmd)) {
        stop("bftools not available in expected path!")
      }
      arguments <-  paste(
        "-nopix",
        file.path( input, cxd ),
        sep = " "
      )
      md <- system2(
        command = cmd,
        args = arguments,
        stdout = TRUE
      )

      result$filesize <-  file.size(cxd) == 525639680

      if (result$filesize) {
        tfl <- grep("Field \\d+ Time_From_Last", md, value = TRUE)
        tfl <- read.delim(text = tfl, sep = " ", header = FALSE)

        result$noframes = nrow(tfl) == 125
        meantfl <- sum(tfl$V4) / (nrow(tfl)-1)
        result$framerate <-  meantfl > 0.035 & meantfl <= 0.045
      }

      return(result)
    }
  )

  names(ok$cxd) <- basename(cxds)
}
