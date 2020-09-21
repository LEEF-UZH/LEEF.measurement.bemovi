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

  ok$bemovi_extract <- file.exists( file.path(input, "bemovi", "bemovi_extract.yml") )
  ok$video_description <- file.exists( file.path(input, "bemovi", "video.description.txt") )

  return(ok)
}
