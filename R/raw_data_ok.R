#' Check if data in raw data folder is OK
#'
#' @param input raw data folder containing bemovi data, i.e usually is \code{some/path/bemovi}
#'
#' @return \code{TRUE} if ok, \code{FALSE} or \code{list} of problems if not
#'
#' @importFrom utils read.delim
#' @importFrom utils read.table
#'
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

  if (ok$video_description) {
    vd <- utils::read.table(file.path(input, "bemovi", "video.description.txt"), header = TRUE)
    vf <- list.files(file.path(input, "bemovi"), pattern = "\\.cxd")
    invd <- gsub("\\.cxd", "", vf) %in% vd$file
    names(invd) <- vf
    ok$videofile_in_video_description <- invd
  } else {
    ok$videofile_in_video_description <- FALSE
  }
  # ok$video_file_names <- bemovi.LEEF::check_video_file_names(
  #   to.data = input,
  #   raw.video.folder = "./",
  #   video.description.folder = "."
  # )

  return(ok)
}
