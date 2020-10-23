#' Extractor bemovi data
#'
#' Analyse all \code{.avi} files in \code{bemovi} folder and save as \code{.rds} file.
#'
#' This function is executing \code{extractor_bemovi_particle()},
#' \code{extractor_bemovi_trajectory()} and finally
#' \code{extractor_bemovi_merge()}
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export

extractor_bemovi <- function(
  input,
  output
) {
  # if ( Sys.info()['sysname'] != "Linux" ) {
    extractor_bemovi_particle(input, output)
  # }
  extractor_bemovi_trajectory(input, output)
  extractor_bemovi_merge(input, output)
  ##
  invisible(TRUE)
}

#' Extractor bemovi data FOR LINUX ON CLUSTER
#'
#' Analyse all \code{.avi} files in \code{bemovi} folder and save as \code{.rds} file.
#'
#' \bold{\code{extracxtor_bemovi_particle()} needs to be executed before!}
#'
#' This function is executing
#' \code{extractor_bemovi_trajectory()} and finally
#' \code{extractor_bemovi_merge()}
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export

extractor_bemovi_headless_linux <- function(
  input,
  output
) {
  extractor_bemovi_trajectory(input, output)
  extractor_bemovi_merge(input, output)
  ##
  invisible(TRUE)
}
