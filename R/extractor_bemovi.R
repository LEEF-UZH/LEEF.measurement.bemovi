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
  extractor_bemovi_particle(input, output)
  extractor_bemovi_trajectory(input, output)
  extractor_bemovi_merge(input, output)
  extractor_bemovi_filter(input, output)
  extractor_bemovi_id_species(input, output)

  # Copy RRD ----------------------------------------------------------------

  file.copy(
    from = list.files(
      path = file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()),
      pattern = "\\.rds$"
    ),
    to = file.path(output, "bemovi")
  )

  ##
  invisible(TRUE)
}




