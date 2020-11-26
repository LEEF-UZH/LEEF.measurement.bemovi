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
#' @importFrom  yaml read_yaml
#' 
#' @export

extractor_bemovi <- function(
  input,
  output
) {
  final_files <- NULL
  extractor_bemovi_particle(input, output)
  extractor_bemovi_trajectory(input, output)
  extractor_bemovi_merge(input, output)
  final_files <- c(final_files, "Master.rds")
  extractor_bemovi_filter(input, output)
  final_files <- c(final_files, "Master.rds", "Master.filtered.rds", "Morph_mvt.F1.rds", "Morph_mvt.F2.rds")
  # extractor_bemovi_id_species(input, output)
  # final_files <- c(final_files, "Morph_mvt.F2.rds", "Master.filtered.rds", "Mean_density_per_ml.rds")

  # Copy RRD ----------------------------------------------------------------

  file.copy(
    from = file.path(input, "sample_metadata.yml"),
    to = file.path(output, "sample_metadata.yml")
  )

  timestamp <- yaml::read_yaml(file.path(input, "sample_metadata.yml"))$timestamp
  for (fn in final_files) {
    dat <- readRDS( file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder(), fn) )
    dat <- cbind(timestamp = timestamp, dat)
    saveRDS( dat, file.path(output, "bemovi", fn))
  }

  ##
  invisible(TRUE)
}




