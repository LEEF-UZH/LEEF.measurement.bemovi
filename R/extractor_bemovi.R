#' Extractor bemovi data
#'
#' Analyse all \code{.avi} files in \code{bemovi} folder and save as \code{.rds} file.
#'
#' This function is executing \code{extractor_bemovi_particle()},
#' \code{extractor_bemovi_trajectory()} and finally
#' \code{extractor_bemovi_merge()}
#' for all parameter files in the format \code{bemovi_extract*.yml}
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom  yaml read_yaml
#' @importFrom utils write.csv
#'
#' @export

extractor_bemovi <- function(
  input,
  output
) {


  # prepare output folder ---------------------------------------------------


  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  to_copy <- list.files( file.path(input, "bemovi"), full.names = TRUE )
  to_copy <- grep(
    "\\.avi$|\\.metadata",
    to_copy,
    value = TRUE,
    invert = TRUE
  )
  file.copy(
    to_copy,
    file.path(output, "bemovi"),
    overwrite = TRUE
  )

  dir.create(file.path(output, "bemovi", bemovi.LEEF::par_video.description.folder()), showWarnings = FALSE)
  file.copy(
    from = file.path(input, "bemovi", bemovi.LEEF::par_video.description.file()),
    to   = file.path(output, "bemovi", bemovi.LEEF::par_video.description.folder(), bemovi.LEEF::par_video.description.file())
  )


  # handle multiple bemovi_extract files ------------------------------------


  bmc_org <- file.path(output, "bemovi", "bemovi_extract.ORGORGORG.yml")
  bmc <- file.path(output, "bemovi", "bemovi_extract.yml")
  if ( file.exists( bmc ) ) {
    file.rename(
      from = bmc,
      to = bmc_org
    )
  }

  on.exit(
    {
      unlink( bmc )
      if ( file.exists( bmc_org ) ) {
        file.copy(
          from = bmc_org,
          to = bmc
        )
	      unlink( bmc_org )
      }
    }
  )



  bmcs <- list.files(
    path = file.path(input, "bemovi"),
    "bemovi_extract.*\\.yml$",
    recursive = FALSE,
    full.names = TRUE
  )

  timestamp <- yaml::read_yaml(file.path(output, "bemovi", "sample_metadata.yml"))$timestamp

  for (bconf in bmcs) {
    file.copy(
      from = bconf,
      to = bmc,
      overwrite = TRUE
    )
    final_files <- NULL
    extractor_bemovi_particle(input, output)
    extractor_bemovi_trajectory(input, output)
    extractor_bemovi_merge(input, output)
    final_files <- c(
      final_files,
      gsub(pattern = "\\.rds$", replacement = ".csv", bemovi.LEEF::par_master())
    )
    extractor_bemovi_filter(input, output)
    final_files <- c(
      final_files,
      gsub(pattern = "\\.rds$", replacement = ".csv", bemovi.LEEF::par_master()),
      gsub(pattern = "\\.rds$", replacement = ".csv", bemovi.LEEF::par_morph_mvt())
    )
    extractor_bemovi_id_species(input, output)
    final_files <- c(
      final_files,
      gsub(pattern = "\\.rds$", replacement = ".csv", bemovi.LEEF::par_morph_mvt()),
      gsub(pattern = "\\.rds$", replacement = ".csv", bemovi.LEEF::par_master()),
      gsub(pattern = "\\.rds$", replacement = ".csv", bemovi.LEEF::par_mean_density())
    )
    final_files <- unique(final_files)

    # DOEX NOT WORK YET!
    # extractor_bemovi_overlay(input, output)
    #

    # Copy RRD ----------------------------------------------------------------

    for (fn in final_files) {
      csv <- file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder(), fn)
      rds <- gsub("\\.csv$", ".rds", csv)
      ##
      dat <- readRDS( rds )
      dat <- cbind(timestamp = timestamp, dat)
      utils::write.csv(
        dat,
        file.path(output, "bemovi", fn),
        row.names = FALSE
      )
    }
  }


  file.copy(
    from = file.path(input, "bemovi", "sample_metadata.yml"),
    to = file.path(output, "bemovi", "sample_metadata.yml")
  )

  ##
  invisible(TRUE)
}




