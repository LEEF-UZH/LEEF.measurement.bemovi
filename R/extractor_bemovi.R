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
  bmc_org <- file.path(input, "bemovi", "bemovi_extract.ORGORGORG.yml")
  bmc <- file.path(input, "bemovi", "bemovi_extract.yml")
  on.exit(
    {
      unlink( bmc )
      if ( file.exists( bmc_org ) ) {
        file.copy(
          from = bmc_org,
          to = bmc
        )
      }
    }
  )

  if ( file.exists( bmc ) ) {
    file.rename(
      from = bmc,
      to = bmc_org
    )
  }

  bmcs <- list.files(
    path = file.path(input, "bemovi"),
    "bemovi_extract.*\\.yml$",
    recursive = FALSE,
    full.names = TRUE
  )

  timestamp <- yaml::read_yaml(file.path(input, "bemovi", "sample_metadata.yml"))$timestamp

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
    # extractor_bemovi_id_species(input, output)
    # final_files <- c(final_files, "Morph_mvt.F2.rds", "Master.filtered.csv", "Mean_density_per_ml.csv")

    # Copy RRD ----------------------------------------------------------------

    for (fn in final_files) {
      csv <- file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder(), fn)
      rds <- gsub("\\.csv$", ".rds", csv)
      ##
      dat <- readRDS( rds )
      dat <- cbind(timestamp = timestamp, dat)
      utils::write.csv(
        dat,
        file.path(output, "bemovi", fn)
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




