#' Extractor bemovi to fid species and calculate density
#'
#' This function id's the species and calculates the densities.
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @import bemovi.LEEF
#' @importFrom  data.table as.data.table setkey
#' @importFrom dplyr group_by summarise mutate n
#'
#' @export

extractor_bemovi_id_species <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("Id Species bemovi...\n")

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Load bemovi_extract.yml parameter ---------------------------------------

  bemovi.LEEF::load_parameter( file.path(output, "bemovi", "bemovi_extract.yml") )

  # ID Species --------------------------------------------

  processing <- file.path(normalizePath(output), "bemovi", "PROCESSING.ID.SPECIES.PROCESSING")
  error <- file.path(normalizePath(output), "bemovi", "ERROR.ID.SPECIES.ERROR")
  on.exit(
    {
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
      }
    }
  )

  ##
  file.create( processing )

  # Define and create temporary folder structure -------------------------------------------------

  bemovi.LEEF::par_to.data( tempfile( pattern = "bemovi.") )
  bemovi.LEEF::Create_folder_structure()
  # file.copy(
  #   from = file.path( output, "bemovi", bemovi.LEEF::par_video.description.folder(), bemovi.LEEF::par_video.description.file() ),
  #   to   = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_video.description.folder(), bemovi.LEEF::par_video.description.file() ),
  # )

  # ID Speciese -------------------------------------------------

  morph_mvt <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_merged.data.folder(),
      "Morph_mvt.F2.rds"
    )
  )
  trajectory.data.filtered <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_merged.data.folder(),
      "Master.filtered.rds"
    )
  )

  # For now: add dummy values for variable predict_spec

  set.seed(6543) # delete row
  morph_mvt$predict_spec <- sample(
    c( "Sauron","Lord Voldemort","Darth Vader","Imperator Palpatine" ),
    size = nrow(morph_mvt),
    replace = TRUE
  ) # replace these 2 rows

  # 5. Add species identity to trajectory.data
  take_all <- as.data.table(morph_mvt)
  take_all <- take_all[, list(id, predict_spec)]
  setkey(take_all, id)
  setkey(trajectory.data.filtered, id)
  trajectory.data.filtered <- trajectory.data.filtered[take_all]

  # -----------------------------------------------------------------------------------------------------
  # calculate species densities -------------------------------------------------------------------------
  # density for each frame in each sample

  extrapolation.factor <- 13.84 # to be updated WILL THI BE CONSTANT??

  count_per_frame <- trajectory.data.filtered %>%
    dplyr::group_by(file, date, predict_spec, species.composition, microcosm.nr, temperature, magnification, sample, video, frame) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(dens.ml = count * extrapolation.factor) # change!

  mean_density_per_ml <- count_per_frame %>%
    dplyr::group_by(date, predict_spec, species.composition, microcosm.nr, temperature, magnification, sample) %>%
    dplyr::summarise(mean.dens.ml = mean(dens.ml))

  outfiles <- c(
    morph_file         = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.F2.rds" ),
    traj.filtered_file = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.filtered.rds"),
    mean.dens_file     = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Mean_density_per_ml.rds" )
  )
  saveRDS(
    morph_mvt,
    file = outfiles["morph_file"]
  )
  saveRDS(
    trajectory.data.filtered,
    file = outfiles["traj.filtered_file"]
  )
  saveRDS(
    mean_density_per_ml,
    file = outfiles["mean.dens_file"]
  )

  # Finalize ----------------------------------------------------------------

  if ( all(file.exists( outfiles )) ) {
    file.copy(
      from = file.path( outfiles ),
      to   = file.path( output, "bemovi", bemovi.LEEF::par_merged.data.folder() )
    )
  } else {
    file.create( error )
  }

  unlink(bemovi.LEEF::par_to.data(), recursive = TRUE)
  unlink( processing )

  message("\ndone\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
