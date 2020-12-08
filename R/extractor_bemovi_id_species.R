#' Extractor bemovi to fid species and calculate density
#'
#' This function id's the species and calculates the densities.
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @import bemovi.LEEF
#' @importFrom  stats predict
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
      "Morph_mvt.rds"
    )
  )
  trajectory.data.filtered <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_merged.data.folder(),
      "Master.rds"
    )
  )

  ################# ADAPT BEGIN
  # 1. Load in random classifiers (RData)

  # load(rf_classifiers_Videos_constant_temperature.RData)
  # load(current_best_rf_classifiers_Videos_increasing_temperature.RData) # the classifiers for increasing temperatures will have to be updated during experiment!!
  classifiers <- list(
    readRDS( file.path( input, "bemovi", "classifiers_const_temp.rds")),
    readRDS( file.path( input, "bemovi", "classifiers_inc_temp.rds"))
  )

  # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime

  morph_mvt_list <- split(
    x = morph_mvt,
    f = interaction(morph_mvt$species.composition, morph_mvt$temperature_treatment),
    drop = TRUE
  )

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers

  for(i in 1:length(morph_mvt_list)){

    df <- morph_mvt_list[[i]]

    # either "constant" or "increasing"

    temperature_treatment <- unique(df$temperature_treatment)

    # an integer between 1 and 16
    species.composition <- unique(df$species.composition)

    # get the right classifier name for a certain bottle and temperature treatment. e.g. "rf_constant_7"
    classifier_name <- paste(
      "rf",
      temperature_treatment,
      species.composition,
      sep = "_"
    )

    # species prediction
    df$species <- stats::predict(
      classifiers[[class_name]],
      df
    )

    # probability of each species prediction
    df$species_probability <-
      apply(
        stats::predict(
          classifiers[[class_name]],
          df,
          type = "prob"
        ),
        1,
        max
      )

    morph_mvt_list[[i]] <- df
  }

  # 4. Merge the 32 dfs back into a single df: morph_mvt

  morph_mvt <- do.call("rbind", morph_mvt_list)

  # 5. Add species identity to trajectory.data
  take_all <- data.table::as.data.table(morph_mvt)
  take_all <- take_all[, list(id, species)]
  data.table::setkey(take_all, id)
  data.table::setkey(trajectory.data.filtered, id)
  trajectory.data.filtered <- trajectory.data.filtered[take_all]
  trajectory.data <- trajectory.data.filtered

  trajectory.data$predict_spec <- trajectory.data$species # needed for overlays

  ################# ADAPT END


  # -----------------------------------------------------------------------------------------------------
  # calculate species densities -------------------------------------------------------------------------
  # density for each frame in each sample

  # extrapolation.factor <- 13.84 # to be updated WILL THI BE CONSTANT??

  count_per_frame <- trajectory.data.filtered %>%
    dplyr::group_by(file, date, species, species.composition, microcosm.nr, temperature, magnification, sample, video, frame) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(dens.ml = count * bemovi.LEEF::par_extrapolation.factor()) # change!

  mean_density_per_ml <- count_per_frame %>%
    dplyr::group_by(date, species, species.composition, microcosm.nr, temperature, magnification, sample) %>%
    dplyr::summarise(mean.dens.ml = mean(dens.ml))

  outfiles <- c(
    morph_file         = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.rds" ),
    traj.filtered_file = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.rds"),
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
