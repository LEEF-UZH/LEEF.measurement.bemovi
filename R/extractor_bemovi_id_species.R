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

  # ID Species --------------------------------------------------

  morph_mvt <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_morph_mvt()
    )
  )
  trajectory.data.filtered <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_master()
    )
  )


# Scripd begin ------------------------------------------------------------


  # 1. Load in random classifiers (rds)

  classifiers_constant <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_classifier_constant()
    )
  )

  classifiers_increasing <- readRDS(
    file.path(
      output,
      "bemovi",
      bemovi.LEEF::par_classifier_increasing()
    )
  )


  # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime

  morph_mvt_list <- split(
    x = morph_mvt,
    f = interaction(morph_mvt$composition_id, morph_mvt$temperature_treatment),
    drop = TRUE
  )

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers

  for(i in 1:length(morph_mvt_list)){

    df <- morph_mvt_list[[i]]

    temperature_treatment <- unique(df$temperature_treatment) # either "constant" or "increasing"
    composition_id <- unique(df$composition_id) # a char between c_01 and c_16

    if (temperature_treatment == "constant"){
      df$species <- predict(classifiers_constant[[composition_id]], df) # species prediction
      df$species_probability <- apply(predict(classifiers_constant[[composition_id]], df, type = "prob"),
                                      1,max) # probability of each species prediction
    } else {
      df$species <- predict(classifiers_increasing[[composition_id]], df) # species prediction
      df$species_probability <- apply(predict(classifiers_increasing[[composition_id]], df, type = "prob"),
                                      1,max) # probability of each species prediction
    }
    morph_mvt_list[[i]] <- df
  }

  # 4. Merge the 32 dfs back into a single df: morph_mvt

  morph_mvt <- do.call("rbind", morph_mvt_list)

  # 5. Add species identity to trajectory.data
  take_all <- as.data.table(morph_mvt)
  take_all <- take_all[, list(id, species)]
  setkey(take_all, id)
  setkey(trajectory.data.filtered, id)
  trajectory.data.filtered <- trajectory.data.filtered[take_all]
  trajectory.data <- trajectory.data.filtered

  trajectory.data$predict_spec <- trajectory.data$species # needed for overlays

  # -----------------------------------------------------------------------------------------------------
  # calculate species densities -------------------------------------------------------------------------

  # density for each frame in each sample

  area_org <- bemovi.LEEF::par_width() * bemovi.LEEF::par_height()

  area_crop <-
    ( max(bemovi.LEEF::par_crop_pixels()$xmin, 0) - min(bemovi.LEEF::par_crop_pixels()$xmax, bemovi.LEEF::par_width() ) ) *
    ( max(bemovi.LEEF::par_crop_pixels()$ymin, 0) - min(bemovi.LEEF::par_crop_pixels()$ymax, bemovi.LEEF::par_height()))

  cropping.factor <- area_org / area_crop

  count_per_frame <- trajectory.data %>%
    group_by(file, date, species, bottle, composition_id, temperature_treatment, magnification, sample, video, frame, dilution_factor) %>%
    summarise(count = n()) %>%
    mutate(dens.ml = count * bemovi.LEEF::par_extrapolation.factor() * cropping.factor * dilution_factor)

  mean_density_per_ml <- count_per_frame %>%
    group_by(date, species, composition_id, bottle, temperature_treatment, magnification, sample) %>%
    summarise(density = mean(dens.ml))


  # -----------------------------------------------------------------------------------------------------
  # add density = 0 for extinct species ------------------------------------------------------------

  # magnification & cropping specific!


  comps <- read.csv(
    file.path(
      output,
      "bemovi",
      "compositions.csv"
    )
  )

  comp_id <- unique(comps$composition)
  comps <- comps %>%
    dplyr::select(tidyselect::any_of(bemovi.LEEF::par_species_tracked()))

  comps.list <- apply(comps, 1, function(x){
    idx <- which(x==1)
    names(idx)
  })
  names(comps.list) <- comp_id

  mean_density_per_ml_list <- split(x = mean_density_per_ml,
                                    f = mean_density_per_ml$bottle,
                                    drop = T)


  for(i in 1:length(mean_density_per_ml_list)){
    df <- mean_density_per_ml_list[[i]]
    ID <- unique(df$composition_id)
    idx <- which(!is.element(unlist(comps.list[[ID]]), df$species))
    if(length(idx)==0) next
    for(j in idx){
      new.entry <- tail(df,1)
      new.entry$species <- comps.list[[ID]][j]
      new.entry$density <- 0
      df <- rbind(df, new.entry)
    }
    mean_density_per_ml_list[[i]] <- df
  }

  mean_density_per_ml <- do.call("rbind", mean_density_per_ml_list) %>%
    filter(species %in% species.tracked)
  morph_mvt <- morph_mvt %>%
    filter(species %in% species.tracked)
  trajectory.data <- trajectory.data %>%
    filter(species %in% species.tracked)


# Script end --------------------------------------------------------------

  outfiles <- c(
    morph_file           = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_morph_mvt() ),
    traj.unfiltered_file = file.path( bemovi.LEEF::par_to.data(), "/6 - merged data unfiltered",         bemovi.LEEF::par_master()),
    traj.filtered_file   = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_master()),
    mean.dens_file       = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), beomvi.LEEF::par_mean_density() )
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
