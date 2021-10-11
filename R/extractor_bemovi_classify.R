#' Extractor bemovi to id species and calculate density
#'
#' This function id's the species and calculates the densities.
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @import bemovi.LEEF
#' @importFrom  stats predict
#' @importFrom  data.table as.data.table setkey as.data.table
#' @importFrom dplyr group_by summarise mutate n filter full_join
#' @importFrom purrr reduce
#' @importFrom tidyselect any_of
#' @importFrom utils read.csv tail
#' @import e1071
#' @import loggit
#'
#' @export

extractor_bemovi_classify <- function(
  input,
  output
) {
  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  loggit::set_logfile(file.path(output, "bemovi", "bemovi.log"))

  message("########################################################")
  message("   BEGIN classify bemovi...")

  # Load bemovi_extract.yml parameter ---------------------------------------

bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))

# ID Species --------------------------------------------

processing <- file.path(normalizePath(output), "bemovi", "PROCESSING.CLASSIFY.SPECIES.PROCESSING")
error <- file.path(normalizePath(output), "bemovi", "ERROR.CLASSIFY.SPECIES.ERROR")
on.exit({
  if (file.exists(processing)) {
    unlink(processing)
    file.create(error)
    message("   ERROR classify bemovi")
    message("   END classify bemovi")
  }
})

##
file.create(processing)

# Define and create temporary folder structure -------------------------------------------------

bemovi.LEEF::par_to.data(file.path(output, "tmp.bemovi"))
bemovi.LEEF::Create_folder_structure()

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


# Script begin ------------------------------------------------------------


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

  for (i in seq_along(morph_mvt_list)) {
    message("      classifying ", i)
    df <- morph_mvt_list[[i]]

    temperature_treatment <- unique(df$temperature_treatment) # either "constant" or "increasing"
    composition_id <- unique(df$composition_id) # a char between c_01 and c_16
    noNAs <- !rowSums(is.na(df)) > 0

    if (temperature_treatment == "constant"){
      pr <- predict(classifiers_constant[[composition_id]], df, probability = TRUE)
      df$species[noNAs] <- as.character(pr) # species prediction
      df$species_probability[noNAs] <- apply(attributes(pr)$probabilities,1,max) # probability of each species prediction
      probabilities <- attributes(pr)$probabilities
      colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
      if (sum(!noNAs) == 0){
        df <- cbind(df, probabilities)
      } else {
        df_noNAs <- cbind(df[noNAs,], probabilities)
        probabilities[] <- NA
        while (nrow(probabilities) < sum(!noNAs)) {
          probabilities <- rbind(probabilities, NA)
        }
        df_NAs <- cbind(df[!noNAs,], as.data.frame(probabilities)[1:sum(!noNAs),])
        df <- rbind(df_noNAs, df_NAs)
      }
    } else {
      pr <- predict(classifiers_increasing[[composition_id]], df, probability = TRUE)
      df$species[noNAs] <- as.character(pr) # species prediction
      df$species_probability[noNAs] <- apply(attributes(pr)$probabilities,1,max)  # probability of each species prediction
      probabilities <- attributes(pr)$probabilities
      colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
      if (sum(!noNAs) == 0){
        df <- cbind(df, probabilities)
      } else {
        df_noNAs <- cbind(df[noNAs,], probabilities)
        probabilities[] <- NA
        while (nrow(probabilities) < sum(!noNAs)) {
          probabilities <- rbind(probabilities, NA)
        }
        df_NAs <- cbind(df[!noNAs,], as.data.frame(probabilities)[1:sum(!noNAs),])
        df <- rbind(df_noNAs, df_NAs)
      }
    }
    morph_mvt_list[[i]] <- df
  }

  # 4. Merge the 32 dfs back into a single df: morph_mvt

  morph_mvt <- purrr::reduce(morph_mvt_list, dplyr::full_join)

  # 5. Add species identity to trajectory.data
  take_all <- data.table::as.data.table(morph_mvt)
  take_all <- take_all[, list(id, species)]
  data.table::setkey(take_all, id)
  data.table::setkey(trajectory.data.filtered, id)
  trajectory.data.filtered <- trajectory.data.filtered[take_all]
  trajectory.data <- trajectory.data.filtered

  trajectory.data$predict_spec <- trajectory.data$species # needed for overlays

  # -----------------------------------------------------------------------------------------------------
  # calculate species densities -------------------------------------------------------------------------

  # add temporary dummy variables in trajectory.data to be deleted afterwards

  exp_design <- as.data.frame(
    read.table(
      file.path(input, "bemovi", par_video.description.file()),
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE
      )
    )

  empty_videos_ind <- which(!is.element(exp_design$file, unique(trajectory.data$file)))
  empty_videos <- exp_design[empty_videos_ind,]

  dummy_rows <- trajectory.data[1:nrow(empty_videos),]

  dummy_rows <- dplyr::left_join(empty_videos, dummy_rows, by=colnames(empty_videos))
  dummy_rows$frame <- 1
  dummy_rows$species <- "dummy_species"

  trajectory.data <- rbind(trajectory.data, dummy_rows)

  # density for each frame in each sample

  area_org <- bemovi.LEEF::par_width() * bemovi.LEEF::par_height()

  area_crop <-
    (
      max(
        bemovi.LEEF::par_crop_pixels()$xmin, 0) - min(bemovi.LEEF::par_crop_pixels()$xmax,
        bemovi.LEEF::par_width())
    ) * (
      max(
        bemovi.LEEF::par_crop_pixels()$ymin, 0) - min(bemovi.LEEF::par_crop_pixels()$ymax,
        bemovi.LEEF::par_height()
      )
    )

  cropping.factor <- area_org / area_crop

  count_per_frame <- trajectory.data %>%
    group_by(
      file,
      date,
      species,
      bottle,
      composition_id,
      temperature_treatment,
      magnification,
      sample,
      video,
      frame,
      dilution_factor
    ) %>%
    summarise(count = n()) %>%
    mutate(dens.ml = count * bemovi.LEEF::par_extrapolation.factor() * cropping.factor * dilution_factor)

  mean_density_per_ml <- count_per_frame %>%
    group_by(
      date,
      species,
      composition_id,
      bottle,
      temperature_treatment,
      magnification,
      sample) %>%
    summarise(density = sum(dens.ml) / (3*125)) # previously: 'density = mean(dens.ml)'


  # -----------------------------------------------------------------------------------------------------
  # add density = 0 for extinct species ------------------------------------------------------------

  # magnification & cropping specific!


  comps <- utils::read.csv(
    file.path(
      output,
      "bemovi",
      "compositions.csv"
    )
  )

  comp_id <- unique(comps$composition)
  comps <- comps %>%
    dplyr::select(tidyselect::any_of(bemovi.LEEF::par_species_tracked()))

  comps.list <- apply(comps, 1, function(x) {
    idx <- which(x == 1)
    names(idx)
  })
  names(comps.list) <- comp_id

  mean_density_per_ml_list <- split(x = mean_density_per_ml,
                                    f = mean_density_per_ml$bottle,
                                    drop = T)


  for (i in seq_along(mean_density_per_ml_list)) {
    df <- mean_density_per_ml_list[[i]]
    ID <- unique(df$composition_id)
    idx <- which(!is.element(unlist(comps.list[[ID]]), df$species))
    if (length(idx) == 0) next
    for (j in idx) {
      new.entry <- utils::tail(df, 1)
      new.entry$species <- comps.list[[ID]][j]
      new.entry$density <- 0
      df <- rbind(df, new.entry)
    }
    mean_density_per_ml_list[[i]] <- df
  }

  mean_density_per_ml <- do.call("rbind", mean_density_per_ml_list) %>%
    dplyr::filter(species %in% bemovi.LEEF::par_species_tracked())
  morph_mvt <- morph_mvt %>%
    dplyr::filter(species %in% bemovi.LEEF::par_species_tracked())
  trajectory.data <- trajectory.data %>%
    dplyr::filter(species %in% bemovi.LEEF::par_species_tracked())


# Script end --------------------------------------------------------------

  outfiles <- c(
    morph_file = file.path(
      bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_morph_mvt()
    ),
    #    traj.unfiltered_file = file.path(
    # bemovi.LEEF::par_to.data(), "/6 - merged data unfiltered",
    # bemovi.LEEF::par_master()
    # ),
    traj.filtered_file = file.path(
      bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_master()
    ),
    mean.dens_file = file.path(
      bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_mean_density()
    )
  )

  saveRDS(
    morph_mvt,
    file = outfiles["morph_file"]
  )
  saveRDS(
    # trajectory.data.filtered,
    trajectory.data,
    file = outfiles["traj.filtered_file"]
  )
  saveRDS(
    mean_density_per_ml,
    file = outfiles["mean.dens_file"]
  )

  # Finalize ----------------------------------------------------------------

  if (all(file.exists(outfiles))) {
    file.copy(
      from = file.path(outfiles),
      to = file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()),
      overwrite = TRUE
    )
  } else {
    file.create(error)
  }

  unlink(bemovi.LEEF::par_to.data(), recursive = TRUE)
  unlink(processing)

  message("   END classify bemovi")
  message("########################################################")

  invisible(TRUE)
}
