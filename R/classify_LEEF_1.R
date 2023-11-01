#' Classify `morph_mvt` nad calculates densities
#'
#' @param morph_mvt merged track data - one row per particle
#' @param classifiers_constant constant tempersture classifier
#' @param classifiers_increasing increasing temperature classifier
#'
#' @return `morph_mvt` with the classified species and probabilities
#' @export
#'
#' @md
#'
#' @examples
classify <- function(
  bemovi_extract,
  morph_mvt,
  trajectory_data,
  classifiers_constant,
  classifiers_increasing,
  video_description_file,
  composition
){
  bemovi.LEEF::load_parameter(bemovi_extract)

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

  # 5. Add species identity to trajectory_data


  take_all <- data.table::as.data.table(morph_mvt)
  take_all <- take_all[, list(id, species)]
  data.table::setkey(take_all, id)
  data.table::setkey(trajectory_data, id)
  trajectory_data <- trajectory_data[take_all]

  ispecies_column <- which(names(trajectory_data) == "i.species")
  if (length(ispecies_column) > 0){
    trajectory_data$species <- trajectory_data$i.species
    trajectory_data <- subset(trajectory_data, select = -ispecies_column)
  }


  trajectory_data$predict_spec <- trajectory_data$species # needed for overlays

  empty_videos_ind <- which(!is.element(video_description_file$file, unique(trajectory_data$file)))

  if (length(empty_videos_ind) > 0) {
    empty_videos <- video_description_file[empty_videos_ind,]

    dummy_rows <- trajectory_data[1:nrow(empty_videos),]

    dummy_rows <- dplyr::left_join(empty_videos, dummy_rows, by=colnames(empty_videos))
    dummy_rows$frame <- 1
    dummy_rows$species <- "dummy_species"

    trajectory_data <- rbind(trajectory_data, dummy_rows)
  }

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

  count_per_frame <- trajectory_data %>%
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
      sample
    ) %>%
    summarise(
      numberOfVideos = length(unique(file)),
      density = sum(dens.ml) / (numberOfVideos * 125)
    ) %>%
    mutate(numberOfVideos = NULL)
 # previously: 'density = mean(dens.ml)'


  # -----------------------------------------------------------------------------------------------------
  # add density = 0 for extinct species ------------------------------------------------------------

  # magnification & cropping specific!

  comp_id <- unique(composition$composition)
  composition <- composition %>%
    dplyr::select(tidyselect::any_of(bemovi.LEEF::par_species_tracked()))

  composition.list <- apply(composition, 1, function(x) {
    idx <- which(x == 1)
    names(idx)
  })
  names(composition.list) <- comp_id

  mean_density_per_ml_list <- split(x = mean_density_per_ml,
                                    f = mean_density_per_ml$bottle,
                                    drop = T)


  for (i in seq_along(mean_density_per_ml_list)) {
    df <- mean_density_per_ml_list[[i]]
    ID <- unique(df$composition_id)
    idx <- which(!is.element(unlist(composition.list[[ID]]), df$species))
    if (length(idx) == 0) next
    for (j in idx) {
      if (composition.list[[ID]][j] %in% bemovi.LEEF::par_species_tracked()) {
        new.entry <- utils::tail(df, 1)
        new.entry$species <- composition.list[[ID]][j]
        new.entry$density <- 0
        df <- rbind(df, new.entry)
      }
      mean_density_per_ml_list[[i]] <- df
    }

  }


  mean_density_per_ml <- do.call("rbind", mean_density_per_ml_list) %>%
    dplyr::filter(species %in% bemovi.LEEF::par_species_tracked())
  ### TO CHECK IF CAN BE DONE EARLIER
  morph_mvt <- morph_mvt %>%
    dplyr::filter(species %in% bemovi.LEEF::par_species_tracked())
  ###
  trajectory_data <- trajectory_data %>%
    dplyr::filter(species %in% bemovi.LEEF::par_species_tracked())


  return(
    list(
      morph_mvt = morph_mvt,
      trajectory_data = trajectory_data,
      mean_density_per_ml = mean_density_per_ml
    )
  )
}
