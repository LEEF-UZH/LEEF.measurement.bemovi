#' Classify `morph_mvt` nad calculates densities
#'
#' @param morph_mvt merged track data - one row per particle
#' @param classifiers list of classifiers TI (Temperature Increasing), NI (Nutrient Increasing), SI (Salt Increasing)
#' @param classifiers_constant constant temperature classifier
#' @param classifiers_increasing increasing temperature classifier
#'
#' @return `morph_mvt` with the classified species and probabilities
#' @export
#'
#' @md
#'
#' @examples
classify_LEEF_2 <- function(
  bemovi_extract,
  morph_mvt,
  trajectory_data,
  classifiers,
  video_description_file,
  composition
){
  bemovi.LEEF::load_parameter(bemovi_extract)

  # 2. Make a list of 8 dataframes: split morph_mvt based on treatments

  morph_mvt_list <- split(
    x = morph_mvt,
    f = morph_mvt$bottle,
    drop = TRUE
  )

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers
  LeefClass <- function(classifier, df, noNAs){
    pr <- predict(classifier, df, probability = TRUE)
    df$species[noNAs] <- as.character(pr) # species prediction
    df$species_probability[noNAs] <- apply(attributes(pr)$probabilities,1,max) # probability of each species prediction
    probabilities <- attributes(pr)$probabilities
    colnames(probabilities) <- paste0(colnames(probabilities),"_prob")

    # in script
    # df <- cbind(df, probabilities)
    # in pipeline
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

    return(df)
  }

  for(i in 1:length(morph_mvt_list)){
    message("      classifying ", i)

    df <- morph_mvt_list[[i]]

    x <- unique(df$temperature) # either "constant" or "increasing"
    if (x == "increasing") {
      TTreat <- "TI"
    } else if (x == "constant") {
      TTreat <- "TC"
    } else {
      stop("Undefined value for `temperature` in experimental design table!")
    }

    x <- unique(df$resources) # either "constant" or "increasing"
    if (x == "increasing") {
      NTreat <- "NI"
    } else if (x == "constant") {
      NTreat <- "NC"
    } else {
      stop("Undefined value for `resources` in experimental design table!")
    }

    x <- unique(df$salinity) # either "constant" or "increasing"
    if (x == "increasing") {
      STreat <- "SI"
    } else if (x == "constant") {
      STreat <- "SC"
    } else {
      stop("Undefined value for `salinity` in experimental design table!")
    }

    classifier_name <- paste(TTreat, NTreat, STreat, sep = "_")

    noNAs <- !rowSums(is.na(df)) > 0
    df <- LeefClass(classifiers[[classifier_name]], df, noNAs)

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


  # begin pipeline only
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
  # end pipeline only

  count_per_frame <- trajectory_data %>%
    group_by(
      file,
      date,
      species,
      bottle,
      resources,
      temperature,
      salinity,
      magnification,
      sample,
      video,
      frame,
      dilution_factor,
      replicate
    ) %>%
    summarise(count = n()) %>%
    mutate(dens.ml = count * bemovi.LEEF::par_extrapolation.factor() * cropping.factor * dilution_factor)

  mean_density_per_ml <- count_per_frame %>%
    group_by(
      date,
      species,
      bottle,
      resources,
      temperature,
      salinity,
      magnification,
      sample,
      replicate
    ) %>%
    summarise(
      numberOfVideos = length(unique(file)),
      density = sum(dens.ml) / (numberOfVideos * 125)
    ) %>%
    mutate(numberOfVideos = NULL)


  # -----------------------------------------------------------------------------------------------------
  # add density = 0 for extinct species ------------------------------------------------------------

  # magnification & cropping specific!


  composition <- composition %>%
    dplyr::select(tidyselect::any_of(bemovi.LEEF::par_species_tracked()))

  composition <- composition[which(composition  == 1)]

  mean_density_per_ml_list <- split(x = mean_density_per_ml,
                                    f = mean_density_per_ml$bottle,
                                    drop = TRUE)

  for(i in seq_along(mean_density_per_ml_list)){
    df <- mean_density_per_ml_list[[i]]
    idx <- which(!is.element(bemovi.LEEF::par_species_tracked(), df$species))
    if(length(idx)==0) next
    for(j in idx){
      new.entry <- tail(df,1)
      new.entry$species <- bemovi.LEEF::par_species_tracked()[j]
      new.entry$density <- 0
      df <- rbind(df, new.entry)
    }
    mean_density_per_ml_list[[i]] <- df
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
