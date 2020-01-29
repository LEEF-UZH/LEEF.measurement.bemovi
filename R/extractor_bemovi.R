#' Extractor bemovi data
#'
#' Analyse all \code{.avi} files in \code{bemovi} folder and save as \code{.rds} file.
#'
#' This function is extracting data to be added to the database (and therefore make accessible for further analysis and forecasting)
#' from \code{.avi} files.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom bemovi check_video_file_names locate_and_measure_particles link_particles merge_data
#' @importFrom bemovi par_to.data par_video.description.folder par_raw.video.folder par_particle.data.folder par_trajectory.data.folder
#' @importFrom bemovi par_temp.overlay.folder par_overlay.folder par_merged.data.folder par_ijmacs.folder par_to.particlelinker
#' @importFrom bemovi par_memory par_pixel_to_scale par_difference.lag par_thresholds par_min_size
#' @importFrom utils write.table
#' @export

extractor_bemovi <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("Extracting bemovi...\n")

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Get avi file names ------------------------------------------------------

  bemovi_path <- file.path( input, "bemovi" )
  bemovi_path <- gsub("xxx", "", bemovi_path )
  bemovi_files <- list.files(
    path = bemovi_path,
    pattern = "*.avi",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(bemovi_files) == 0) {
    message("nothing to extract\n")
    message("\n########################################################\n")
    return(invisible(FALSE))
  }


# Load bemovi_extract.yml parameter ---------------------------------------

  bemovi::load_parameter( file.path(input, "bemovi", "bemovi_extract.yml") )
  bemovi::par_IJ.path( system.file(package = "LEEF.bemovi", "tools", "Fiji.app", "Contents", "MacOS" ) )
  bemovi::par_to.data( file.path(output, "bemovi") )
  bemovi::par_to.particlelinker( system.file(package = "LEEF.bemovi", "tools", "ParticleLinker" ) )

# Define and create temporary folder structure -------------------------------------------------

  bemovi::par_to.data( tempfile( pattern = "bemovi.") )

  bemovi::Create_folder_structure()
  file.symlink(
    from = normalizePath(bemovi_files),
    to = file.path( bemovi::par_to.data(), bemovi::par_raw.video.folder() )
  )

# Create video.description.file -------------------------------------------

  nbm <- basename(bemovi_files)
  nbm <- gsub(".cxd", "", nbm)
  nbm <- gsub(".avi", "", nbm)
  nbm <- data.frame( file = nbm )
  utils::write.table(
    x = nbm,
    file = file.path(bemovi::par_to.data(), bemovi::par_video.description.folder(), bemovi::par_video.description.file()),
    row.names = FALSE
  )

# Check Folder structure --------------------------------------------------

  # Checks the files in the raw data for the supported avi and cxd file formats and that file names do not
  # contain periods except before the file type extension
  #
  # RESULT: returns an error message and a list with unsupported files or names
  bemovi::check_video_file_names()


# Identify moving particles and extract morphological data ----------------

  # Function calls ImageJ software and its ParticleAnalyzer function to extract
  # for each frame of the video several morphological descriptors and the X- and
  # Y-coordinates of all moving particles. All videos in the raw.video.folder
  # are analyses, separately.
  #
  # RESULT: file.path( particle.data.older, "particle.rds)
  bemovi::locate_and_measure_particles()


# Identify Trajectories ---------------------------------------------------

  # The function takes the XY-coordinates provided by the ImageJ
  # ParticleAnalyzer and uses a standalone version of the ImageJ MOSAIC plugin
  # ParticleLinker to create trajectories. This requires some creation of
  # temporary files, which are subsequently deleted.
  #
  # RESULT: file.path( trajectory.data.older, "trajectory.rds")
  bemovi::link_particles( start_vid = 1 )


# Merge Morphological data and Trajectories in single data.frame ----------

  # Merges the morphology data, the trajectory data with the video descriptions,
  # which can / should contain the information on sampling units, video date and
  # time, treatments and replicate etc. The files are merged by the use of the
  # video file names. For the exact meaning of each of the columns, please refer
  # to the locate_and_measure_particles() and link_particles() functions.
  #
  # RESULT: file.path( merged.data.folder, "Master.rds")
  bemovi::merge_data()


#   stop("Until here and no further!!!")
# # Filter trajectory data --------------------------------------------------
#
#   trajectory.data <- readRDS(file.path(to.data,merged.data.folder,"Master.rds"))
#
#   trajectory.data.filtered <- bemovi::filter_data(
#     raw_data = trajectory.data,
#     net_filter = 50,
#     duration_filter = 1,
#     detect_filter = 0.1,
#     median_step_filter = 5
#   )
#   trajectory.data.filtered$type <- "filtered data"
#   trajectory.data$type <- "raw data"
#
#   morph_mvt <- bemovi::summarize_trajectories(
#     data = trajectory.data.filtered,
#     write = FALSE,
#     calculate.median = FALSE,
#     to.data = to.data,
#     merged.data.folder = merged.data.folder
#   )
#
#   predict.data <- morph_mvt
#   trajectory_data_filt_month <- trajectory.data.filtered
#
#   rm(trajectory.data, trajectory.data.filtered)
#
#   #### load in classification data ----
#
#   to.data <- "/Users/davidinauen/Documents/UZH/Master/Data/bemovi_training_data/25/"
#   load(paste0(to.data,merged.data.folder,"morph_mvt.rds"))
#   load(paste0(to.data,merged.data.folder,"Master.rds"))
#   trajectory.data.filtered <- filter_data(trajectory.data,50,1,0.1,5)
#   trajectory.data.filtered$type <- "filtered data"
#   trajectory.data$type <- "raw data"
#
#   ## support vector machine approach
#   #target <- c(mean_area,sd_area, mean_perimeter, sd_perimeter, mean_major, sd_major, mean_minor, sd_minor, mean_ar, sd_ar, sd_turning, gross_speed, sd_gross_speed, max_gross_speed, min_gross_speed)
#
#   training.data <- morph_mvt
#   trajectory_data_filt_training <- trajectory.data.filtered
#   rm(morph_mvt, trajectory.data.filtered, trajectory.data)
#
#
#   training.data.model <- training.data
#   training.data.model$species <- as.factor(training.data.model$species)
#   # using the parameters used by pennekamp et al. 2016
#   training.data.model <- training.data.model %>% select(mean_area,sd_area, mean_perimeter, sd_perimeter, mean_major, sd_major, mean_minor, sd_minor, mean_ar, sd_ar, sd_turning, gross_speed, sd_gross_speed, max_gross_speed, min_gross_speed, species)
#
#   # building the model using the polynomial kernel 0.1*x1.x2 + 1)^8
#   svm.model <- svm(species ~ mean_area + sd_area + mean_perimeter + sd_perimeter + mean_major + sd_major + mean_minor + sd_minor + mean_ar + sd_ar + sd_turning + gross_speed + sd_gross_speed + max_gross_speed + min_gross_speed, data = training.data.model, kernel = "polynomial", degree = 8, gamma = 0.1, coef0=1)
#
#   # checking the performance of the prediction
#
#   svm.pred <- predict(svm.model, training.data[,-16])
#   training.data$predict_spec <- svm.pred
#   table(pred = svm.pred, true = training.data[,32]) # doesnt seem to be too bad
#
#   # now predicting for the month
#   svm.pred <- predict(svm.model, predict.data[,-32])
#   table(pred = svm.pred, true = predict.data[,32]) # well now way to check here, check with random forest
#   predict.data$predict_spec <- as.factor(svm.pred)
#
#   morph_mvt <- predict.data
#   take_all <- as.data.table(morph_mvt)
#   take_all <- take_all[, list(id, predict_spec)]
#   setkey(take_all, id)
#   setkey(trajectory_data_filt_month, id)
#   data <- trajectory_data_filt_month[take_all]
#
#   data$species_predicted <- ifelse(data$predict_spec == "colpidium", "Colpidium",ifelse(data$predict_spec == "paramecium", "Paramecium",ifelse(data$predict_spec == "didinium", "Didinium",ifelse(data$predict_spec == "euplotes", "Euplotes",ifelse(data$predict_spec == "dexiostomata", "Dexiostomata", "unknown")))))
#
#   #### saving the classified bemovi data ----
#
#   final_august <- filter(morph_mvt, species == "pbfe")
#   final_august %>% group_by(predict_spec) %>% summarise(count = length(ID)) # look how many were classified
#
#   setwd("/Users/davidinauen/Documents/UZH/Master/Data/bemovi_video_processing/new_svm_classification")
#   save(final_august, file = "bemovi_svm_classified_august.rds")
#
#   ## create overlays
#
#   setwd("/Users/davidinauen/Documents/UZH/Master/Data/bemovi_video_processing/07_july/25/5 - merged data/")
#   trajectory.data <- data
#   save(trajectory.data, file = "Master.rds")
#
#   to.data <- "/Users/davidinauen/Documents/UZH/Master/Data/bemovi_video_processing/07_july/25/"
#
#
#
#   create_overlays(
#     to.data, merged.data.folder, raw.video.folder,
#     temp.overlay.folder, overlay.folder,
#     2048,
#     2048,
#     difference.lag,
#     type="label",
#     predict_spec=T,
#     IJ.path,
#     contrast.enhancement = 1.3,
#     memory = memory.alloc
#   )
#
#
#
#
#   ## not used in analysis:
#   #### random forest classification ----
#
#   morph_mvt_colp <- morph_mvt[morph_mvt$species == "colpidium" & morph_mvt$net_speed >  50 & morph_mvt$net_disp > 200, ]
#   morph_mvt_eupl <- morph_mvt[morph_mvt$species == "euplotes" & morph_mvt$net_speed > 20 & morph_mvt$net_disp > 100 & morph_mvt$mean_area > 4000, ]
#   morph_mvt_dexi <- morph_mvt[morph_mvt$species == "dexiostomata" & morph_mvt$net_speed > 150 & morph_mvt$net_disp > 500, ]
#   morph_mvt_para <- morph_mvt[morph_mvt$species == "paramecium" & morph_mvt$net_speed > 100 & morph_mvt$net_disp > 150 & morph_mvt$mean_area > 4000, ]
#   morph_mvt_roti <- morph_mvt[morph_mvt$species == "rotifer" & morph_mvt$net_speed > 20 & morph_mvt$net_disp > 20, ]
#   morph_mvt_didi <- morph_mvt[morph_mvt$species == "didinium" & morph_mvt$net_speed > 100 & morph_mvt$net_disp > 100 & morph_mvt$mean_area > 7500, ]
#
#   training_data <- rbind(morph_mvt_colp,morph_mvt_eupl,morph_mvt_para, morph_mvt_dexi, morph_mvt_didi, morph_mvt_roti)
#   rm(morph_mvt_colp,morph_mvt_eupl,morph_mvt_para, morph_mvt_dexi, morph_mvt_didi, morph_mvt_roti)
#
#   training_data <- training_data[complete.cases(training_data),]
#   rf_fit_all <- randomForest(factor(species) ~ mean_area + sd_area + mean_perimeter + sd_perimeter + mean_major + sd_major +  mean_ar + sd_ar + mean_turning + sd_turning + gross_disp + max_net + net_disp + net_speed + max_step + min_step + sd_step + sd_gross_speed + max_gross_speed + min_gross_speed, data=training_data, importance=TRUE, proximity=TRUE)
#
#   print(rf_fit_all$confusion)
#
#
#   morph_mvt$predict_spec <- predict(rf_fit_all, morph_mvt)
#   take_all <- as.data.table(morph_mvt)
#   take_all <- take_all[, list(id, predict_spec)]
#   setkey(take_all, id)
#   setkey(trajectory.data.filtered, id)
#   data <- trajectory.data.filtered[take_all]
#
#   data$species_predicted <- ifelse(data$predict_spec == "colpidium", "Colpidium",ifelse(data$predict_spec == "paramecium", "Paramecium",ifelse(data$predict_spec == "didinium", "Didinium",ifelse(data$predict_spec == "euplotes", "Euplotes",ifelse(data$predict_spec == "dexiostomata", "Dexiostomata", "unknown")))))
#
#
#   #### saving the classified bemovi data ----
#
#   final <- filter(morph_mvt, species == "pbfe")
#   setwd("/Users/davidinauen/Documents/UZH/Master/Data/bemovi_video_processing/morph_mvt_classification/")
#   save(final, file = "bemovi_classified_april.rds")
#
#
#   setwd("/Users/davidinauen/Documents/UZH/Master/Data/bemovi_video_processing/08_july/25/5 - merged data/")
#   trajectory.data <- trajectory.data.filtered
#   save(trajectory.data, file = "Master.rds")
#
#
#   to.data <- "/Users/davidinauen/Documents/UZH/Master/Data/bemovi_video_processing/07_july/25/"
#
#   create_overlays(to.data, merged.data.folder, raw.video.folder,
#                   temp.overlay.folder, overlay.folder,
#                   2048,
#                   2048,
#                   difference.lag,
#                   type="label",
#                   predict_spec=T,
#                   IJ.path,
#                   contrast.enhancement = 1.3,
#                   memory = memory.alloc)
#
#
#
#
#
#
#
#

# SAVE --------------------------------------------------------------------

  add_path <- file.path( output, "bemovi" )
  dir.create( add_path, showWarnings = FALSE )
  file.copy(
    from = c(
      file.path( bemovi::par_to.data(), bemovi::par_particle.data.folder(), "particle.rds"),
      file.path( bemovi::par_to.data(), bemovi::par_trajectory.data.folder(), "trajectory.rds"),
      file.path( bemovi::par_to.data(), bemovi::par_merged.data.folder(), "Master.rds")
    ),
    to = c(
      file.path( add_path, "particle.rds"),
      file.path( add_path, "trajectory.rds"),
      file.path( add_path, "Master.rds")
    )
  )

  bemovi::save_parameter( file.path( add_path, "bemovi_extract.yml") )

# Finalize ----------------------------------------------------------------

  message("\ndone\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
