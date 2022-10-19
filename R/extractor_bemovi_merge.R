#' Extractor bemovi data
#'
#' Analyse all \code{.avi} files in \code{bemovi} folder and save as \code{.rds} file.
#'
#' This function is extracting data to be added to the database
#' (and therefore make accessible for further analysis and forecasting)
#' from \code{.avi} files.
#'
#' @param input only for compatibility - not used
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom bemovi.LEEF check_video_file_names locate_and_measure_particles link_particles merge_data
#' @importFrom bemovi.LEEF par_to.data par_video.description.folder par_raw.video.folder
#' @importFrom bemovi.LEEF par_particle.data.folder par_trajectory.data.folder
#' @importFrom bemovi.LEEF par_temp.overlay.folder par_overlay.folder par_merged.data.folder
#' @importFrom bemovi.LEEF par_ijmacs.folder par_to.particlelinker
#' @importFrom bemovi.LEEF par_memory par_pixel_to_scale par_difference.lag par_thresholds par_min_size
#' @importFrom utils write.table
#' @import loggit
#' @export

extractor_bemovi_merge <- function(
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
  message("   BEGIN merging bemovi...")


  # Load bemovi_extract.yml parameter ---------------------------------------

  bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))

  # Paths for different OS
  switch(Sys.info()["sysname"],
    Darwin = {
      bemovi.LEEF::par_java.path(
        file.path(tools_path(), "Fiji.app", "java", "macosx", "adoptopenjdk-8.jdk", "jre", "Contents", "Home", "bin")
      )
      bemovi.LEEF::par_IJ.path(file.path(tools_path(), "Fiji.app", "Contents", "MacOS"))
    },
    Windows = {
      bemovi.LEEF::par_java.path(
        file.path(tools_path(), "Fiji.app", "java", "win64", "jdk1.8.0_172", "jre", "bin")
      )
      bemovi.LEEF::par_IJ.path(file.path(tools_path(), "Fiji.app"))
    },
    Linux = {
      bemovi.LEEF::par_java.path(
        file.path(tools_path(), "Fiji.app", "java", "linux-amd64", "jdk1.8.0_172", "jre", "bin")
      )
      bemovi.LEEF::par_IJ.path(file.path(tools_path(), "Fiji.app"))
    },
    stop("OS not supported by bemoviu!")
  )

  bemovi.LEEF::par_to.data(file.path(output, "bemovi"))
  bemovi.LEEF::par_to.particlelinker(system.file(package = "LEEF.measurement.bemovi", "ParticleLinker"))

# Merge Morphological data and Trajectories in single data.frame ----------

  # Merges the morphology data, the trajectory data with the video descriptions,
  # which can / should contain the information on sampling units, video date and
  # time, treatments and replicate etc. The files are merged by the use of the
  # video file names. For the exact meaning of each of the columns, please refer
  # to the locate_and_measure_particles() and link_particles() functions.
  #
  # RESULT: file.path( merged.data.folder, "Master.rds")

  dir.create(file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()), showWarnings = FALSE)

  processing <- file.path(normalizePath(output), "bemovi", paste0("PROCESSING.MERGING.", "all", ".PROCESSING"))
  error <- file.path(normalizePath(output), "bemovi", paste0("ERROR.MERGING.", "all", ".ERROR"))

  tryCatch({
      file.create(processing)
      if (
            length(list.files(file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_particle.data.folder()))) > 0
          ) {
        bemovi.LEEF::merge_data()
      }
      unlink(processing)
    },
    finally = {
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
        stop("error in merging all together")
      }
    }
  )


  # Finalize ----------------------------------------------------------------

  # Merge experimental design in --------------------------------------------

  master <- readRDS(file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.rds"))
  exp_design <- read.csv(file.path(input, "bemovi", "experimental_design.csv"))
  master <- merge(master, exp_design, by.x = "bottle", by.y = "bottles", all.x = TRUE, all.y = FALSE, suffixes = c(".video_descr", ""))
  saveRDS(master, file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.rds"))

  # file.copy(
  #   from = file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.rds"),
  #   to = file.path( output, "bemovi", "Master.rds" ),
  #   overwrite = TRUE
  # )


  message("   END merging bemovi")
  message("########################################################")

  invisible(TRUE)
}
