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
#' @importFrom bemovi.LEEF check_video_file_names locate_and_measure_particles link_particles merge_data
#' @importFrom bemovi.LEEF par_to.data par_video.description.folder par_raw.video.folder par_particle.data.folder par_trajectory.data.folder
#' @importFrom bemovi.LEEF par_temp.overlay.folder par_overlay.folder par_merged.data.folder par_ijmacs.folder par_to.particlelinker
#' @importFrom bemovi.LEEF par_memory par_pixel_to_scale par_difference.lag par_thresholds par_min_size
#' @importFrom utils write.table
#' @importFrom parallel mclapply
#' @export

extractor_bemovi_merge <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("Merging bemovi...\n")

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Load bemovi_extract.yml parameter ---------------------------------------
  bemovi.LEEF::load_parameter( file.path(input, "bemovi", "bemovi_extract.yml") )
  bemovi.LEEF::par_IJ.path(file.path( tools_path(), "Fiji.app", "Contents", "MacOS" ) )
  # bemovi.LEEF::par_IJ.path(file.path( tools_path(), "ImageJ.app", "Contents", "MacOS" ) )
  bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "macosx", "adoptopenjdk-8.jdk", "jre", "Contents", "Home", "bin") )
  # bemovi.LEEF::par_java.path( file.path( tools_path(),  "ImageJ.app", "jre", "bin") )
  bemovi.LEEF::par_to.data( file.path(output, "bemovi") )
  bemovi.LEEF::par_to.particlelinker( system.file( package = "LEEF.measurement.bemovi", "ParticleLinker" ) )

# Merge Morphological data and Trajectories in single data.frame ----------

  # Merges the morphology data, the trajectory data with the video descriptions,
  # which can / should contain the information on sampling units, video date and
  # time, treatments and replicate etc. The files are merged by the use of the
  # video file names. For the exact meaning of each of the columns, please refer
  # to the locate_and_measure_particles() and link_particles() functions.
  #
  # RESULT: file.path( merged.data.folder, "Master.rds")

  dir.create(file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()), showWarnings = FALSE)

  processing <- file.path( normalizePath(output), "bemovi", paste0("PROCESSING.MERGING.", "all", ".PROCESSING"))
  error <- file.path(normalizePath(output), "bemovi", paste0("ERROR.MERGING.", "all", ".ERROR"))

  tryCatch(
    {
      file.create(processing)
      if ( length(list.files( file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_particle.data.folder()) ) ) > 0 ) {
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

  message("### Done.")


# Finalize ----------------------------------------------------------------

  file.copy(
    from = file.path(input, "bemovi", "bemovi_extract.yml"),
    to = file.path( output, "bemovi", "bemovi_extract.yml" ),
    overwrite = TRUE
  )

  file.copy(
    from = file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.rds"),
    to = file.path( output, "bemovi", "Master.rds" ),
    overwrite = TRUE
  )


  message("\ndone\n")
  message("\n########################################################\n")

  invisible(TRUE)
}