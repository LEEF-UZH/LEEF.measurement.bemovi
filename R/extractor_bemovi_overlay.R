#' Extractor bemovi data trajectories
#'
#' This function is calculating the trajectories
#'
#' @param input only for compatibility - not used
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

extractor_bemovi_overlay <- function(
  input,
  output
) {

  message("\n########################################################\n")
  message("Creating overlays bemovi...\n")

  processing <- file.path(normalizePath(output), "bemovi", "CREATING.OVERLAYS.CREATING")
  error <- file.path(normalizePath(output), "bemovi", "ERROR.OVERLAYS.ERROR")

  old_ijmacs.folder <- bemovi.LEEF::par_ijmacs.folder()

  on.exit(
    {
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
      }
      bemovi.LEEF::par_ijmacs.folder(old_ijmacs.folder)
    }
  )

  ##
  file.create( processing )

  # Load bemovi_extract.yml parameter ---------------------------------------
  bemovi.LEEF::load_parameter( file.path(output, "bemovi", "bemovi_extract.yml") )
  bemovi.LEEF::par_ijmacs.folder(bemovi.LEEF::par_temp.overlay.folder())

  # Paths for different OS
  switch(
    Sys.info()['sysname'],
    Darwin = {
      bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "macosx",      "adoptopenjdk-8.jdk",  "jre", "Contents", "Home", "bin") )
      bemovi.LEEF::par_IJ.path( file.path( tools_path(),    "Fiji.app", "Contents", "MacOS" ) )
    },
    Windows = {
      bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "win64",       "jdk1.8.0_172", "jre", "bin") )
      bemovi.LEEF::par_IJ.path( file.path( tools_path(),    "Fiji.app" ) )
    },
    Linux = {
      bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "linux-amd64", "jdk1.8.0_172", "jre", "bin" ) )
      bemovi.LEEF::par_IJ.path( file.path( tools_path(),    "Fiji.app" ) )
    },
    stop("OS not supported by bemovi!")
  )

  bemovi.LEEF::par_to.particlelinker( system.file( package = "LEEF.measurement.bemovi", "ParticleLinker" ) )


# Create temporary directory structure ------------------------------------

  tmpdir <- tempfile()
  dir.create(tmpdir)



  # Create overlays ---------------------------------------------------------

  bemovi.LEEF::create_overlays(
    to.data = file.path(output, "bemovi"),
    difference.lag = bemovi.LEEF::par_difference.lag(),
    type = "label",
    predict_spec = "species",
    contrast.enhancement = 1.0,
  )

  # Copy results to output --------------------------------------------------





  # Finalize ----------------------------------------------------------------

  message("\ndone\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
