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
#' @importFrom bemovi.LEEF par_to.data par_video.description.folder par_raw.video.folder
#' @importFrom bemovi.LEEF par_particle.data.folder par_trajectory.data.folder
#' @importFrom bemovi.LEEF par_temp.overlay.folder par_overlay.folder par_merged.data.folder
#' @importFrom bemovi.LEEF par_ijmacs.folder par_to.particlelinker
#' @importFrom bemovi.LEEF par_memory par_pixel_to_scale par_difference.lag par_thresholds par_min_size
#' @importFrom utils write.table
#' @importFrom parallel mclapply
#' @import loggit
#' @export

extractor_bemovi_overlay <- function(
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
  message("   BEGIN overlay bemovi...")

  processing <- file.path(normalizePath(output), "bemovi", "CREATING.OVERLAYS.CREATING")
  error <- file.path(normalizePath(output), "bemovi", "ERROR.OVERLAYS.ERROR")

  old_ijmacs.folder <- bemovi.LEEF::par_ijmacs.folder()

  on.exit({
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
      }
      bemovi.LEEF::par_ijmacs.folder(old_ijmacs.folder)
    }
  )

  ##
  file.create(processing)

  # Load bemovi_extract.yml parameter ---------------------------------------

  bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))
  bemovi.LEEF::par_ijmacs.folder(bemovi.LEEF::par_temp.overlay.folder())


  # Create overlays ---------------------------------------------------------


  bemovi.LEEF::create_overlays(
    to.data = file.path(output, "bemovi"),
    raw.video.folder = "1.pre-processed.data/bemovi/",
    difference.lag = bemovi.LEEF::par_difference.lag(),
    type = "label",
    predict_spec = "species",
    ffmpeg = file.path(tools_path(), "ffmpeg")
  )


  # Finalize ----------------------------------------------------------------

  message("   END overlay")
  message("########################################################")

  invisible(TRUE)
}
