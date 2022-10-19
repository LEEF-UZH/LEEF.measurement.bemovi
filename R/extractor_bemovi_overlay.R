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

  # Load bemovi_extract.yml parameter ---------------------------------------
  bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))
  bemovi.LEEF::par_mc.cores(getOption("mc.cores", 1))

  processing <- file.path(normalizePath(output), "bemovi", "CREATING.OVERLAYS.CREATING")
  error <- file.path(normalizePath(output), "bemovi", "ERROR.OVERLAYS.ERROR")

  on.exit({
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
      }
    }
  )

  ##
  file.create(processing)

  # Create overlays ---------------------------------------------------------

  bemovi.LEEF::create_overlays_subtitle(
    to.data = file.path(output, "bemovi"),
    raw.video.folder = file.path(input, "bemovi"),  # "1.pre-processed.data/bemovi/",
    overlay.type = "label",
    label = "species",
    ffmpeg = file.path(tools_path(), "ffmpeg"),
    mc.cores = bemovi.LEEF::par_mc.cores()
  )

  # Finalize ----------------------------------------------------------------

  message("   END overlay")
  message("########################################################")

  invisible(TRUE)
}
