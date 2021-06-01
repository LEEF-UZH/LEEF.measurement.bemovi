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

extractor_bemovi_trajectory <- function(
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
  message("   trajectories bemovi...")

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )


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
    stop("OS not supported by bemovi!")
  )

  bemovi.LEEF::par_to.particlelinker(system.file(package = "LEEF.measurement.bemovi", "ParticleLinker"))


# Identify Trajectories ---------------------------------------------------

  # The function takes the XY-coordinates provided by the ImageJ
  # ParticleAnalyzer and uses a standalone version of the ImageJ MOSAIC plugin
  # ParticleLinker to create trajectories. This requires some creation of
  # temporary files, which are subsequently deleted.
  #
  # RESULT: file.path( trajectory.data.older, "trajectory.rds")

  dir.create(file.path(output, "bemovi", bemovi.LEEF::par_trajectory.data.folder()), showWarnings = FALSE)

  message("### Identify trajectories ...")

  ijouts <- list.files(
    path = file.path(output, "bemovi", bemovi.LEEF::par_particle.data.folder()),
    pattern = "\\.ijout\\.txt",
    full.names = TRUE
  )

  if (length(ijouts) > 0) {
    message("      PARALLEL BEGIN")
    parallel::mclapply(
      # lapply(
      ijouts,
      function(ijout) {
        message("      processing ", ijout)

        processing <- file.path(
          normalizePath(output), "bemovi",
          paste0("CALCULATING.TRAJECTORIES.", basename(ijout), ".PROCESSING")
        )
        error <- file.path(
          normalizePath(output), "bemovi",
          paste0("ERROR.TRAJECTORIES.", basename(ijout), ".ERROR")
        )
        on.exit({
          if (file.exists(processing)) {
            unlink(processing)
            file.create(error)
            message("      ERROR trajectories ", ijout)
          }
        })
        ##
        file.create(processing)
        # Define and create temporary folder structure -------------------------------------------------

        bemovi.LEEF::par_to.data(tempfile(pattern = "bemovi."))

        bemovi.LEEF::Create_folder_structure()
        file.symlink(
          from = normalizePath(ijout),
          to = file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_particle.data.folder())
        )

        bemovi.LEEF::link_particles(start_vid = 1)
        ##
        outfile <- file.path(paste0("ParticleLinker_", basename(ijout)))
        if (file.exists(file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_trajectory.data.folder(), outfile))) {
          file.copy(
            from = file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_trajectory.data.folder(), outfile),
            to = file.path(output, "bemovi", bemovi.LEEF::par_trajectory.data.folder(), outfile)
          )
        } else {
          file.create(error)
        }

        ##
        unlink(bemovi.LEEF::par_to.data(), recursive = TRUE)
        unlink(processing)
        message("      done ", ijout)
      },
      mc.preschedule = FALSE
    )
    message("      PARALLEL END")
  }


  # Combine outputs ---------------------------------------------------------

  message("   combining outpute")
  processing <- file.path(
    normalizePath(output), "bemovi",
    paste0("PROCESSING.MERGING.", "particleLinker", ".PROCESSING")
  )
  error <- file.path(normalizePath(output), "bemovi", paste0("ERROR.MERGING.", "particleLinker", ".ERROR"))

  bemovi.LEEF::par_to.data(file.path(output, "bemovi"))

  tryCatch({
      file.create(processing)
      bemovi.LEEF::calculate_mvt(
        bemovi.LEEF::organise_link_data()
      )
      unlink(processing)
    },
    finally = {
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
        stop("error in merging trajectories")
      }
    }
  )

  # Finalize ----------------------------------------------------------------

  message("   done")
  message("########################################################")

  invisible(TRUE)
}
