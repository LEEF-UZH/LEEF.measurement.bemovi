#' Extractor bemovi data particles
#'
#' This function extracts the particles.
#' @param input directory from which to read the data
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

extractor_bemovi_particle <- function(
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
  message("   particle bemovi...")


  # Get avi file names ------------------------------------------------------

  bemovi_path <- file.path(input, "bemovi")
  bemovi_path <- gsub("xxx", "", bemovi_path)
  bemovi_files <- list.files(
    path = bemovi_path,
    pattern = "*.avi",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(bemovi_files) == 0) {
    message("   nothing to extract")
    message("########################################################")
    return(invisible(FALSE))
  }


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

# Locate and Measure Particles --------------------------------------------

  dir.create(file.path(output, "bemovi", bemovi.LEEF::par_particle.data.folder()), showWarnings = FALSE)

  message("      PARALLEL BEGIN")
  parallel::mclapply(
  # lapply(
    bemovi_files,
    function(video) {
      message("      processing ", basename(video))
      processing <- file.path(
        normalizePath(output), "bemovi",
        paste0("CALCULATING.PARTICLE.", basename(video), ".PROCESSING")
      )
      error <- file.path(
        normalizePath(output), "bemovi",
        paste0("ERROR.PARTICLE.", basename(video), ".ERROR")
      )
      on.exit({
          if (file.exists(processing)) {
            unlink(processing)
            file.create(error)
            message("      ERROR particle", basename(video))
          }
        }
      )
      ##
      file.create(processing)
      # Define and create temporary folder structure -------------------------------------------------

      bemovi.LEEF::par_to.data(tempfile(pattern = "bemovi."))

      bemovi.LEEF::Create_folder_structure()
      file.symlink(
        from = normalizePath(video),
        to = file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_raw.video.folder())
      )

      # Copy video.description.file -------------------------------------------

      file.copy(
        from = file.path(
          output, "bemovi", bemovi.LEEF::par_video.description.folder(),
          bemovi.LEEF::par_video.description.file()
        ),
        to = file.path(
          bemovi.LEEF::par_to.data(), bemovi.LEEF::par_video.description.folder(),
          bemovi.LEEF::par_video.description.file()
        )
      )

      # Check Folder structure --------------------------------------------------

      # Checks the files in the raw data for the supported avi and cxd file formats and that file names do not
      # contain periods except before the file type extension
      #
      # RESULT: returns an error message and a list with unsupported files or names
      bemovi.LEEF::check_video_file_names()


      # Identify moving particles and extract morphological data ----------------

      # Function calls ImageJ software and its ParticleAnalyzer function to extract
      # for each frame of the video several morphological descriptors and the X- and
      # Y-coordinates of all moving particles. All videos in the raw.video.folder
      # are analyses, separately.
      #
      # RESULT: file.path( particle.data.older, "particle.rds)

      message("      locate and measure ", basename(video))
      bemovi.LEEF::locate_and_measure_particles()

      outfile <- gsub(".avi", ".ijout.txt", basename(video))
      if (file.exists(file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_particle.data.folder(), outfile))) {
        file.copy(
          from = file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_particle.data.folder(), outfile),
          to = file.path(output, "bemovi", bemovi.LEEF::par_particle.data.folder(), outfile)
        )

        # Delete input video ------------------------------------------------------

        # unlink(video)
      } else {
        file.create(error)
      }
      unlink(bemovi.LEEF::par_to.data(), recursive = TRUE)
      unlink(processing)
      message("      done ", basename(video))
    },
    mc.preschedule = FALSE
  )
  message("      PARALLEL END")



# Combine outputs ---------------------------------------------------------

  processing <- file.path(normalizePath(output), "bemovi", paste0("PROCESSING.MERGING.", "particle", ".PROCESSING"))
  error <- file.path(normalizePath(output), "bemovi", paste0("ERROR.MERGING.", "particle", ".ERROR"))

  bemovi.LEEF::par_to.data(file.path(output, "bemovi"))

  tryCatch({
      file.create(processing)
      bemovi.LEEF::organise_particle_data()
      unlink(processing)
    },
    finally = {
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
        stop("error in merging particles")
      }
    }
  )

# Finalize ----------------------------------------------------------------

  message("   done")
  message("########################################################")

  invisible(TRUE)
}
