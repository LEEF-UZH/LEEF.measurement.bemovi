#' Extractor bemovi to filter traced particles
#'
#' This function applies a filter.
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @import bemovi.LEEF
#' @importFrom  magrittr %>%
#' @import loggit
#'
#' @export

extractor_bemovi_filter <- function(
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
  message("   BEGIN filtering bemovi...")

  dir.create(
    file.path(output, "bemovi"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Load bemovi_extract.yml parameter ---------------------------------------

  bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))
  bemovi.LEEF::par_mc.cores(getOption("mc.cores"))

  # Filter Particles Particles --------------------------------------------

  dir.create(file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()), showWarnings = FALSE)

  processing <- file.path(normalizePath(output), "bemovi", "PROCESSING.FILTERING.PROCESSING")
  error <- file.path(normalizePath(output), "bemovi", "ERROR.FILTERING.ERROR")
  on.exit({
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
        message("   ERROR filtering bemovi")
        message("   END filtering bemovi")
      }
    }
  )

  ##
  file.create(processing)

  # Define and create temporary folder structure -------------------------------------------------

  bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))
  bemovi.LEEF::par_mc.cores(getOption("mc.cores"))
  bemovi.LEEF::par_to.data(file.path(output, "tmp.bemovi"))
  bemovi.LEEF::Create_folder_structure()

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

  # Load Data ---------------------------------------------------------------

  master <- file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_master())
  dir.create(file.path(bemovi.LEEF::par_to.data(), "6 - merged data unfiltered"))
  file.copy(
    from = master,
    to = file.path(bemovi.LEEF::par_to.data(), "6 - merged data unfiltered")
  )

  trajectory.data.unfiltered <- readRDS(master)
  trajectory.data.unfiltered$type <- "raw data"
  master <- file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_master())
  saveRDS(trajectory.data.unfiltered, file = master)

  trajectory.data.filtered <- bemovi.LEEF::filter_data(
    raw_data = trajectory.data.unfiltered,
    net_filter = bemovi.LEEF::par_net_filter(),
    duration_filter = bemovi.LEEF::par_duration_filter(),
    detect_filter = bemovi.LEEF::par_detect_filter(),
    median_step_filter = bemovi.LEEF::par_median_step_filter()
  )
  trajectory.data.filtered$type <- "filtered data"
  master.filtered <- file.path(
    bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
    bemovi.LEEF::par_master()
  )
  saveRDS(trajectory.data.filtered, file = master.filtered)


  # -----------------------------------------------------------------------------------------------------
  # check if trajectories are tracked and that filter settings are correct ------------------------------
  # Not necessary in final script.
  # -----------------------------------------------------------------------------------------------------
  # summarize trajectories ------------------------------------------------------------------------------
  # morph_mvt contains descriptives of each tracked particle averaged (or median) over all frames it
  # is tracked

  morph_mvt <- bemovi.LEEF::summarize_trajectories(
    data = trajectory.data.filtered,
    write = TRUE,
    calculate.median = FALSE
    # to.data,
    # merged.data.folder
  )

  # file.rename(
  #   from = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.rds" ),
  #   to = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.F1.rds" )
  # )

  # Filter data (2nd time) --------------------------------------------------

  min_area <- bemovi.LEEF::par_min_area()
  max_area <- bemovi.LEEF::par_max_area()

  index <- which((morph_mvt$mean_area > min_area) & (morph_mvt$mean_area < max_area)) # change!
  ids <- morph_mvt$id[index]

  trajectory.data.filtered <- trajectory.data.filtered %>%
    subset(id %in% ids)

  # -----------------------------------------------------------------------------------------------------
  # summarize trajectories again ------------------------------------------------------------------------

  morph_mvt <- bemovi.LEEF::summarize_trajectories(
    data = trajectory.data.filtered,
    write = TRUE,
    calculate.median = FALSE
    # to.data,
    # merged.data.folder
  )

  # file.rename(
  #   from = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.rds" ),
  #   to = file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.F2.rds" )
  # )

  # Finalize ----------------------------------------------------------------

  outfiles <- c(
    file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_master()),
    # # file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Master.filtered.rds"),
    # file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.F1.rds" ),
    # file.path( bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), "Morph_mvt.F2A.rds" ),
    file.path(bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_morph_mvt())
  )


  if (all(file.exists(outfiles))) {
    file.copy(
      from = file.path(outfiles),
      to = file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()),
      overwrite = TRUE
    )
    file.copy(
      from = file.path(bemovi.LEEF::par_to.data(), "6 - merged data unfiltered", ""),
      to = file.path(output, "bemovi"),
      recursive = TRUE,
      overwrite = TRUE
    )
  } else {
    file.create(error)
  }

  unlink(bemovi.LEEF::par_to.data(), recursive = TRUE)
  unlink(processing)

  message("   END filtering bemovi")
  message("########################################################")

  invisible(TRUE)
}
