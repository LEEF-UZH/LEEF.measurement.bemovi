#' Extractor bemovi to id species and calculate density
#'
#' This function id's the species and calculates the densities.
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @import bemovi.LEEF
#' @importFrom tools file_path_sans_ext
#' @importFrom stats predict
#' @importFrom data.table as.data.table setkey as.data.table
#' @importFrom dplyr group_by summarise mutate n filter full_join
#' @importFrom purrr reduce
#' @importFrom tidyselect any_of
#' @importFrom utils read.csv tail
#' @import e1071
#' @import loggit
#'
#' @export

extractor_bemovi_classify <- function(
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
  message("   BEGIN classify bemovi...")

  processing <- file.path(normalizePath(output), "bemovi", "PROCESSING.CLASSIFY.SPECIES.PROCESSING")
  error <- file.path(normalizePath(output), "bemovi", "ERROR.CLASSIFY.SPECIES.ERROR")
  on.exit({
    if (file.exists(processing)) {
      unlink(processing)
      file.create(error)
      message("   ERROR classify bemovi")
      message("   END classify bemovi")
    }
  })

  ##
  file.create(processing)


  bemovi.LEEF::load_parameter(file.path(output, "bemovi", "bemovi_extract.yml"))


  # Read classifiers into list ----------------------------------------------


  dir_classifiers <- file.path(normalizePath(output), "bemovi", par_classifiers())

  class_files <- list.files(dir_classifiers, pattern = "\\.rds$", full.names = TRUE)
  classifiers <- lapply(
    class_files,
    readRDS
  )
  names(classifiers) <- tools::file_path_sans_ext(basename(class_files))
  classifiers$comment <- readLines(file.path(dir_classifiers, "README.txt"))


  # Classify ----------------------------------------------------------------


  classified <- classify_LEEF_2(
    bemovi_extract = file.path(output, "bemovi", "bemovi_extract.yml"),
    morph_mvt = readRDS(file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_morph_mvt())),
    trajectory_data = readRDS(file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder(), bemovi.LEEF::par_master())),
    classifiers_constant = readRDS(file.path(output, "bemovi", bemovi.LEEF::par_classifier_constant())),
    classifiers_increasing = readRDS(file.path(output, "bemovi", bemovi.LEEF::par_classifier_increasing())),
    video_description_file = as.data.frame(read.table(file.path(input, "bemovi", bemovi.LEEF::par_video.description.file()), sep = "\t", header = TRUE, stringsAsFactors = FALSE)),
    composition = utils::read.csv(file.path(output, "bemovi", "compositions.csv"))
  )

  # Script end --------------------------------------------------------------

  bemovi.LEEF::par_to.data(file.path(output, "tmp.bemovi"))
  bemovi.LEEF::Create_folder_structure()

  outfiles <- c(
    morph_file = file.path(
      bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_morph_mvt()
    ),
    traj.filtered_file = file.path(
      bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_master()
    ),
    mean.dens_file = file.path(
      bemovi.LEEF::par_to.data(), bemovi.LEEF::par_merged.data.folder(),
      bemovi.LEEF::par_mean_density()
    )
  )

  saveRDS(
    classified$morph_mvt,
    file = outfiles["morph_file"]
  )
  saveRDS(
    # trajectory.data.filtered,
    classified$trajectory_data,
    file = outfiles["traj.filtered_file"]
  )
  saveRDS(
    classified$mean_density_per_ml,
    file = outfiles["mean.dens_file"]
  )

  # Finalize ----------------------------------------------------------------

  if (all(file.exists(outfiles))) {
    file.copy(
      from = file.path(outfiles),
      to = file.path(output, "bemovi", bemovi.LEEF::par_merged.data.folder()),
      overwrite = TRUE
    )
  } else {
    file.create(error)
  }

  unlink(bemovi.LEEF::par_to.data(), recursive = TRUE)
  unlink(processing)

  message("   END classify bemovi")
  message("########################################################")

  invisible(TRUE)
}
