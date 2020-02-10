#' Register the processing of bemovi data in the LEEF.Data package
#'
#' @return invisibly \code{TRUE} when completed successful
#' @export
#'
register <- function() {
  if (is.null(system.file(package = "LEEF.Data"))) {
    stop("This function requres the package to be installed!")
  }
  LEEF.Data::add_pre_processor( pre_processor_bemovi )
  LEEF.Data::add_extractor( extractor_bemovi )

  invisible(TRUE)
}

