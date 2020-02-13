

.onLoad <- function(lib, pkg) {
  opt <- list(
    debug = FALSE,
    tools_path = system.file( package = "LEEF.measurement.bemovi", "tools" )
  )
  options(LEEF.measurement.bemovi = opt)
}
