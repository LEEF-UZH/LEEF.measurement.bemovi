

.onLoad <- function(lib, pkg) {
  opt <- list(
    debug = FALSE,
    tools_path = "./tools"
  )
  options(LEEF.measurement.bemovi = opt)
}
