

.onLoad <- function(lib, pkg) {
  opt <- list(
    debug = FALSE,
    tools_path = "./tools"
  )
  options(LEEF.measurement.bemovi = opt)
}

utils::globalVariables(
	c(
		"count",
		"dens.ml",
		"frame",
		"id",
		"magnification",
		"microcosm.nr",
		"predict_spec",
		"species.composition",
		"temperature",
		"video"
	)
)