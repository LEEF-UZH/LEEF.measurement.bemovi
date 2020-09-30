#' Checks tools_path() if all tools are there or downloads them
#'
#' @param path path for the tools. defaults to \code{tools_path()}
#' @param download if \code{TRUE}, the tools are downloaded. If \code{FALSE},
#'   the default, the ptools path is only checked.
#'
#' @return \code{TRUE} if all tools are installed. a named list of tools which re not installed.
#' @importFrom utils unzip download.file
#' @export
#'
#' @examples
#' check_tools_path()
#'
check_tools_path <- function(
  path = tools_path(),
  download = FALSE
) {

  result <- list()

# We need it below --------------------------------------------------------

  # Paths for different OS
  switch(
    Sys.info()['sysname'],
    Darwin = {
      bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "macosx",      "adoptopenjdk-8.jdk",  "jre", "Contents", "Home", "bin") )
      bemovi.LEEF::par_IJ.path( file.path( tools_path(),    "Fiji.app", "Contents", "MacOS" ) )
    },
    Windows = {
      bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "win64",       "jdk1.8.0_172", "jre", "bin") )
      bemovi.LEEF::par_IJ.path( file.path( tools_path(),    "Fiji.app" ) )
    },
    Linux = {
      bemovi.LEEF::par_java.path( file.path( tools_path(),  "Fiji.app", "java", "linux-amd64", "jdk1.8.0_172", "jre", "bin" ) )
      bemovi.LEEF::par_IJ.path( file.path( tools_path(),    "Fiji.app" ) )
    },
    stop("OS not supported by bemoviu!")
  )

# tools dir ---------------------------------------------------------------

  tools <- file.path( path )
  message( "### checking tools path '", tools, " ###" )
  if (!file.exists( tools )) {
    if (download) {
      dir.create( tools, showWarnings = FALSE, recursive = TRUE)
    } else {
      warning(
        "Tools directory does not exist at '", tools, "'.\n",
        "  To create it, run the command\n",
        "    `check_tools_dir`(download = TRUE)`\n",
        "### end checking ###"
      )
      result$tools.path <- FALSE
    }
  }

# pre_processor - bfconvert -----------------------------------------------------------

  bfconvert <- file.path( path, "bftools", "bfconvert" )
  message( "### checking path to bfconvert '", bfconvert, " ###" )
  if (!file.exists( bfconvert )) {
    if (download) {
      utils::download.file(
        url = "http://downloads.openmicroscopy.org/latest/bio-formats5.6/artifacts/bftools.zip",
        destfile = file.path(path, "bftools.zip"),
        mode = "wb"
      )
      message("Extracting...")
      utils::unzip(
        zipfile = file.path(path, "bftools.zip"),
        exdir = file.path( path )
      )

      Sys.chmod(
        paths = list.files( file.path( path, "bftools"), full.names = TRUE),
        mode = "555"
      )

      unlink(file.path(path, "bftools.zip"))
    } else {
      warning(
        "File bfconvert does not exist at '", bfconvert, "'.\n",
        "  To download, run the command\n",
        "    `check_tools_dir`(download = TRUE)`\n",
        "### end checking ###"
      )
      result$bfconvert <- FALSE
    }
  }

  # pre_processor - ffmpeg -----------------------------------------------------------

  ffmpeg <- file.path( path, "ffmpeg" )
  message( "### checking path to ffmpeg '", ffmpeg, " ###" )
  if (!file.exists( ffmpeg )) {
    if (download) {

      switch(
        Sys.info()['sysname'],
        Darwin = {
          utils::download.file(
            url = "https://evermeet.cx/ffmpeg/ffmpeg-4.3.1.zip",
            destfile = file.path(path, "ffmpeg.zip"),
            mode = "wb"
          )
          message("Extracting...")
          x <- utils::unzip(
            zipfile = file.path(path, "ffmpeg.zip"),
            exdir = file.path( path )
          )
          Sys.chmod(
            paths = file.path( path, "ffmpeg"),
            mode = "555"
          )
          unlink(file.path(path, "ffmpeg.zip"))
        },
        Windows = {
          utils::download.file(
            url = "https://github.com/BtbN/FFmpeg-Builds/releases/download/autobuild-2020-09-20-14-26/ffmpeg-n4.3.1-17-gdae6d75a31-win64-gpl-4.3.zip",
            destfile = file.path(path, "ffmpeg.zip"),
            mode = "wb"
          )
          message("Extracting...")
          x <- utils::unzip(
            zipfile = file.path(path, "ffmpeg.zip"),
            exdir = file.path( path )
          )
          Sys.chmod(
            paths = file.path( path, "ffmpeg"),
            mode = "555"
          )
          unlink(file.path(path, "ffmpeg.zip"))
        },
        Linux = {
          utils::download.file(
            url = "https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz",
            destfile = file.path(path, "ffmpeg.tar.xz"),
            mode = "wb"
          )
          message("Extracting...")
          x <- utils::untar(
            tarfile = file.path(path, "ffmpeg.tar.xz"),
            files = file.path("ffmpeg-4.3.1-amd64-static", "ffmpeg"),
            exdir = file.path( path )
          )
          file.copy(
            from = file.path( path, "ffmpeg-4.3.1-amd64-static", "ffmpeg"),
            to = file.path( path, "ffmpeg" )
          )
          Sys.chmod(
            paths = file.path( path, "ffmpeg"),
            mode = "555"
          )
          unlink(file.path(path, "ffmpeg-4.3.1-amd64-static"), recursive = TRUE)
          unlink(file.path(path, "ffmpeg.tar.xz"))
        },
        stop("OS not supported by Fiji!")
      )
    } else {
      warning(
        "File ffmpeg does not exist at '", ffmpeg, "'.\n",
        "  To download, run the command\n",
        "    `check_tools_dir`(download = TRUE)`\n",
        "### end checking ###"
      )
      result$ffmpeg <- FALSE
    }
  }

  # extractor - fiji ---------------------------------------------------------------

  fiji <- file.path( tools_path(), "Fiji.app" )

  message( "### checking path to Fiji.app '", fiji, " ###" )
  if (!file.exists( fiji )) {
    if (download) {
      link <- switch(
        Sys.info()['sysname'],
        Darwin = "https://downloads.imagej.net/fiji/archive/20191027-2045//fiji-macosx.zip",
        Windows = "https://downloads.imagej.net/fiji/archive/20191027-2045/fiji-win64.zip",
        Linux = "https://downloads.imagej.net/fiji/archive/20191027-2045/fiji-linux64.zip",
        stop("OS not supported by Fiji!")
      )
      utils::download.file(
        url = link,
        destfile = file.path(path, "fiji.zip"),
        mode = "wb"
      )
      message("Extracting...")
      utils::unzip(
        zipfile = file.path(path, "fiji.zip"),
        exdir = file.path( path )
      )

      Sys.chmod(
        paths = list.files(
          bemovi.LEEF::par_IJ.path(),
          full.names = TRUE
        ),
        mode = "555"
      )

      Sys.chmod(
        paths = list.files(
          bemovi.LEEF::par_java.path(),
          full.names = TRUE
        ),
        mode = "555"
      )

      unlink(file.path(path, "fiji.zip"))
    } else {
      warning(
        "Fiji.app does not exist at '", fiji, "'.\n",
        "  To download, run the command\n",
        "    `check_tools_dir`(download = TRUE)`\n",
        "### end checking ###"
      )
      result$Fiji.app <- FALSE
    }
  }


# the end -----------------------------------------------------------------

  if (length(result) == 0 ) {
    result <- TRUE
  }

  return(result)

}



