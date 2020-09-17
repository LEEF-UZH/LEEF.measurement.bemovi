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

  ffmpeg <- file.path( path, "ffmpeg", "bin", "ffmpeg" )
  message( "### checking path to ffmpeg '", ffmpeg, " ###" )
  if (!file.exists( ffmpeg )) {
    if (download) {
      link <- switch(
        Sys.info()['sysname'],
        Darwin = "https://ffmpeg.zeranoe.com/builds/macos64/static/ffmpeg-4.3.1-macos64-static.zip",
        Windows = "https://ffmpeg.zeranoe.com/builds/win64/static/ffmpeg-20200831-4a11a6f-win64-static.zip",
        Linux = "https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz",
        stop("OS not supported by Fiji!")
      )
      utils::download.file(
        url = link,
        destfile = file.path(path, "ffmpeg.zip"),
        mode = "wb"
      )
      message("Extracting...")
      x <- utils::unzip(
        zipfile = file.path(path, "ffmpeg.zip"),
        exdir = file.path( path )
      )
      file.rename(
        from = file.path(path, strsplit(x[[1]], .Platform$file.sep)[[1]][[2]]),
        to = file.path(path, "ffmpeg")
      )

      Sys.chmod(
        paths = list.files( file.path( path, "ffmpeg"), full.names = TRUE, recursive = TRUE),
        mode = "555"
      )

      unlink(file.path(path, "ffmpeg.zip"))
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

  # IJ.path <- file.path( tools_path(), "Fiji.app", "Contents", "MacOS" )
  # java.path <- file.path( tools_path(),  "Fiji.app", "java", "macosx", "jdk1.8.0_172.jre", "jre", "Contents", "Home", "bin")

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
          file.path(
            path,
            "Fiji.app",
            "Contents",
            "MacOS"
          ), full.names = TRUE),
        mode = "555"
      )

      Sys.chmod(
        paths = list.files(
          file.path(
            path,
            "Fiji.app",
            "java",
            "macosx",
            "adoptopenjdk-8.jdk",
            "jre",
            "Contents",
            "Home",
            "bin"
          ), full.names = TRUE),
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



