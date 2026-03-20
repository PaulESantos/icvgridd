
# .onLoad hook for when namespace is loaded
.onLoad <- function(libname, pkgname) {
  ensure_exifr(install = interactive(), quiet = TRUE)
  configure_exiftool(quiet = TRUE, install_url = TRUE)
}

# .onAttach for library(exifr)
.onAttach <- function(libname, pkgname) {
  if(!ensure_exifr(install = FALSE, quiet = TRUE)) {
    packageStartupMessage(
      "Package `exifr` is not installed. Run `icvgridd::ensure_exifr()` ",
      "or `remotes::install_github('paleolimbot/exifr')`."
    )
  }

  version <- try(exiftool_version(), silent = TRUE)
  if(!inherits(version, "try-error")) {
    packageStartupMessage("icvgridd is using ExifTool version ", version)
  } else {
    # there are already a bunch of warnings as a result of the failed
    # configuration
  }
}
