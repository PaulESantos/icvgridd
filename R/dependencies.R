#' Ensure that `exifr` is available
#'
#' Checks whether the [`exifr`](https://github.com/paleolimbot/exifr) package
#' is installed. If it is not available, the function can install it from
#' GitHub so that `icvgridd` can continue working with ExifTool metadata.
#'
#' In interactive sessions, `icvgridd` calls this function automatically when
#' the package is loaded. You can also run it manually after installation.
#'
#' @param install Logical. If `TRUE`, attempts to install `exifr` when it is
#'   not already available.
#' @param quiet Logical. If `FALSE`, prints progress messages.
#' @param repos CRAN repository used to install `remotes` when needed.
#'
#' @return `TRUE` if `exifr` is available after the check, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' ensure_exifr()
#' }
ensure_exifr <- function(install = interactive(),
                         quiet = FALSE,
                         repos = "https://cloud.r-project.org") {
  if(requireNamespace("exifr", quietly = TRUE)) {
    if(!quiet) {
      message("Package `exifr` is available (version ",
              as.character(utils::packageVersion("exifr")),
              ").")
    }
    return(TRUE)
  }

  if(!install) {
    if(!quiet) {
      message(
        "Package `exifr` is not installed. Install it with ",
        "`icvgridd::ensure_exifr()` or ",
        "`remotes::install_github('paleolimbot/exifr')`."
      )
    }
    return(FALSE)
  }

  if(!quiet) {
    message("Package `exifr` was not found. Installing dependency...")
  }

  remotes_ok <- requireNamespace("remotes", quietly = TRUE)
  if(!remotes_ok) {
    remotes_ok <- tryCatch(
      {
        utils::install.packages("remotes", repos = repos)
        requireNamespace("remotes", quietly = TRUE)
      },
      error = function(e) {
        if(!quiet) {
          warning("Could not install `remotes`: ", conditionMessage(e))
        }
        FALSE
      }
    )
  }

  if(!remotes_ok) {
    return(FALSE)
  }

  installed <- tryCatch(
    {
      remotes::install_github(
        "paleolimbot/exifr",
        dependencies = TRUE,
        upgrade = "never",
        quiet = quiet
      )
      requireNamespace("exifr", quietly = TRUE)
    },
    error = function(e) {
      if(!quiet) {
        warning("Could not install `exifr`: ", conditionMessage(e))
      }
      FALSE
    }
  )

  if(!installed && !quiet) {
    message(
      "Package `exifr` is still unavailable. ",
      "Try `remotes::install_github('paleolimbot/exifr')` manually."
    )
  }

  installed
}
