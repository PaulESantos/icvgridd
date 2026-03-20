
#' Call exiftool from R
#'
#' Uses \code{system()} to run a basic call to \code{exiftool}.

#' @param args a list of non-shell quoted arguments (e.g. \code{-n -csv})
#' @param fnames a list of filenames (\code{shQuote()} will be applied to this vector)
#' @param intern \code{TRUE} if output should be returned as a character vector.
#' @param quiet Suppress output of the command itself.
#' @param ... additional arguments to be passed to \link{system2}
#'
#' @return The exit code if \code{intern=FALSE}, or the standard output as a character vector
#'  if \code{intern=TRUE}.
#' @export
#'
#' @examples
#' exiftool_call()
#' exiftool_version()
#'
exiftool_call <- function(args = NULL, fnames = NULL, intern = FALSE, ..., quiet = FALSE) {
  if(!("command" %in% names(exiftool_options))) {
    stop("ExifTool not properly configured. Run configure_exiftool() to debug.")
  }

  command <- exiftool_options$command
  args <- c(exiftool_options$args, args)

  if(length(fnames) > 0) {
    args <- c(args, shQuote(fnames))
  }

  if(!quiet) message(paste(c(shQuote(command), args), collapse = " "))
  if(intern) {
    dots <- list(...)
    do.call(command_stdout, c(list(command = command, args = args), dots))
  } else {
    dots <- list(...)
    do.call(system2_compat, c(list(command = command, args = args), dots))
  }
}

#' @rdname exiftool_call
#' @export
exiftool_version <- function() {
  as.numeric(exiftool_call(args = "-ver", intern = TRUE, quiet = TRUE))
}

#' @rdname exiftool_call
#' @export
exiftool_command <- function(args = character(0), fnames = character(0)) {
  # this is needed to estimate the command line length for large numbers of filenames

  if(!("command" %in% names(exiftool_options))) {
    stop("ExifTool not properly configured. Run configure_exiftool() to debug.")
  }

  command <- exiftool_options$command
  args <- c(exiftool_options$args, args)

  if(length(fnames) > 0) {
    args <- c(args, shQuote(fnames))
  }

  paste(c(shQuote(command), args), collapse = " ")
}


# where the configuration is stored: a local environment
exiftool_options <- new.env(parent = emptyenv())

#' Configure perl, ExifTool
#'
#' @param command The exiftool command or location of exiftool.pl
#' @param perl_path The path to the perl executable
#' @param install_url The url from which exiftool could be installed
#' @param install_location The location to install exiftool
#' @param quiet Use quiet = FALSE to display status updates
#'
#' @return The exiftool command, invisibly
#' @export
#'
configure_exiftool <- function(command = NULL, perl_path = NULL,
                               install_url = NULL,
                               install_location = NULL,
                               quiet = FALSE) {

  # appdir location
  rappdir_location <- file.path(
    rappdirs::user_data_dir(appname = "r-exifr", appauthor = "paleolimbot"),
    "exiftool"
  )

  # configure perl
  if(is.null(perl_path)) {
    perl_path <- configure_perl(quiet = quiet)
  }

  if(is.null(command)) {
    # use default list of possible locations
    command <- c(
      getOption("exifr.exiftoolcommand"),
      file.path(rappdir_location, "exiftool.pl"),
      system.file("exiftool/exiftool.pl", package = "exifr"),
      "exiftool"
    )
  } else if(length(command) == 0) {
    command <- character(0)
  }

  # try commands on their own, or with perl if it exists
  for(com in setdiff(command, "")) {
    # automatically fail ''
    if(nchar(com) == 0) {
      next
    }

    # requires perl
    if(endsWith(com, ".pl")) {
      if(is.null(perl_path)) {
        next
      }
      saved_command <- com
      args <- shQuote(com)
      com <- perl_path
    } else {
      args <- character(0)
      saved_command <- com
    }

    if(test_exiftool(com, args, quiet = quiet)) {
      if(length(args) > 0) {
        arg_string <- paste(" with args", paste0("`", args, "`", collapse = " "))
      } else {
        arg_string <- ""
      }
      if(!quiet) message("ExifTool found at `", com, "`", arg_string)

      options(exifr.exiftoolcommand = saved_command)
      exiftool_options$command <- com
      exiftool_options$args <- args
      if(!quiet) message("Using ExifTool version ", exiftool_version())
      return(invisible(com))
    }
  }

  if(is.null(install_url)) {
    # don't install!
    warning(
      "Could not find ExifTool at any of the following locations: ",
      paste0("`", command, "`", collapse = ", ")
    )
    return(invisible(NULL))
  }

  if(identical(install_url, TRUE)) {
    install_url <- "http://paleolimbot.github.io/exifr/exiftool.zip"
  }

  message(
    "Could not find ExifTool at any of the following locations: ",
    paste(command, collapse = ", "),
    ". Attempting to install from ",
    install_url
  )

  if(is.null(install_location)) {
    # define default install locations
    install_location <- c(
      rappdir_location,
      file.path(system.file("", package = "exifr")),
      path.expand("~/exiftool")
    )
  }

  # find writable locations
  write_dir <- find_writable(install_location)

  # attempt to download the file
  download_file <- tempfile()[1]
  on.exit(unlink(download_file))
  curl::curl_download(install_url, download_file, quiet = quiet)

  # extract downloaded file
  if(!quiet) message("Extracting exiftool to ", write_dir)
  utils::unzip(download_file, exdir = write_dir)

  # configure exiftool
  invisible(
    configure_exiftool(
      file.path(write_dir, "exiftool", "exiftool.pl"),
      perl_path = perl_path,
      install_url = NULL
    )
  )
}

#' @rdname configure_exiftool
#' @export
configure_perl <- function(perl_path = NULL, quiet = FALSE) {
  if(is.null(perl_path)) {
    # use default list of possible locations
    perl_path <- c(
      getOption("exifr.perlpath"),
      "perl",
      "C:\\Perl64\\bin\\perl",
      "C:\\Perl\\bin\\perl",
      "C:\\Strawberry\\perl\\bin\\perl"
    )
  }

  for(p in unique(perl_path)) {
    if(nchar(p) == 0) {
      next
    }
    if(test_perl(p, quiet = quiet)) {
      if(!quiet) message("Perl found at `", p, "`")
      options(exifr.perlpath = p)
      return(invisible(p))
    }
  }

  warning(
    "Could not find perl at any of the following locations: ",
    paste0("`", perl_path, "`", collapse = ", "),
    ". Specify perl location using options(exifr.perlpath='my/path/to/perl')"
  )
  invisible(NULL)
}

#' @rdname configure_exiftool
#' @export
configure_exiftool_reset <- function() {
  rm("command", envir = exiftool_options)
  rm("args", envir = exiftool_options)
}

test_perl <- function(command, quiet = TRUE) {
  if(!quiet) message("Trying perl command: `", command, "`")
  test_command(command, args = "--version", return_code = 0, regex_stdout = "[Pp]erl")
}

test_exiftool <- function(command, args = character(0), quiet = TRUE) {
  if(length(args) > 0) {
    arg_string <- paste(" with args", paste0("`", args, "`", collapse = " "))
  } else {
    arg_string <- ""
  }

  if(!quiet) {
    message("Trying ExifTool at `", command, "`", arg_string)
  }

  test_command(command, c(args, "-ver"), return_code = 0, regex_stdout = "^\\s*[0-9.]+\\s*$")
}

test_command <- function(command, args = character(0), regex_stderr = NULL, regex_stdout = NULL, return_code = NULL) {
  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(c(out, err)))

  command_out <- suppressWarnings(
    suppressMessages(
      try(
        system2_compat(command, args = args, stdout = out, stderr = err),
        silent = TRUE
      )
    )
  )

  if(!is.null(regex_stderr) && !grepl(regex_stderr, paste(readLines(err), collapse = "\n"))) {
    return(FALSE)
  }

  if(!is.null(regex_stdout) && !grepl(regex_stdout, paste(readLines(out), collapse = "\n"))) {
    return(FALSE)
  }

  if(!is.null(return_code)) {
    return(command_out == return_code)
  }

  return(!inherits(command_out, "try-error"))
}

command_stdout <- function(command, args = character(0), env = NULL, ..., quiet = FALSE) {
  output <- tempfile()
  err <- tempfile()
  on.exit(unlink(c(output, err)))

  system2_compat(command, args, env = env, stdout = output, stderr = err, ...)

  if(file.exists(err) && !quiet) {
    for(line in readLines(err)) {
      if(nchar(line) > 0) {
        message(line)
      }
    }
  }

  if(file.exists(output)) {
    paste(readLines(output), collapse = "\n")
  } else {
    ""
  }
}

system2_compat <- function(command, args = character(0), env = NULL, ...) {
  if(.Platform$OS.type == "windows" && length(env) > 0) {
    with_envvars(env, system2(command, args = args, ...))
  } else {
    system2(command, args = args, env = env, ...)
  }
}

with_envvars <- function(env, code) {
  if(length(env) == 0) {
    return(force(code))
  }

  env_parts <- strsplit(env, "=", fixed = TRUE)
  env_names <- vapply(env_parts, function(x) x[[1]], character(1))
  env_values <- vapply(
    env_parts,
    function(x) paste(x[-1], collapse = "="),
    character(1)
  )

  old_values <- Sys.getenv(env_names, unset = NA_character_)
  on.exit({
    to_restore <- !is.na(old_values)
    if(any(to_restore)) {
      restore <- as.list(old_values[to_restore])
      names(restore) <- env_names[to_restore]
      do.call(Sys.setenv, restore)
    }

    to_unset <- is.na(old_values)
    if(any(to_unset)) {
      do.call(Sys.unsetenv, as.list(env_names[to_unset]))
    }
  }, add = TRUE)

  new_values <- as.list(env_values)
  names(new_values) <- env_names
  do.call(Sys.setenv, new_values)

  force(code)
}

find_writable <- function(install_location) {
  for(il in install_location) {
    testfile <- file.path(il, ".dummyfile")
    file.create(testfile, showWarnings = FALSE)
    if(file.exists(testfile)) {
      unlink(testfile)
      return(il)
    }
  }

  stop(
    "Could not find a writable directory in which to install ExifTool. Tried ",
    paste0("`", install_location, "`", collapse = ", ")
  )
}
