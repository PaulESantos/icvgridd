#' Leer datos EXIF de archivos
#'
#' Lee datos EXIF en un \code{data.frame} llamando a la aplicación de línea de comandos
#' ExifTool, escrita por Phil Harvey. Dependiendo del número de imágenes y
#' los requisitos de longitud de línea de comandos, el comando puede ser llamado varias veces.
#'
#' Desde el sitio web de \href{https://exiftool.org}{ExifTool}:
#' ExifTool es una biblioteca Perl independiente de la plataforma junto con una aplicación de línea de comandos
#' para leer, escribir y editar información de metadatos en una amplia
#' variedad de archivos. ExifTool admite muchos formatos de metadatos diferentes, incluyendo
#' EXIF, GPS, IPTC, XMP, JFIF, GeoTIFF, ICC Profile, Photoshop IRB, FlashPix,
#' AFCP y ID3, así como las notas del fabricante de muchas cámaras digitales de Canon,
#' Casio, FLIR, FujiFilm, GE, HP, JVC/Victor, Kodak, Leaf,
#' Minolta/Konica-Minolta, Motorola, Nikon, Nintendo, Olympus/Epson,
#' Panasonic/Leica, Pentax/Asahi, Phase One, Reconyx, Ricoh, Samsung, Sanyo,
#' Sigma/Foveon y Sony. Para obtener más información, consulta el
#' \href{https://exiftool.org}{sitio web de ExifTool}.
#'
#' Ten en cuenta que las etiquetas binarias como las miniaturas se cargan como cadenas codificadas en base64
#' que comienzan con "base64:".
#'
#' @param path Un vector de nombres de archivo
#' @param tags Un vector de etiquetas para la salida. Es una buena idea especificar esto
#'   cuando se leen grandes cantidades de archivos, ya que reduce significativamente la sobrecarga de salida
#'   Los espacios serán eliminados en el data frame de salida. Este
#'   parámetro no distingue entre mayúsculas y minúsculas.
#' @param recursive TRUE para pasar la opción "-r" a ExifTool
#' @param args Argumentos adicionales
#' @param quiet Utiliza FALSE para mostrar información de diagnóstico
#'
#' @return Un data frame (tibble) con columnas SourceFile y una por etiqueta leída en
#'   cada archivo. El número de filas puede diferir, especialmente si recursive está configurado
#'   en TRUE, pero en general será una por archivo.
#' @export
#'
read_exif <- function(path, tags = NULL, recursive = FALSE, args = NULL, quiet = TRUE) {

  # ---- general input processing ----
  # expand path
  path <- path.expand(path)

  # check that all files exist (files that do not exist cause problems later,
  # as do directories without recursive = TRUE)
  if(recursive) {
    missing_dirs <- path[!dir.exists(path)]
    if(length(missing_dirs) > 0) {
      stop("Did you mean recursive = TRUE? ",
           "The following directories are missing (or are not directories). ",
           paste(missing_files, collapse = ", "))
    }
  } else {
    missing_files <- path[!file.exists(path) | dir.exists(path)]
    if(length(missing_files) > 0) {
      stop("Did you mean recursive = TRUE? ",
           "The following files are missing (or are not files): ",
           paste(missing_files, collapse = ", "))
    }
  }

  if(length(path) == 0) {
    return(tibble::tibble(SourceFile = character(0)))
  }

  if(recursive) {
    args <- c(args, "-r")
  }

  if(!is.null(tags)) {
    # tags cannot have spaces...whitespace is stripped by ExifTool
    tags <- gsub("\\s", "", tags)
    args <- c(paste0("-", tags), args)
  }

  # required args are -n for numeric output,
  # -j for JSON output, -q for quiet, and -b to make sure
  # output is base64 encoded
  args <- unique(c("-n", "-j", "-q", "-b", args))
  # an extra -q further silences warnings
  if(quiet) {
    args <- c(args, "-q")
  }

  # ---- generate system commands ----

  if(!quiet) message("Generating command line arguments...")

  if(.Platform$OS.type == "windows") {
    command_length <- 8191 # according to https://support.microsoft.com/en-us/kb/830473
  } else {
    # according to the interweb this should be around 100kb, but 50kb should do
    # (is 'getconf ARG_MAX' on mac, equals 262144)
    command_length <- 50 * 1024
  }

  # initialize variables
  images_per_command <- length(path)
  commands <- exiftool_command(args = args, fnames = path)
  ngroups <- 1
  groups <- rep(1, length(path))

  while(any(nchar(commands) >= (command_length * 0.75)) && (images_per_command >= 2)) {
    # make the images per command half of previous value
    images_per_command <- images_per_command %/% 2

    # calculate number of groups
    ngroups <- (length(path) + images_per_command - 1) %/% images_per_command
    groups <- rep(seq_len(ngroups), rep(images_per_command, ngroups))[seq_along(path)]

    # subdivide path into groups and generate the commands
    commands <- vapply(
      split(path, groups),
      function(fnames) exiftool_command(args = args, fnames = fnames),
      character(1)
    )
  }

  # ---- run the commands, read output ----

  if(!quiet) message("Running ", length(commands), " commands")
  results <- lapply(split(path, groups), function(fnames) read_exif_base(args, fnames, quiet = quiet))
  tibble::as_tibble(do.call(plyr::rbind.fill, results))
}

# base function to read a single command to a df
read_exif_base <- function(args, fnames, quiet = TRUE) {
  # run command
  return_value <- exiftool_call(args, fnames, intern = TRUE, quiet = quiet)

  # read, return the output
  tibble::as_tibble(jsonlite::fromJSON(return_value))
}
