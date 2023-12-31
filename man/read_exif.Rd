% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_exifr.R
\name{read_exif}
\alias{read_exif}
\title{Leer datos EXIF de archivos}
\usage{
read_exif(path, tags = NULL, recursive = FALSE, args = NULL, quiet = TRUE)
}
\arguments{
\item{path}{Un vector de nombres de archivo}

\item{tags}{Un vector de etiquetas para la salida. Es una buena idea especificar esto
cuando se leen grandes cantidades de archivos, ya que reduce significativamente la sobrecarga de salida
Los espacios serán eliminados en el data frame de salida. Este
parámetro no distingue entre mayúsculas y minúsculas.}

\item{recursive}{TRUE para pasar la opción "-r" a ExifTool}

\item{args}{Argumentos adicionales}

\item{quiet}{Utiliza FALSE para mostrar información de diagnóstico}
}
\value{
Un data frame (tibble) con columnas SourceFile y una por etiqueta leída en
cada archivo. El número de filas puede diferir, especialmente si recursive está configurado
en TRUE, pero en general será una por archivo.
}
\description{
Lee datos EXIF en un \code{data.frame} llamando a la aplicación de línea de comandos
ExifTool, escrita por Phil Harvey. Dependiendo del número de imágenes y
los requisitos de longitud de línea de comandos, el comando puede ser llamado varias veces.
}
\details{
Desde el sitio web de \href{https://exiftool.org}{ExifTool}:
ExifTool es una biblioteca Perl independiente de la plataforma junto con una aplicación de línea de comandos
para leer, escribir y editar información de metadatos en una amplia
variedad de archivos. ExifTool admite muchos formatos de metadatos diferentes, incluyendo
EXIF, GPS, IPTC, XMP, JFIF, GeoTIFF, ICC Profile, Photoshop IRB, FlashPix,
AFCP y ID3, así como las notas del fabricante de muchas cámaras digitales de Canon,
Casio, FLIR, FujiFilm, GE, HP, JVC/Victor, Kodak, Leaf,
Minolta/Konica-Minolta, Motorola, Nikon, Nintendo, Olympus/Epson,
Panasonic/Leica, Pentax/Asahi, Phase One, Reconyx, Ricoh, Samsung, Sanyo,
Sigma/Foveon y Sony. Para obtener más información, consulta el
\href{https://exiftool.org}{sitio web de ExifTool}.

Ten en cuenta que las etiquetas binarias como las miniaturas se cargan como cadenas codificadas en base64
que comienzan con "base64:".
}
