#' Lee imágenes de un directorio con extensión común.
#'
#' Esta función toma una ruta de directorio y busca todas las imágenes con extensiones comunes (JPEG, JPG, PNG, BMP, TIFF, TIF) en el directorio y sus subdirectorios. Devuelve una lista de rutas de archivos de las imágenes encontradas.
#'
#' @param img_folder Ruta al directorio que contiene las imágenes.
#'
#' @return Una lista de rutas de archivos de las imágenes encontradas.
#'
#'
#' @export
read_icv_images <- function(img_folder){
  # check the root directori
  x <- normalizePath(img_folder, mustWork = FALSE)
  # made the normalized path to all scans img
  normalizePath(dir(path = x,
                    pattern = "\\.jpeg$|\\.jpg$|\\.png$|\\.bmp$|\\.tif$|\\.tiff$",
                    recursive = TRUE,
                    full.names = TRUE),
                mustWork = FALSE)
}
