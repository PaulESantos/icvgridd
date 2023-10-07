#' Añade una grilla a una imagen para el cálculo del índice de contaminación visual.
#'
#' Esta función toma una imagen y agrega una grilla con líneas de diferentes colores para ayudar en el cálculo del índice de contaminación visual.
#'
#' @param img_path Ruta al archivo de imagen.
#' @param dpi dpi for image gridded save.
#'
#' @return La función guarda la imagen con la grilla añadida en un archivo con el nombre modificado.
#'
#' @examples
#' # Ejemplo de uso:
#' # add_icvgrid("imagen.jpg")
#' @export
add_icvgrid <- function(img_path, dpi = 150) {
meta <- icvgridd::read_exif(path = img_path) |>
  janitor::clean_names() |>
  dplyr::select(
    file_name,
    dplyr::contains("resolution"),
    dplyr::contains("image"),
    megapixels
  )

message("Image resolution: ",
        meta$x_resolution,
        " x ",
        meta$y_resolution,
        " pixels")

dpi_res <- readline("The resolution its correct? (y/n):
                           Enter preferred resolution (e.g. 96 or 72): ")
if (!grepl("^[0-9]+$", dpi_res)) {
  dpi_res <- as.integer(meta$x_resolution)
}



meta_2 <- meta |>
  dplyr::mutate(
    x_cm = (image_width / as.numeric(dpi_res)) / 0.393701,
    y_cm = (image_height / as.numeric(dpi_res)) / 0.393701
  )
########
df <- dplyr::tibble(
  x = NULL,
  y = NULL
)
df1 <- dplyr::tibble(
  y = seq(0, meta_2$y_cm, (meta_2$y_cm / 20)),
  x = seq(0, meta_2$x_cm, (meta_2$x_cm / 20))
)

img <- jpeg::readJPEG(img_path)

yinterval <- seq(0, meta_2$y_cm, (meta_2$y_cm / 20))
primer_nivel <- yinterval[1:15]
segundo_nivel <- yinterval[16:21]
# Plot with background image
plot1 <-
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
  ggpubr::background_image(img) +
  ggplot2::geom_hline(
    yintercept = primer_nivel,
    color = "green",
    linetype = "longdash"
  ) +
  ggplot2::geom_hline(
    yintercept = segundo_nivel,
    color = "red",
    linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    data = df1,
    ggplot2::aes(
      x = x, xend = x,
      y = 0, yend = segundo_nivel[1]
    ),
    color = "green",
    linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    data = df1,
    ggplot2::aes(
      x = x, xend = x,
      y = segundo_nivel[1], yend = segundo_nivel[length(segundo_nivel)]
    ),
    color = "red",
    linetype = "dashed"
  ) +
  ggplot2::geom_hline(
    yintercept = seq(0, meta_2$y_cm, meta_2$y_cm / 3)[2:3],
    color = "#324aa8",
    linetype = "dotdash",
    linewidth = 1.5
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  ) +
  ggplot2::coord_fixed(expand = FALSE)
# ggplot2::coord_cartesian(expand = TRUE)

print(plot1)

file_extencion <- gsub("\\..*", "_grid.jpeg", img_path)
ggplot2::ggsave(here::here(file_extencion),
                plot = plot1,
                dpi = dpi,
                device = "jpeg",
                height = meta_2$image_height,
                width = meta_2$image_width,
                units = "px",
                limitsize = FALSE
)

}
