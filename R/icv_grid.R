#' Añade una grilla a una imagen para el cálculo del índice de contaminación visual.
#'
#' Esta función toma una imagen y agrega una grilla con líneas de diferentes
#' colores para ayudar en el cálculo del índice de contaminación visual.
#'
#' @param img_path Ruta al archivo de imagen.
#' @param dpi dpi resolucion empleada para guardar la imagen.
#' @param save argumento que define si la imagen se guarda o visualiza, por
#'   defecto TRUE.
#' @param resolution resolucion manual en dpi. Se usa cuando la resolucion no
#'   puede leerse desde los metadatos EXIF o cuando se desea forzar un valor.
#'
#' @return La función guarda la imagen con la grilla añadida en la misma carpeta
#'   del archivo original, con el sufijo `_grid.jpeg`.
#'
#' @examples
#' # Ejemplo de uso:
#' # add_icvgrid("imagen.jpg", resolution = 72)
#' @export
add_icvgrid <- function(img_path, dpi = 150, save = TRUE, resolution = NULL) {
  if(!is.null(resolution) && !(length(resolution) %in% c(1, length(img_path)))) {
    stop("`resolution` must be NULL, a single value, or one value per image.")
  }

  gridd_drawing <- function(img_path, dpi = dpi, save = save, resolution = NULL) {
    # Empty tibble for images display
    df <- dplyr::tibble(
      x = NULL,
      y = NULL
    )

    # import image file
    image_file <- magick::image_read(path = img_path)
    image_info <- magick::image_info(image_file)

    image_width <- image_info$width[[1]]
    image_height <- image_info$height[[1]]

    print(
      ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggpubr::background_image(image_file)
    )

    if(!is.null(resolution)) {
      dpi_res <- as.numeric(resolution)
      message("Using manual resolution: ", dpi_res, " dpi")
    } else {
      # get metadata only when a manual resolution was not provided
      meta <- janitor::clean_names(
        icvgridd::read_exif(path = img_path)
      )

      exif_x_resolution <- NA_real_
      exif_y_resolution <- NA_real_

      if(nrow(meta) > 0) {
        if("x_resolution" %in% names(meta)) {
          exif_x_resolution <- suppressWarnings(as.numeric(meta$x_resolution[[1]]))
        }

        if("y_resolution" %in% names(meta)) {
          exif_y_resolution <- suppressWarnings(as.numeric(meta$y_resolution[[1]]))
        }
      }

      if(!is.na(exif_x_resolution) && !is.na(exif_y_resolution)) {
        message(
          "Image resolution: ",
          exif_x_resolution,
          " x ",
          exif_y_resolution,
          " dpi"
        )
      } else {
        message("Image resolution could not be read from EXIF metadata.")
      }

      if(!is.na(exif_x_resolution) && exif_x_resolution > 0) {
        if(interactive()) {
          dpi_res <- readline(
            "Press Enter to use the detected resolution or type a manual value in dpi (e.g. 96 or 72): "
          )

          if(!grepl("^[0-9]+(\\.[0-9]+)?$", dpi_res)) {
            dpi_res <- exif_x_resolution
          } else {
            dpi_res <- as.numeric(dpi_res)
          }
        } else {
          dpi_res <- exif_x_resolution
        }
      } else {
        stop(
          "Could not read image resolution from EXIF metadata. ",
          "Supply a manual value with `resolution = ...`."
        )
      }
    }

    if(length(dpi_res) != 1 || is.na(dpi_res) || !is.finite(dpi_res) || dpi_res <= 0) {
      stop("`resolution` must be a single positive number.")
    }

    meta_2 <- dplyr::tibble(
      image_width = image_width,
      image_height = image_height,
      x_cm = (image_width / dpi_res) / 0.393701,
      y_cm = (image_height / dpi_res) / 0.393701
    )

    # New tibble with the grid coordinates
    df1 <- dplyr::tibble(
      y = seq(0, meta_2$y_cm, (meta_2$y_cm / 20)),
      x = seq(0, meta_2$x_cm, (meta_2$x_cm / 20))
    )

    yinterval <- seq(0, meta_2$y_cm, (meta_2$y_cm / 20))
    primer_nivel <- yinterval[1:15]
    segundo_nivel <- yinterval[16:21]

    # Plot with background image
    plot1 <-
      ggplot2::ggplot(df, ggplot2::aes(x, y)) +
      ggpubr::background_image(image_file) +
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

    if(isTRUE(save)) {
      print(plot1)
      file_extencion <- file.path(
        dirname(img_path),
        paste0(tools::file_path_sans_ext(basename(img_path)), "_grid.jpeg")
      )
      ggplot2::ggsave(
        filename = file_extencion,
        plot = plot1,
        dpi = dpi,
        device = "jpeg",
        height = meta_2$image_height,
        width = meta_2$image_width,
        units = "px",
        limitsize = FALSE
      )
    } else {
      print(plot1)
    }
  }

  for(i in seq_along(img_path)) {
    img_resolution <- if(is.null(resolution)) {
      NULL
    } else if(length(resolution) == 1) {
      resolution
    } else {
      resolution[[i]]
    }

    gridd_drawing(img_path[[i]], dpi, save = save, resolution = img_resolution)
  }
}
