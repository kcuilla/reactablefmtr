#' Add color tiles to rows in a column
#'
#' The `color_tiles()` function conditionally colors the background of each cell similarly to color_scales().
#'     The difference is that color_tiles() uses round colored tiles around values instead of the entire background of the cell.
#'     Another difference is color_tiles() allows number formatting with number_fmt whereas color_scales() does not.
#'     The last difference is it needs to placed within the cell argument in reactable::colDef vs the style argument for color_scales().
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A vector of colors to color the cells.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are red-white-blue: c("#ff3030", "#ffffff", "#1e90ff").
#'     Can use R's built-in colors or other color packages.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @param bright_values Optionally display values as white.
#'     Values with a dark-colored background will be shown in white.
#'     Default is set to TRUE but can be turned off by setting to FALSE.
#'
#' @param span Optionally apply colors to values in relation to the entire dataset instead of by column.
#'     Default is set to NULL but can be turned on by setting to TRUE.
#'
#' @return a function that applies conditional color tiles
#'     to a column of numeric values.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## By default, the colors_tiles() function uses a red-white-blue three-color pattern
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(cell = color_tiles(data))))
#'
#' ## If only two colors are desired,
#' ## you can specify them with colors = 'c(color1, color2)';
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(cell = color_tiles(data,
#'  colors = c("red", "green")))))
#'
#' ## Use span to apply colors to values in relation to the entire dataset
#' reactable(data,
#' defaultColDef = colDef(cell = color_tiles(data, span = TRUE)))
#'
#' ## Use number_fmt to format numbers using the scales package
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#' defaultColDef = colDef(cell = color_tiles(car_prices,
#' number_fmt = scales::dollar)))
#'
#' @export


color_tiles <- function(data, colors = c("#ff3030", "#ffffff", "#1e90ff"), number_fmt = NULL, bright_values = TRUE, span = NULL) {

  color_pal <- function(x) {

    if (!is.na(x))
      rgb(colorRamp(c(colors))(x), maxColorValue = 255)
    else
      NULL
  }

  assign_color <- function(x) {

    if (!is.na(x)) {
      rgb_sum <- rowSums(colorRamp(c(colors))(x))
      color <- ifelse(rgb_sum >= 375, "black", "white")
      color
    } else
      NULL
  }

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    normalized <- if (is.null(span) || span == FALSE) {

      (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

    } else if (span == TRUE)

      (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))

    cell_color <- color_pal(normalized)

    font_color <- assign_color(normalized)

    if (bright_values == FALSE) {

      htmltools::div(label,
                     style = list(background = cell_color,
                                  display = "flex",
                                  justifyContent = "center",
                                  borderRadius = "4px",
                                  height = "18px"))

    } else

      htmltools::div(label,
                     style = list(background = cell_color,
                                  color = font_color,
                                  display = "flex",
                                  justifyContent = "center",
                                  borderRadius = "4px",
                                  height = "18px"))

  }
}
