#' Add color scales to rows in a column
#'
#' The `color_scales()` function conditionally colors each cell of a column depending on their value in relation to other values in that particular column.
#'     It should be placed within the style argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A vector of colors to color the cells.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are red-white-blue: c("#ff3030", "#ffffff", "#1e90ff").
#'     Can use R's built-in colors or other color packages.
#'
#' @export
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## By default, the colors_scales() function uses a red-white-blue three-color pattern
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(style = color_scales(data))))
#'
#' ## If only two colors are desired, you can specify them within 'c(color1, color2)' or 'colors = c(color1, color2)':
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(style = color_scales(data, c("red", "green")))))
#'
#' ## Apply color_scales() across all numeric columns using reactable::defaultColDef
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data)))

color_scales <- function(data, colors = c("#ff3030", "#ffffff", "#1e90ff")) {

    color_pal <- function(x) {

      if (!is.na(x))
        rgb(colorRamp(c(colors))(x), maxColorValue = 255)
      else
        NULL
    }

    style <- function(value, index, name) {

      if (!is.numeric(value))
        return()

      normalized <-
        (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

      cell_color <- color_pal(normalized)

      list(background = cell_color)
    }
  }
