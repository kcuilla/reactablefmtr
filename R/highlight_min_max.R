#' Highlights the minimum and maximum value in a column
#'
#' The `highlight_min_max()` function assigns a font color and/or background color to both the minimum and maximum values in a column.
#'     It should be placed within the style argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param min_font_color color to assign to minimum value in a column.
#'     Default color is red.
#'
#' @param max_font_color color to assign to maximum value in a column.
#'     Default color is green.
#'
#' @param min_highlighter color to assign the background of a cell containing minimum value in a column.
#'
#' @param max_highlighter color to assign the background of a cell containing maximum value in a column.
#'
#' @import reactable
#'
#' @return a function that applies a color
#'     to the minimum and maximum values in a column of numeric values.
#'
#' @examples
#' data <- MASS::road[11:17, ]
#'
#' ## By default, the minimum and maximum values are bold with a red and green font color respectively
#' reactable(data,
#' defaultColDef = colDef(
#'     style = highlight_min_max(data)))
#'
#' ## Assign a different font color to the min and max values
#' reactable(data,
#' defaultColDef = colDef(
#'     style = highlight_min_max(data,
#'     min_font_color = "orange",
#'     max_font_color = "blue")))
#'
#' ## Highlight the background of the cell for the min and max values in each column
#' reactable(data,
#' defaultColDef = colDef(
#'     style = highlight_min_max(data,
#'     min_highlighter = "salmon",
#'     max_highlighter = "skyblue")))
#'
#' @export


highlight_min_max <- function(data, min_font_color = "red", max_font_color = "green", min_highlighter = NULL, max_highlighter = NULL) {

  style <- function(value, index, name) {

    if (!is.numeric(value)) {

      return(value)

    } else if (!is.na(value) && value == min(data[[name]], na.rm = TRUE)) {

      list(fontWeight = "bold", color = min_font_color, background = min_highlighter)

    } else if (!is.na(value) && value == max(data[[name]], na.rm = TRUE)) {

      list(fontWeight = "bold", color = max_font_color, background = max_highlighter)
    }
  }
}
