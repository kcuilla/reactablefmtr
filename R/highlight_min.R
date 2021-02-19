#' Highlights the minimum value in a column
#'
#' The `highlight_min()` function assigns a font color and/or background color to the minimum value in a column.
#'     It should be placed within the style argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param font_color color to assign to minimum value in a column.
#'     Default color is red.
#'
#' @param highlighter color to assign the background of a cell containing minimum value in a column.
#'
#' @return a function that applies a color
#'     to the minimum value in a column of numeric values.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::road[11:17, ]
#'
#' ## By default, the minimum value is bold with a red font color
#' reactable(data,
#' defaultColDef = colDef(
#'     style = highlight_min(data)))
#'
#' ## Assign a different font color
#' reactable(data,
#' defaultColDef = colDef(
#'     style = highlight_min(data,
#'     font_color = "green")))
#'
#' ## Highlight the background of the cell for the minimum value in each column
#' reactable(data,
#' defaultColDef = colDef(
#'     style = highlight_min(data,
#'     highlighter = "yellow")))
#'
#' @export


highlight_min <- function(data, font_color = "red", highlighter = NULL) {

  style <- function(value, index, name) {

    if (!is.numeric(value)) {

      return(value)

    } else if (!is.na(value) && value == min(data[[name]], na.rm = TRUE)) {

      list(fontWeight = "bold", color = font_color, background = highlighter)
    }
  }
}
