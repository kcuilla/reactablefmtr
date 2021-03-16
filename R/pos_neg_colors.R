#' Assign colors to negative and positive values
#'
#' The `pos_neg_colors()` function assigns a color to all negative values and a color to all positive values.
#'     It should be placed within the style argument in reactable::colDef.
#'
#' @param neg_col color to assign to negative values.
#'
#' @param pos_col color to assign to positive values.
#'
#' @param bold optional argument to bold values.
#'     Default is set to NULL or not bold.
#'
#' @import reactable
#'
#' @return a function that applies a color
#'     to the positive and negative values of numeric column.
#'
#' @examples
#' data <- data.frame(
#' Symbol = c("GOOG", "FB", "AMZN", "NFLX", "TSLA"),
#' Price = c(1265.13, 187.89, 1761.33, 276.82, 328.13),
#' Change = c(4.14, 1.51, -19.45, 5.32, -12.45))
#'
#' ## Assign the color red to negative values and green to positive values
#' reactable(data,
#' columns = list(
#' Change = colDef(
#' style = pos_neg_colors("red", "green"))))
#'
#' ## Bold values
#' reactable(data,
#' columns = list(
#' Change = colDef(
#' style = pos_neg_colors("red", "green", bold = TRUE))))
#'
#' @export


pos_neg_colors <- function(neg_col, pos_col, bold = NULL) {

  colors <- function(value) {

    if (!is.numeric(value)) return(value)

    color <- if (!is.na(value) & value > 0) {
      pos_col

    } else if (!is.na(value) & value < 0) {
      neg_col
    }

    if (is.null(bold) || bold == FALSE) {
      list(color = color)

    } else if (bold == TRUE) {

      list(fontWeight = "bold", color = color)
    }
  }
}
