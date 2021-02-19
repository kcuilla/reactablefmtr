#' Add horizontal gradient bars to rows in a column
#'
#' The `data_bars_gradient()` function conditionally adds a left-to-right linear gradient to each row of a column.
#'     The length of the bars are relative to the value of the row in relation to other values within the same column.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A vector of colors of at least two colors.
#'     Colors should be given in order from left to right as shown on the data bar.
#'     Default colors are c("#1efffd", "#1e20ff").
#'
#' @param background Optionally assign a color to use as the background for cells.
#'     Default is set to white.
#'
#' @param commas Optionally format values as commas.
#'     Default is set to NULL or FALSE.
#'
#' @return a function that applies data bars
#'     to a column of numeric values.
#'
#' @import reactable
#'
#' @examples
#' library(reactable)
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, colors are provided
#' reactable(data,
#' defaultColDef = colDef(
#' align = "left",
#' cell = data_bars_gradient(data)))
#'
#' ## Apply custom colors for gradient
#' reactable(data,
#' defaultColDef = colDef(
#' align = "left",
#' cell = data_bars_gradient(data, colors = c("white","grey","black"))))
#'
#' ## Apply bacground color
#' reactable(data,
#' defaultColDef = colDef(
#' align = "left",
#' cell = data_bars_gradient(data, background = "black")))
#'
#' @export


data_bars_gradient <- function(data, colors = c("#1efffd", "#1e20ff"), background = "white", commas = NULL) {

  if (length(colors) == 1) {

    stop("must provide at least two colors. Ex. colors = c('white','grey','black')")
  }

  gradient <- paste0("linear-gradient(to right,", paste(colors, collapse = ", "))

  cell <- function(value, index, name) {

    bar_chart <-
      function(label,
               width = "100%",
               height = "16px",
               backgroundImage = gradient,
               background = NULL) {

        bar <-
          htmltools::div(style = list(
            background = gradient,
            width = width,
            height = height
          ))

        chart <-
          htmltools::div(style = list(
            flexGrow = 1,
            marginLeft = "8px",
            background = background
          ),
          bar)

        htmltools::div(style = list(display = "flex", alignItems = "center"),
                       label,
                       chart)
      }

    width <- if (is.numeric(value)) {

      paste0(value / max(data[[name]], na.rm = TRUE) * 100, "%")

    } else if (!is.numeric(value))

      return(value)

    if (is.null(commas) || commas == FALSE) {

      value <- value

    } else value <- format(value, big.mark = ",")

    max_digits <- nchar(max(data[[name]]))

    value <- format(value, width = max_digits, justify = "right")

    bar_chart(value,
              width = width,
              backgroundImage = gradient,
              background = background)
  }
}
