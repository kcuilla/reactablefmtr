#' Add horizontal gradient bars to rows in a column
#'
#' The `data_bars_gradient()` function is depreciated.
#'     The new version of `data_bars()` can convert colors into gradients with `gradient = TRUE`.
#'     Please use `data_bars()` instead.
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
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @return a function that applies data bars
#'     to a column of numeric values.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, colors are provided
#' reactable(data,
#' defaultColDef = colDef(
#' align = "left",
#' cell = data_bars(data,
#' fill_color = c("#1efffd", "#1e20ff"),
#' fill_gradient = TRUE)))
#'
#' @export


data_bars_gradient <- function(data, colors = c("#1efffd", "#1e20ff"), background = "white", number_fmt = NULL) {

  .Deprecated("data_bars(fill_gradient = TRUE)")

  if (length(colors) == 1) {

    stop("must provide at least two colors. Ex. colors = c('white','grey','black')")
  }

  gradient <- paste0("linear-gradient(to right,", paste(colors, collapse = ", "))

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

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
            height = height,
            transition = "width 1s"
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

    max_digits <- max(nchar(data[[name]]))+1

    chart_label <- stringr::str_pad(label, max_digits)

    bar_chart(chart_label,
              width = width,
              backgroundImage = gradient,
              background = background)
  }
}
