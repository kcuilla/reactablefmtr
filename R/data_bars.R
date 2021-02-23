#' Add horizontal bars to rows in a column
#'
#' The `data_bars()` function conditionally adds a horizontal bar to each row of a column.
#'     The length of the bars are relative to the value of the row in relation to other values within the same column.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A single color or a vector of colors.
#'     Colors should be given in order from low values to high values.
#'     Can use R's built-in colors or other color packages.
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
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' library(reactable)
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## Horizontal bars with lengths relative to cell value
#' reactable(data,
#' columns = list(
#' MPG.city = colDef(
#' name = "MPG (city)",
#' align = "left",
#' cell = data_bars(data, "dodgerblue"))))
#'
#' ## Add background color
#' reactable(data,
#' columns = list(
#' MPG.city = colDef(
#' name = "MPG (city)",
#' align = "left",
#' cell = data_bars(data, "dodgerblue", "grey"))))
#'
#' ## Conditionally color data bars based on their relative values
#' ## by supplying more than one color
#' ## and apply across all numeric columns using reactable::defaultColDef
#' reactable(data,
#' pagination = FALSE,
#' defaultSortOrder = "desc",
#' defaultSorted = "MPG.city",
#' defaultColDef = colDef(
#' cell = data_bars(data,
#' colors = c("firebrick1","gold","limegreen"))))
#'
#' ## Use number_fmt to format numbers using the scales package
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#' defaultColDef = colDef(cell = data_bars(car_prices,
#' number_fmt = scales::dollar_format(accuracy = 0.1))))
#'
#' @export


data_bars <- function(data, colors = "#1e90ff", background = "white", number_fmt = NULL) {

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    color_pal <- function(x) {

      if (!is.na(x))
        rgb(colorRamp(c(colors))(x), maxColorValue = 255)
      else
        NULL
    }

    normalized <-
      (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

    fill_color <- color_pal(normalized)

    bar_chart <-
      function(label,
               width = "100%",
               height = "16px",
               fill = "#00bfc4",
               background = NULL) {

        bar <-
          htmltools::div(style = list(
            background = fill,
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
              fill = fill_color,
              background = background)
  }
}

