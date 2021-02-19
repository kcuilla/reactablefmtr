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
#' @param commas Optionally format values as commas.
#'     Default is set to NULL or FALSE.
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
#' @export


data_bars <- function(data, colors = "#1e90ff", background = "white", commas = NULL) {

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

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

     if (is.null(commas) || commas == FALSE) {

      value <- value

    } else value <- format(value, big.mark = ",")

    max_digits <- nchar(max(data[[name]]))

    value <- format(value, width = max_digits, justify = "right")

    bar_chart(value,
              width = width,
              fill = fill_color,
              background = background)
  }
}
