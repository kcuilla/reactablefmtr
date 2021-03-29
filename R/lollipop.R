#' Add a horizontal lollipop to rows in a column
#'
#' The `lollipop()` function adds a lollipop (a circle on the end of a bar with a value inside) to each row of a column.
#'     The length of the bars are relative to the value of the row in relation to other values within the same column.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param circle_color A single color or vector of colors.
#'     If more than one color is provided, the colors will be assigned from lowest value to highest value in the column.
#'     Default color is "#1e90ff".
#'
#' @param bar_color A single color or vector of colors.
#'     If more than one color is provided, the colors will create a left-to-right gradient.
#'     Default color is "lightgrey".
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @param bright_values Display values with a dark-colored background in white text.
#'     Default is set to TRUE but can be turned off by setting to FALSE.
#'
#' @return a function that applies a lollipop
#'     to a column of numeric values.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, colors for the lollipop bar and circle are provided
#' reactable(
#' data,
#' defaultColDef = colDef(
#' cell = lollipop(data)))
#'
#' ## Change the color of the bar with 'bar_color' and the circle with 'circle_color'
#' reactable(
#' data,
#' columns = list(
#' MPG.city = colDef(
#'  cell = lollipop(data,
#'  bar_color = "black",
#'  circle_color = "red")),
#' MPG.highway = colDef(
#'  cell = lollipop(data,
#'  bar_color = "orange",
#'  circle_color = "black"))))
#'
#' ## Providing more than one color to 'bar_color' applies a color gradient
#' ## and providing more than one color to 'circle_color'
#' ## conditionally colors based on their relative values
#' reactable(
#' data,
#' columns = list(
#' MPG.city = colDef(
#'  cell = lollipop(data,
#'  bar_color = c("#440154FF","#31688EFF","#35B779FF","#FDE725FF"),
#'  circle_color = "black")),
#' MPG.highway = colDef(cell = lollipop(data,
#'  bar_color = "black",
#'  circle_color = c("#440154FF","#31688EFF","#35B779FF","#FDE725FF")))))
#'
#' @export

lollipop <- function(data, circle_color = "#1e90ff", bar_color = "lightgrey", number_fmt = NULL, bright_values = TRUE) {

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    color_pal <- function(x) {

      if (!is.na(x))
        rgb(colorRamp(c(circle_color))(x), maxColorValue = 255)
      else
        NULL
    }

    assign_color <- function(x) {

      if (!is.na(x)) {
        rgb_sum <- rowSums(colorRamp(c(circle_color))(x))
        color <- ifelse(rgb_sum >= 375, "black", "white")
        color
      } else
        NULL
    }

    normalized <-
      (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

    fill_color <- color_pal(normalized)

    font_color <- assign_color(normalized)

    gradient <- paste0("linear-gradient(to right,", paste(bar_color, collapse = ", "))

    bar_chart <-
      function(label,
               width = "100%",
               height = "9px",
               color = NULL,
               fill = NULL,
               background = NULL) {

        circle_label <-
          htmltools::div(style = list(
            background = fill,
            color = color,
            borderRadius = "50%",
            border = "5px solid rgba(0, 0, 0, 0)"
          ), label)

        zero_bar <-
          htmltools::div(style = list(
            display = "flex",
            alignItems = "center",
            justifyContent = "flex-end",
            background = bar_color,
            backgroundImage = gradient,
            width = width,
            height = height,
            marginTop = "5px",
            borderTopRightRadius = "7px",
            borderBottomRightRadius = "7px",
            flexGrow = 1,
            transition = "width 1s"
          ),
          circle_label)

        htmltools::div(zero_bar)
      }

    width <- if (is.numeric(value)) {

      paste0(value / max(data[[name]], na.rm = TRUE) * 100, "%")

    } else if (!is.numeric(value))

      return(value)

    max_digits <- max(nchar(data[[name]]))+1

    chart_label <- stringr::str_pad(label, max_digits)

    if (bright_values == FALSE) {

      bar_chart(chart_label,
                width = width,
                fill = fill_color,
                background = bar_color)

    } else {

      bar_chart(chart_label,
                width = width,
                fill = fill_color,
                background = bar_color,
                color = font_color)

    }
  }
}
