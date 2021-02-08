#' Add colored icons to rows in a column
#'
#' The `icon_sets()` function conditionally adds an icon from the Font Awesome library (via shiny) to each row of a column and assigns a color depending on their value in relation to other values in that particular column.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param icons A vector of three icons from the Font Awesome library (via shiny).
#'     Icons should be given in order from low values to high values.
#'     Default icons are circles.
#'
#' @param colors A vector of three colors to color the icons.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are c('red','orange','green').
#'     Can use R's built-in colors or other color packages.
#'
#' @export
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, icon_sets() assigns red circles to the lowest-third values, orange circles to the middle-third values, and green to the top-third values
#' reactable(data, defaultColDef = colDef(cell = icon_sets(data)))
#'
#' ## Assign custom colors
#' reactable(data, defaultColDef = colDef(cell = icon_sets(data, colors = c("tomato", "grey", "dodgerblue"))))
#'
#' ## Assign icons from Font Awesome's icon library
#' reactable(data, defaultColDef = colDef(cell = icon_sets(data, icons = c("arrow-down","minus","arrow-up"))))


icon_sets <- function(data, icons = c("circle","circle","circle"), colors = c("red","orange","green")) {

  if (length(icons) != 3) {

    stop("must provide three icons Ex. icons = c('arrow-down','minus','arrow-up')")
  }

  if (length(colors) != 3) {

    stop("must provide three colors. Ex. colors = c('red','grey','blue')")
  }

  cell <- function(value, index, name) {

    if (!is.numeric(value) | is.na(value)) return(value)

    normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

    if (normalized >= 0.66667) {

      htmltools::tagList(
        value,
        htmltools::div(style = list(display = "inline-block", marginLeft = "8px"),
            htmltools::tagAppendAttributes(shiny::icon(icons[[3]]),
                                style = paste("color:", colors[[3]])))
      )

    } else if (normalized <= 0.66666 & normalized >= 0.33333) {

      htmltools::tagList(
        value,
        htmltools::div(style = list(display = "inline-block", marginLeft = "8px"),
            htmltools::tagAppendAttributes(shiny::icon(icons[[2]]),
                                style = paste("color:", colors[[2]])))
      )

    } else {
      htmltools::tagList(
        value,
        htmltools::div(style = list(display = "inline-block", marginLeft = "8px"),
            htmltools::tagAppendAttributes(shiny::icon(icons[[1]]),
                                style = paste("color:", colors[[1]])))
      )
    }
  }
}
