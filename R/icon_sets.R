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
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @param icon_position Position of icon relative to numbers.
#'     Options are "left", "right", above", "below", or "over".
#'     Default is right.
#'
#' @import reactable
#'
#' @return a function that applies an icon
#'     to a column of numeric values.
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, icon_sets() assigns red circles to the lowest-third values,
#' ## orange circles to the middle-third values,
#' ## and green to the top-third values
#' reactable(data,
#' defaultColDef = colDef(cell = icon_sets(data)))
#'
#' ## Assign custom colors
#' reactable(data,
#' defaultColDef = colDef(cell = icon_sets(data,
#' colors = c("tomato", "grey", "dodgerblue"))))
#'
#' ## Assign icons from Font Awesome's icon library
#' reactable(data,
#' defaultColDef = colDef(cell = icon_sets(data,
#' icons = c("arrow-down","minus","arrow-up"))))
#'
#' ## Use number_fmt to format numbers using the scales package
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#' defaultColDef = colDef(cell = icon_sets(car_prices,
#' number_fmt = scales::dollar)))
#'
#'## Position icons relative to the numbers. Options are to the left, right, above, below, or over.
#' reactable(car_prices,
#' defaultColDef = colDef(cell = icon_sets(car_prices,
#' icon_position = "above")))
#'
#' @export


icon_sets <- function(data,
                      icons = c("circle"),
                      colors = c("red","orange","green"),
                      number_fmt = NULL,
                      icon_position = "right") {


  '%notin%' <- Negate('%in%')

  if (icon_position %notin% c("left", "right", "above", "below", "over") == TRUE) {

    stop("icon_position must be either 'left', 'right', 'above', 'below', or 'over'")
  }

  cell <- function(value, index, name) {

    if (!is.numeric(value) | is.na(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    icon_buckets <- dplyr::ntile(data[[name]], n = length(icons))

    icon_assign <- icon_buckets[index]

    color_buckets <- dplyr::ntile(data[[name]], n = length(colors))

    color_assign <- color_buckets[index]

    if (!is.null(label) & icon_position == "right") {

    htmltools::tagList(
        label,
        htmltools::div(style = list(display = "inline-block", marginLeft = "8px"),
                       htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                      style = paste("color:", colors[[color_assign]]))))

    } else if (!is.null(label) & icon_position == "left") {

    htmltools::tagList(
      htmltools::div(style = list(display = "inline-block", marginRight = "8px"),
                     htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                    style = paste("color:", colors[[color_assign]]))),
      label)

    } else if (!is.null(label) & icon_position == "below") {

      htmltools::tagList(
        htmltools::div(label),
        htmltools::div(style = list(display = "inline-block"),
                       htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                      style = paste("color:", colors[[color_assign]]))))

    } else if (!is.null(label) & icon_position == "above") {

      htmltools::tagList(
        htmltools::div(style = list(display = "inline-block"),
                       htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                      style = paste("color:", colors[[color_assign]]))),
        htmltools::div(label))

    } else if (!is.null(label) & icon_position == "over") {

      htmltools::div(style = list(display = "inline-block"),
                      htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                    style = paste("color:", colors[[color_assign]])))
    }
  }
}

