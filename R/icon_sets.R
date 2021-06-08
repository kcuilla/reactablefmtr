#' Add colored icons to cells in a column
#'
#' The `icon_sets()` function conditionally adds an icon from the Font Awesome library (via shiny) to each cell of a column
#'     and assigns a color depending on their value in relation to other values in that particular column.
#'     Any number of icons and any number of colors can be used.
#'     The number of icons and colors determines how the values are shown from low values to high values.
#'     The icons can be positioned over, above, below, or to the right or left of the values.
#'     The size of the icon can be adjusted.
#'     Icons and icon colors can be provided via another reference column in the dataset which is useful when assigning icons/colors to particular occurences.
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
#'     Default colors provided are blue-grey-orange: c("#67a9cf","#808080","#ef8a62").
#'     Can use R's built-in colors or other color packages.
#'
#' @param opacity A value between 0 and 1 that adjusts the opacity in colors.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param icon_position Position of icon relative to numbers.
#'     Options are "left", "right", above", "below", or "over".
#'     Default is right.
#'
#' @param icon_ref Optionally assign icons from another column
#'     by providing the name of the column containing the icons in quotes.
#'     Only one icon can be provided per cell.
#'     Default is NULL.
#'
#' @param icon_size A value representing the size of the icon in px.
#'     Default is 16.
#'
#' @param icon_color_ref Optionally assign color to the icons from another column
#'     by providing the name of the column containing the icon colors in quotes.
#'     Only one color can be provided per cell.
#'     Default is NULL.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @import reactable
#'
#' @return a function that applies an icon
#'     to a column of numeric values.
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, icon_sets() assigns blue circles to the lowest-third values,
#' ## grey circles to the middle-third values,
#' ## and orange to the top-third values
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
                      colors = c("#67a9cf", "#808080", "#ef8a62"),
                      opacity = 1,
                      icon_position = "right",
                      icon_ref = NULL,
                      icon_size = 16,
                      icon_color_ref = NULL,
                      number_fmt = NULL) {


  '%notin%' <- Negate('%in%')

  if (icon_position %notin% c("left", "right", "above", "below", "over") == TRUE) {

    stop("icon_position must be either 'left', 'right', 'above', 'below', or 'over'")
  }

  if (!is.numeric(opacity)) {

    stop("`opacity` must be numeric")
  }

  if (opacity < 0 | opacity > 1) {

    stop("`opacity` must be a value between 0 and 1")
  }

  cell <- function(value, index, name) {

    if (is.null(icon_ref) & (!is.numeric(value) | is.na(value))) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    icon_buckets <- dplyr::ntile(data[[name]], n = length(icons))

    icon_assign <- icon_buckets[index]

    color_buckets <- dplyr::ntile(data[[name]], n = length(colors))

    color_assign <- color_buckets[index]

    ### icon_ref
    if (is.character(icon_ref)) {

      if (all(icon_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(icon_ref)) { icon_ref <- which(names(data) %in% icon_ref) }

        icons <- data[[icon_ref]][index]

        ### icon_color_ref
        if (is.character(icon_color_ref)) {
          if (all(icon_color_ref %in% names(which(sapply(data, is.character))))) {

            if (is.character(icon_color_ref)) { icon_color_ref <- which(names(data) %in% icon_color_ref) }

            colors <- data[[icon_color_ref]][index]

          } else { stop("Attempted to select non-existing column or non-character column with icon_color_ref") }
        }

        colors <- grDevices::adjustcolor(colors, alpha.f = opacity)

        if (is.null(icon_ref) & is.null(icon_color_ref)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]]))

        } else if (!is.null(icon_ref) & is.null(icon_color_ref)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]]))

        } else if (is.null(icon_ref) & !is.null(icon_color_ref)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors))

        } else if (!is.null(icon_ref) & !is.null(icon_color_ref)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors))

        }

        ### icon_position
        if (icon_position == "right") {

          htmltools::tagList(
            label,
            htmltools::div(style = list(display = "inline-block", marginLeft = "8px"),
                           icon_label))

        } else if (icon_position == "left") {

          htmltools::tagList(
            htmltools::div(style = list(display = "inline-block", marginRight = "8px"),
                           icon_label),
            label)

        } else if (icon_position == "below") {

          htmltools::tagList(
            htmltools::div(label),
            htmltools::div(style = list(display = "inline-block"),
                           icon_label))

        } else if (icon_position == "above") {

          htmltools::tagList(
            htmltools::div(style = list(display = "inline-block"),
                           icon_label),
            htmltools::div(label))

        } else if (!is.null(label) & icon_position == "over") {

          htmltools::div(style = list(display = "inline-block"),
                         icon_label)
        }

      } else {

        stop("Attempted to select non-existing column or non-character column with icon_ref")

      }

    } else if (!is.null(icons)) {

      ### icon_color_ref
      if (is.character(icon_color_ref)) {
        if (all(icon_color_ref %in% names(which(sapply(data, is.character))))) {
          if (is.character(icon_color_ref)) { icon_color_ref <- which(names(data) %in% icon_color_ref) }

          colors <- data[[icon_color_ref]][index]

        } else { stop("Attempted to select non-existing column or non-character column with icon_color_ref") }
      }

      colors <- grDevices::adjustcolor(colors, alpha.f = opacity)


      if (is.null(icon_ref) & is.null(icon_color_ref)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]]))

      } else if (!is.null(icon_ref) & is.null(icon_color_ref)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]]))

      } else if (is.null(icon_ref) & !is.null(icon_color_ref)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors))

      } else if (!is.null(icon_ref) & !is.null(icon_color_ref)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors))

      }

      ### icon_position
      if (icon_position == "right") {

        htmltools::tagList(
          label,
          htmltools::div(style = list(display = "inline-block", marginLeft = "8px"),
                         icon_label))

      } else if (icon_position == "left") {

        htmltools::tagList(
          htmltools::div(style = list(display = "inline-block", marginRight = "8px"),
                         icon_label),
          label)

      } else if (icon_position == "below") {

        htmltools::tagList(
          htmltools::div(label),
          htmltools::div(style = list(display = "inline-block"),
                         icon_label))

      } else if (icon_position == "above") {

        htmltools::tagList(
          htmltools::div(style = list(display = "inline-block"),
                         icon_label),
          htmltools::div(label))

      } else if (!is.null(label) & icon_position == "over") {

        htmltools::div(style = list(display = "inline-block"),
                       icon_label)
      }

    } else icon_label <- NULL

  }
}
