#' Add colored icons to cells in a column
#'
#' The `icon_sets()` function conditionally adds an icon from the Font Awesome library (via shiny) to each cell of a column
#'     and assigns a color depending on their value in relation to other values in that particular column.
#'     Any number of icons and any number of colors can be used.
#'     The number of icons and colors determines how the values are shown from low values to high values.
#'     The icons can be positioned over, above, below, or to the right or left of the values.
#'     The size of the icon can be adjusted.
#'     Icons and icon colors can be provided via another reference column in the dataset which is useful when assigning icons/colors to particular occurrences.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param icons A vector of three icons from the Font Awesome library (via shiny).
#'     Icons should be given in order from low values to high values.
#'     Default icons are circles.
#'
#' @param icon_set Apply a pre-selected set of icons to values.
#'     Options are "ski rating", "medals", and "batteries".
#'     Default is NULL.
#'
#' @param colors The color(s) to assign to the icons.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are blue-grey-orange: c("#15607A", "#B0B0B0", "#FA8C00").
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
#' @param tooltip Logical: hover tooltip.
#'     Default is FALSE.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "1s ease".
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
#' ## Apply pre-set icon sets with icon_set:
#' reactable(data,
#' defaultColDef = colDef(cell = icon_sets(data,
#' icon_set = 'ski rating')))
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
                      icon_set = NULL,
                      colors = c("#15607A", "#B0B0B0", "#FA8C00"),
                      opacity = 1,
                      icon_position = "right",
                      icon_ref = NULL,
                      icon_size = 16,
                      icon_color_ref = NULL,
                      number_fmt = NULL,
                      tooltip = FALSE,
                      animation = "1s ease") {

  '%notin%' <- Negate('%in%')

  if (!is.null(icon_set) && icon_set %notin% c("ski rating", "medals", "batteries") == TRUE) {

    stop("icon_set must be either 'ski rating', 'medals', or 'batteries'")
  }

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

    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)

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

        if (is.null(icon_ref) & is.null(icon_color_ref) & is.null(icon_set)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]], sprintf("; transition: %s", animation)))

        } else if (!is.null(icon_ref) & is.null(icon_color_ref) & is.null(icon_set)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]], sprintf("; transition: %s", animation)))

        } else if (is.null(icon_ref) & !is.null(icon_color_ref) & is.null(icon_set)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors, sprintf("; transition: %s", animation)))

        } else if (!is.null(icon_ref) & !is.null(icon_color_ref) & is.null(icon_set)) {

          icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                       style = paste0("font-size:", icon_size, "px", "; color:", colors, sprintf("; transition: %s", animation)))

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


      if (is.null(icon_ref) & is.null(icon_color_ref) & is.null(icon_set)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]], sprintf("; transition: %s", animation)))

      } else if (!is.null(icon_ref) & is.null(icon_color_ref) & is.null(icon_set)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]], sprintf("; transition: %s", animation)))

      } else if (is.null(icon_ref) & !is.null(icon_color_ref) & is.null(icon_set)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons[[icon_assign]]),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors, sprintf("; transition: %s", animation)))

      } else if (!is.null(icon_ref) & !is.null(icon_color_ref) & is.null(icon_set)) {

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icons),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors, sprintf("; transition: %s", animation)))

      } else {

        if (icon_set == "ski rating") {

         icon_buckets <- dplyr::ntile(data[[name]], n = 4)

         icon_assign <- icon_buckets[index]

         icon_label <- if (icon_assign == 1) {

               circle <- htmltools::tagAppendAttributes(shiny::icon("circle"),
                     style = paste0("font-size:", "16", "px", "; color:", "#39b54a", sprintf("; transition: %s", "color 1s ease")))

         } else if (icon_assign == 2) {

               square <- htmltools::tagAppendAttributes(shiny::icon("square-full"),
                     style = paste0("font-size:", "16", "px", "; color:", "#0f75bc", sprintf("; transition: %s", "color 1s ease")))

         } else if (icon_assign == 3) {

               diamond <- htmltools::tagAppendAttributes(shiny::icon("square"),
                     style = paste0("transform: rotate(45deg); font-size:", "16", "px", "; color:", "#000000", sprintf("; transition: %s", "color 1s ease")))

         } else {

               double_diamond <- list(htmltools::tagAppendAttributes(shiny::icon("square"),
                     style = paste0("transform: rotate(45deg); font-size:", "16", "px", "; color:", "#000000", sprintf("; transition: %s", "color 1s ease"))),
                     htmltools::tagAppendAttributes(shiny::icon("square"),
                     style = paste0("transform: rotate(45deg); font-size:", "16", "px", "; color:", "#000000", sprintf("; transition: %s", "color 1s ease"))))
         }

        } else if (icon_set == "batteries") {

         icon_buckets <- dplyr::ntile(data[[name]], n = 4)

         icon_assign <- icon_buckets[index]

         icon_label <- if (icon_assign == 1) {

             quarter <- htmltools::tagAppendAttributes(shiny::icon("battery-quarter"),
                   style = paste0("font-size:16px; color:#d7191c"))

         } else if (icon_assign == 2) {

             half <- htmltools::tagAppendAttributes(shiny::icon("battery-half"),
                   style = paste0("font-size:16px; color:#fdae61"))

         } else if (icon_assign == 3) {

             three_quarters <- htmltools::tagAppendAttributes(shiny::icon("battery-three-quarters"),
                   style = paste0("font-size:16px; color:#a6d96a"))

         } else {

             full <- htmltools::tagAppendAttributes(shiny::icon("battery-full"),
                   style = paste0("font-size:16px; color:#1a9641"))

         }

        } else if (icon_set == "medals") {

         icon_buckets <- dplyr::ntile(data[[name]], n = 3)

         icon_assign <- icon_buckets[index]

         icon_label <- if (icon_assign == 1) {

             bronze <- htmltools::tagAppendAttributes(shiny::icon("medal"),
                   style = paste0("font-size:", "16", "px", "; color:", "#A77044", sprintf("; transition: %s", "color 1s ease")))

         } else if (icon_assign == 2) {

             silver <- htmltools::tagAppendAttributes(shiny::icon("medal"),
                   style = paste0("font-size:", "16", "px", "; color:", "#D7D7D7", sprintf("; transition: %s", "color 1s ease")))

         }  else {

             gold <- htmltools::tagAppendAttributes(shiny::icon("medal"),
                   style = paste0("font-size:", "16", "px", "; color:", "#D6AF36", sprintf("; transition: %s", "color 1s ease")))
          }
        }
      }

      ### icon_position
      if (icon_position == "right") {

        htmltools::tagList(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  label
                },
          htmltools::div(style = list(display = "inline-block",
                                      marginLeft = "8px"),
                         icon_label))

      } else if (icon_position == "left") {

        htmltools::tagList(
          htmltools::div(style = list(display = "inline-block",
                                      marginRight = "8px"),
                         icon_label),
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  label
                }
        )

      } else if (icon_position == "below") {

        htmltools::tagList(
          htmltools::div(style = list(display = "grid",
                                      position = "relative"),
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  label
                }
          ),
          htmltools::div(style = list(display = "inline-block"),
                         icon_label))

      } else if (icon_position == "above") {

        htmltools::tagList(
          htmltools::div(style = list(display = "inline-block"),
                         icon_label),
          htmltools::div(style = list(display = "grid",
                                      position = "relative"),
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  label
                }
          ))

      } else if (!is.null(label) & icon_position == "over") {

        htmltools::tagList(
          htmltools::div(icon_label,
                         style = list(position = "absolute",
                                      display = "inline-block")),
          htmltools::div(style = list(color = "transparent",
                                      display = "grid",
                                      zIndex = "100",
                                      position = "relative"),
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  label
                }
          ))}

    } else icon_label <- NULL

  }
}
