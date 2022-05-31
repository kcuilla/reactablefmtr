#' Add a trend indicator icon to cells in a column
#'
#' The `icon_trend_indicator()` function conditionally adds an up/down/no-change icon from the Font Awesome library (via shiny) to a column.
#'     There are four options available for the icons: angle-double, arrow, chevron, and arrow-circle.
#'     The icons can be positioned over, above, below, or to the right or left of the values.
#'     Three colors must be provided for the icons in order from down, no-change, to up.
#'     By default, the color of the text matches the colors of the corresponding icons, but can be changed within `text_color`.
#'     The text for values of no change (0) will automatically be hidden but can be shown with `show_zero_label`.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param icons The name of the icons to be displayed.
#'     Options are "angle-double", "arrow", "chevron", and "arrow-circle".
#'     Default is "arrow".
#'
#' @param colors The color(s) to assign to the icons.
#'     Three colors must be provided in order from down, no-change, to up.
#'     Default colors provided are blue-grey-orange: c("#15607A", "#B2B2B2", "#FA8C00").
#'
#' @param icon_position Position of icon relative to numbers.
#'     Options are "left", "right", above", "below", or "over".
#'     Default is right.
#'
#' @param icon_size A value representing the size of the icon in px.
#'     Default is 16.
#'
#' @param show_zero_label Logical: show or hide the text (0) next to no-change icons.
#'     Default is FALSE.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @param opacity A value between 0 and 1 that adjusts the opacity in colors.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param text_color The color of the text next to the icon.
#'     Default is the same color as the corresponding icon.
#'
#' @param bold_text Logical: bold text.
#'     Default is FALSE.
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
#' data <- data.frame(change = c(-0.2,0.0,0.9,-0.7,0.5))
#'
#' ## The default icons displayed are "arrow" with matching text color
#' reactable(data,
#' defaultColDef = colDef(cell = icon_trend_indicator(data)))
#'
#' ## Choose one of four icon options available
#' reactable(data,
#' defaultColDef = colDef(cell = icon_trend_indicator(data, icons = "chevron")))
#'
#' ## Change the color of the text next to the icons
#' reactable(data,
#' defaultColDef = colDef(cell = icon_trend_indicator(data, text_color = "black")))
#'
#' ## Change the position of the icons relative to the text
#' reactable(data,
#' defaultColDef = colDef(cell = icon_trend_indicator(data, icon_position = "left")))
#' @export

icon_trend_indicator <- function(data,
                                 icons = "arrow",
                                 colors = c("#15607A","#B2B2B2","#FA8C00"),
                                 icon_position = "right",
                                 icon_size = 16,
                                 show_zero_label = FALSE,
                                 number_fmt = NULL,
                                 opacity = 1,
                                 text_color = NULL,
                                 bold_text = FALSE,
                                 tooltip = FALSE,
                                 animation = "1s ease") {

  '%notin%' <- Negate('%in%')

  if (!is.null(icons) && icons %notin% c("angle-double","arrow","chevron","arrow-circle") == TRUE) {

    stop("icons must be either 'angle-double', 'arrow', 'chevron', 'arrow-circle'")
  }

  if (icon_position %notin% c("left", "right", "above", "below", "over") == TRUE) {

    stop("icon_position must be either 'left', 'right', 'above', 'below', or 'over'")
  }

  if (length(colors) != 3) {

    stop("must provide three colors within `colors`")
  }

  if (!is.numeric(opacity)) {

    stop("`opacity` must be numeric")
  }

  if (opacity < 0 | opacity > 1) {

    stop("`opacity` must be a value between 0 and 1")
  }

  if (!is.logical(show_zero_label)) {

    stop("`show_zero_label` must be TRUE or FALSE")
  }

  if (!is.logical(tooltip)) {

    stop("`tooltip` must be TRUE or FALSE")
  }

  if (!is.logical(bold_text)) {

    stop("`bold_text` must be TRUE or FALSE")
  }

  if (bold_text == TRUE) {

    bold_text <- "bold"

  } else bold_text <- "normal"

  cell <- function(value, index, name) {

    if (!is.numeric(value) | is.na(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)

    colors <- grDevices::adjustcolor(colors, alpha.f = opacity)

    if (value < 0) {

      icons <- paste0(icons, "-down")
      color_assign <- 1

    } else if (value > 0) {

      icons <- paste0(icons, "-up")
      color_assign <- 3

    } else {

      icons <- "minus"
      color_assign <- 2

      if (show_zero_label == FALSE) {
        label <- ""
      } else label <- label
    }

    if (!is.null(text_color)) {
      text_colors <- c(text_color,text_color,text_color)
    } else text_colors <- colors

    icon_label <- htmltools::tagAppendAttributes(
      shiny::icon(icons),
      style = paste0("font-size:", icon_size, "px", "; color:", colors[[color_assign]], sprintf("; transition: %s", animation)))

      ### icon_position
      if (icon_position == "right") {

        htmltools::tagList(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  htmltools::span(label,
                    style = list(
                      color = text_colors[[color_assign]],
                      fontWeight = bold_text)
                  )
                },
          htmltools::div(style = list(
            display = "inline-block",
            marginLeft = "8px"),
            icon_label))

      } else if (icon_position == "left") {

        htmltools::tagList(
          htmltools::div(
            style = list(
              display = "inline-block",
              marginRight = "8px"),
              icon_label),
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  htmltools::span(label,
                    style = list(
                      color = text_colors[[color_assign]],
                      fontWeight = bold_text)
                  )
                }
        )

      } else if (icon_position == "below") {

        htmltools::tagList(
          htmltools::div(
            style = list(
              display = "grid",
              position = "relative"),
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
                } else {
                  htmltools::span(label,
                    style = list(
                      color = text_colors[[color_assign]],
                      fontWeight = bold_text)
                  )
                }
          ),
          htmltools::div(
            style = list(
              display = "inline-block"),
              icon_label))

      } else if (icon_position == "above") {

        htmltools::tagList(
          htmltools::div(
            style = list(display = "inline-block"),
            icon_label),
          htmltools::div(style = list(
            display = "grid",
            position = "relative"),
              if (tooltip == TRUE) {
                tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
              } else {
                  htmltools::span(label,
                    style = list(
                      color = text_colors[[color_assign]],
                      fontWeight = bold_text)
                  )
              }
          ))

      } else if (!is.null(label) & icon_position == "over") {

        htmltools::tagList(
          htmltools::div(icon_label,
                         style = list(
                           position = "absolute",
                           display = "inline-block")),
          htmltools::div(style = list(
            color = "transparent",
            display = "grid",
            zIndex = "100",
            position = "relative"),
              if (tooltip == TRUE) {
                tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = tooltip_label)
              } else {
                  htmltools::span(label,
                    style = list(
                      color = text_colors[[color_assign]],
                      fontWeight = bold_text)
                  )
              }
          ))}
    }
}
