#' Surround text or numeric values in a colored pill button
#'
#' The `pill_buttons()` function surrounds text or numeric values in a column in a colored pill button.
#'     If `pill_buttons()` is applied to a column containing text, the color of the pill button can be provided via a single color can be provided within `color`
#'     or via another column in the dataset by referencing the column name with `color_ref` (conditionally applying colors to text).
#'     If `pill_buttons` is applied to a numeric column, either a single color or a vector of colors can be provided within `color`
#'     or the colors can also be assigned via `color_ref`.
#'     The opacity of the color(s) provided can be adjusted by providing a value between 0 and 1 in `opacity`.
#'     The color of the text/values can be changed using `text_color`.
#'     If text/values are displayed within a dark-colored background, `brighten_text` will display the text/values in white so they are more visible.
#'     The color of `brighten_text_color` can be changed to a color other than white if desired.
#'     The horizontal alignment of `pill_buttons()` can be controlled using reactable::colDef(align = "center").
#'     `pill_buttons()` needs to placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing either a text or numeric column.
#'
#' @param colors The background color of the pill button.
#'     Only a single color can be provided for columns containing text.
#'     Multiple colors can be provided for columns containing values and
#'     should be given in order from low values to high values.
#'     If multiple colors are provided for columns containing text,
#'     the first color in the vector will be assigned to the text.
#'     The default color provided is "#15607A".
#'     Can use R's built-in colors or other color packages.
#'
#' @param color_ref Optionally assign colors to from another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one color can be provided per row.
#'     Default is NULL.
#'
#' @param opacity A value between 0 and 1 that adjusts the opacity in color(s).
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
#'
#' @param show_text Logical: show text or hide text.
#'     Default is TRUE.
#'
#' @param text_size Numeric value representing the size of the text labels.
#'     Default is NULL.
#'
#' @param text_color Assigns text color to values.
#'     Default is black.
#'
#' @param text_color_ref Optionally assign text color from another column
#'     by providing the name of the column containing the text colors in quotes.
#'     Only one color can be provided per cell.
#'     Default is NULL.
#'
#' @param brighten_text Logical: automatically assign color to text based on background color of the pill button.
#'     Text within dark-colored backgrounds will turn white, text within light-colored backgrounds will be black.
#'     Default is TRUE.
#'
#' @param brighten_text_color Assigns text color to values if values are within a dark-colored pill button backgrounds.
#'     Default is white.
#'
#' @param bold_text Logical: bold text.
#'     Default is FALSE.
#'
#' @param box_shadow Logical: add a box shadow to the buttons.
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
#'     Default is "background 1s ease".
#'
#' @return a function that surrounds text/values in a column
#'     with a colored pill button.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' library(dplyr)
#' data <- iris[45:54, ]
#'
#' ## Surround text with pill buttons:
#' reactable(
#' data,
#' columns = list(
#' Species = colDef(cell = pill_buttons(data))))
#'
#' ## Conditionally apply colors from another column:
#' data %>%
#' mutate(color_assign = case_when(
#' Species == "setosa" ~ "red",
#' Species == "versicolor" ~ "forestgreen",
#' TRUE ~ "grey")) %>%
#' reactable(.,
#' columns = list(
#' Species = colDef(cell = pill_buttons(., color_ref = "color_assign"))))
#'
#' ## Surround numeric values with pill buttons:
#' reactable(
#' data,
#' columns = list(
#' Petal.Width = colDef(cell = pill_buttons(data))))
#'
#' ## Apply multiple colors to numeric values:
#' reactable(
#' data,
#' columns = list(
#' Petal.Width = colDef(
#' cell = pill_buttons(data,
#'                     colors = c("lightpink","white","lightgreen"),
#'                     opacity = 0.3))))
#'
#' ## Apply a box shadow to the buttons to give a 3-D effect:
#' reactable(
#' data,
#' columns = list(
#' Petal.Width = colDef(
#' cell = pill_buttons(data,
#'                     box_shadow = TRUE,
#'                     colors = c("lightpink","white","lightgreen"),
#'                     opacity = 0.3))))
#'
#' @export

pill_buttons <- function(data,
                         colors = "#15607A",
                         color_ref = NULL,
                         opacity = 1,
                         number_fmt = NULL,
                         show_text = TRUE,
                         text_size = NULL,
                         text_color = "black",
                         text_color_ref = NULL,
                         brighten_text = TRUE,
                         brighten_text_color = "white",
                         bold_text = FALSE,
                         box_shadow = FALSE,
                         tooltip = FALSE,
                         animation = "background 1s ease") {

  if (!is.logical(bold_text)) {

    stop("`bold_text` must be TRUE or FALSE")
  }

  if (!is.logical(brighten_text)) {

    stop("`brighten_text` must be TRUE or FALSE")
  }

  if (!is.logical(show_text)) {

    stop("`show_text` must be TRUE or FALSE")
  }

  if (!is.logical(box_shadow)) {

    stop("`box_shadow` must be TRUE or FALSE")
  }

  if (!is.numeric(opacity)) {

    stop("`opacity` must be numeric")
  }

  if (opacity < 0 | opacity > 1) {

    stop("`opacity` must be a value between 0 and 1")
  }

  if (length(text_color) > 1) {

    stop("multiple colors detected in `text_color`. only one color can be used.")
  }

  if (length(brighten_text_color) > 1) {

    stop("multiple colors detected in `brighten_text_color` only one color can be used.")
  }

  if (box_shadow == TRUE) {

    box_shadow <- "0 6px 6px -4px #888888"

  } else box_shadow <- NULL

  cell <- function(value, index, name) {

    if (!is.null(number_fmt) && is.numeric(value)) {

      label <- number_fmt(value)

    } else {

      label <- value

    }

    if (bold_text == TRUE) {

      bold_text <- "bold"

    } else bold_text <- "normal"

    if (!is.numeric(value) && length(colors) > 1) {

      colors <- colors[[1]]

    } else { colors <- colors }

      ### conditional text color
      if (is.character(text_color_ref)) {

        if (all(text_color_ref %in% names(which(sapply(data, is.character))))) {

          if (is.character(text_color_ref)) { text_color_ref <- which(names(data) %in% text_color_ref) }

          font_color <- data[[text_color_ref]][index]
          text_color <- data[[text_color_ref]][index]

        } else {

          stop("Attempted to select non-existing column or non-character column with text_color_ref")
        }

      } else {

         font_color <- text_color
      }

    ### conditional fill color and font color
    if (is.character(color_ref)) {

      if (all(color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(color_ref)) { color_ref <- which(names(data) %in% color_ref) }

        cell_color <- data[[color_ref]][index]
        cell_color <- grDevices::adjustcolor(cell_color, alpha.f = opacity)
        rgb_sum <- rowSums(grDevices::colorRamp(c(cell_color))(1))
        font_color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)

      } else {

        stop("Attempted to select non-existing column or non-character column with color_ref")
      }

    } else if (is.numeric(value)) {

        color_pal <- function(x) {

          if (!is.na(x))
            rgb(colorRamp(c(colors))(x), maxColorValue = 255)
          else
            NULL
        }

        assign_color <- function(x) {

          if (!is.na(x)) {
            rgb_sum <- rowSums(colorRamp(c(colors))(x))
            colors <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)
            colors
          } else
            NULL
        }

        ### normalization for color palette
        if (is.numeric(value) & mean((data[[name]] - mean(data[[name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

          normalized <- 1

        } else {

          normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

        }

        cell_color <- color_pal(normalized)
        cell_color <- grDevices::adjustcolor(cell_color, alpha.f = opacity)
        font_color <- assign_color(normalized)

      } else {

      cell_color <- grDevices::adjustcolor(colors, alpha.f = opacity)
      rgb_sum <- rowSums(grDevices::colorRamp(c(cell_color))(1))
      font_color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)

    }

    if (brighten_text == FALSE & show_text == TRUE) {

      htmltools::div(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = label)
                } else {
                  label
                },
                     style = list(background = cell_color,
                                  color = text_color,
                                  boxShadow = box_shadow,
                                  display = "inline-block",
                                  padding = "2px 12px",
                                  borderRadius = "15px",
                                  fontWeight = bold_text,
                                  fontSize = text_size,
                                  transition = animation))

    } else if (brighten_text == TRUE & !is.null(text_color_ref) & show_text == TRUE) {

      htmltools::div(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = label)
                } else {
                  label
                },
                     style = list(background = cell_color,
                                  color = text_color,
                                  boxShadow = box_shadow,
                                  display = "inline-block",
                                  padding = "2px 12px",
                                  borderRadius = "15px",
                                  fontWeight = bold_text,
                                  fontSize = text_size,
                                  transition = animation))

    } else if (brighten_text == FALSE & show_text == FALSE) {

      htmltools::div(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = label)
                } else {
                  label
                },
                     style = list(background = cell_color,
                                  color = "transparent",
                                  boxShadow = box_shadow,
                                  display = "inline-block",
                                  padding = "2px 12px",
                                  borderRadius = "15px",
                                  fontSize = text_size,
                                  transition = animation))

    } else if (brighten_text == TRUE & show_text == FALSE) {

      htmltools::div(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = label)
                } else {
                  label
                },
                     style = list(background = cell_color,
                                  color = "transparent",
                                  boxShadow = box_shadow,
                                  display = "inline-block",
                                  padding = "2px 12px",
                                  borderRadius = "15px",
                                  fontSize = text_size,
                                  transition = animation))

    } else {

      htmltools::div(
                if (tooltip == TRUE) {
                  tippy::tippy(label, animateFill = FALSE, followCursor = TRUE, tooltip = label)
                } else {
                  label
                },
                     style = list(background = cell_color,
                                  color = font_color,
                                  boxShadow = box_shadow,
                                  display = "inline-block",
                                  padding = "2px 12px",
                                  borderRadius = "15px",
                                  fontWeight = bold_text,
                                  fontSize = text_size,
                                  transition = animation))
    }
  }
}
