#'  Build a customizable bubble grid chart
#'
#' The `bubble_grid()` function creates a customizable bubble grid chart within a reactable table.
#'     The size of the bubbles are in relation to the values within each column - the bigger the value, the bigger the size of the bubble.
#'     There are two shapes available for the bubble grid: circles and squares, which can be specified with `shape`.
#'     The colors can be provided within a vector in `colors` or via another column in the dataset by referencing the column by name with `color_ref`.
#'     If more than one color is provided in `colors`, the colors will be assigned to the values from low to high within the palette.
#'     This is the default setting of `bubble_grid()`, which applies a blue-to-orange color palette to the bubbles. However, a singular color can be provided instead if desired.
#'     `bubble_grid()` can be applied to columns containing character data by referencing another column with numeric values in it with `color_by`.
#'     The opacity of the colors provided can be adjusted by providing a value between 0 and 1 in `opacity`.
#'     `text_color` can be used to change the color of the values displayed within the bubbles.
#'     If values are displayed within a dark-colored background, `brighten_text` will display the values in white text so they are more visible.
#'     For smaller values with a dark-colored background, the values may not be visible.
#'     If you would like these numbers to be visible, you could do so by either:
#'     A) setting `brighten_text` to FALSE and assigning a universal color to the text within `text_color`.
#'     B) leaving `brighten_text` as TRUE and setting `brighten_text_color` to a darker color other than the default white color.
#'     If the user wants to assign colors row-wise instead of column-wise, set `span` equal to TRUE to apply across all columns.
#'     Or can provide the names of the columns by either column name or column position number to apply to only a subset of the columns.
#'     The format of the numbers within the bubbles can be changed by defining the format from a package such as the {scales} package within `number_fmt`.
#'     `bubble_grid()` needs to placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param shape The shape of the bubbles.
#'     Options are 'circles' or 'squares'.
#'     Default is 'circles'.
#'
#' @param colors A vector of colors to color the bubbles.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are blue-white-orange: c("#15607A", "#FFFFFF", "#FA8C00").
#'     Can use R's built-in colors or other color packages.
#'
#' @param color_ref Optionally assign colors to from another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one color can be provided per row.
#'     Default is NULL.
#'
#' @param color_by Assign colors to a column based on the values of another column.
#'    The column in reference must contain numeric data.
#'    The column in which the colors are being assigned to can be either numerical or character.
#'    Default is NULL.
#'
#' @param min_value A value to use as the minimum value for the size of the bubbles.
#'     Default is NULL.
#'
#' @param max_value A value to use as the maximum value for the size of the bubbles.
#'     The default maximum value is the maximum value in the column.
#'     Default is NULL.
#'
#' @param opacity A value between 0 and 1 that adjusts the opacity in colors.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param bias A positive value that determines the spacing between multiple colors.
#'     A higher value spaces out the colors at the higher end more than a lower number.
#'     Default is 1.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
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
#' @param show_text Logical: show text or hide text.
#'     Default is TRUE.
#'
#' @param brighten_text Logical: automatically assign color to text based on background color of cell.
#'     Text within dark-colored backgrounds will turn white, text within light-colored backgrounds will be black.
#'     Default is TRUE.
#'
#' @param brighten_text_color Assigns text color to values if values are within a dark-colored backgrounds.
#'     Default is white.
#'
#' @param bold_text Logical: bold text.
#'     Default is FALSE.
#'
#' @param span Optionally apply colors to values across multiple columns instead of by each column.
#'     To apply across all columns set to TRUE.
#'     If applying to a set of columns, can provide either column names or column positions.
#'     Default is FALSE.
#'
#' @param box_shadow Logical: add a box shadow to the tiles.
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
#'     Animation can be applied to the size of the bubbles by setting it to "all 1s ease".
#'     Default is "background 1s ease".
#'
#' @return a function that builds a bubble grid chart
#'     to a column of values.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## By default, the bubble_grid() function uses a blue-white-orange three-color pattern:
#' reactable(
#'   data,
#'   columns = list(
#'     Petal.Length = colDef(
#'       align = "center",
#'       cell = bubble_grid(data))))
#'
#' ## You can specify your own color palette or a single color across all values with `colors`;
#' reactable(
#'   data,
#'   columns = list(
#'     Petal.Length = colDef(
#'       align = "center",
#'       cell = bubble_grid(data,
#'                          colors = c("orange")))))
#'
#' ## Use squares instead of circles:
#' reactable(
#'   data,
#'   columns = list(
#'     Petal.Length = colDef(
#'       align = "center",
#'       cell = bubble_grid(data,
#'                          shape = "squares"))))
#'
#' ## Hide text and show on hover by enabling the tooltip:
#' reactable(
#'   data,
#'   columns = list(
#'     Petal.Length = colDef(
#'       align = "center",
#'       cell = bubble_grid(data,
#'                          show_text = FALSE,
#'                          tooltip = TRUE))))
#'
#' ## Control the scale of the circles by adjusting the min and max values:
#' reactable(
#'   data,
#'   columns = list(
#'     Petal.Length = colDef(
#'       align = "center",
#'       cell = bubble_grid(data,
#'                          min_value = 1,
#'                          max_value = 2))))
#'
#' ## Use span to apply bubbles to values in relation to the entire data set:
#' reactable(
#'   data,
#'   defaultColDef = colDef(
#'     cell = bubble_grid(data,
#'                        span = TRUE)))
#'
#' ## Use number_fmt to format numbers using the scales package:
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#'   defaultColDef = colDef(
#'     align = "center",
#'     cell = bubble_grid(car_prices,
#'                        number_fmt = scales::dollar)))
#' @export

bubble_grid <- function(data,
                        shape = "circles",
                        colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                        color_ref = NULL,
                        color_by = NULL,
                        min_value = NULL,
                        max_value = NULL,
                        opacity = 1,
                        bias = 1,
                        number_fmt = NULL,
                        text_size = NULL,
                        text_color = "black",
                        text_color_ref = NULL,
                        show_text = TRUE,
                        brighten_text = TRUE,
                        brighten_text_color = "white",
                        bold_text = FALSE,
                        span = FALSE,
                        box_shadow = FALSE,
                        tooltip = FALSE,
                        animation = "background 1s ease") {

  '%notin%' <- Negate('%in%')

  if (!is.null(shape) && shape %notin% c("circles", "squares") == TRUE) {

    stop("`shape` must be either 'circles' or 'squares'")
  }

  if (!is.logical(bold_text)) {

    stop("`bold_text` must be TRUE or FALSE")
  }

  if (!is.logical(brighten_text)) {

    stop("`brighten_text` must be TRUE or FALSE")
  }

  if (!is.logical(box_shadow)) {

    stop("`box_shadow` must be TRUE or FALSE")
  }

  if (!is.logical(tooltip)) {

    stop("`tooltip` must be TRUE or FALSE")
  }

  if (!is.numeric(bias)) {

    stop("`bias` must be numeric")
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

  color_pal <- function(x) {

    if (!is.na(x))
      rgb(colorRamp(c(colors), bias = bias)(x), maxColorValue = 255)
    else
      NULL
  }

  assign_color <- function(x) {

    if (!is.na(x)) {
      rgb_sum <- rowSums(colorRamp(c(colors), bias = bias)(x))
      color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)
      color
    } else
      NULL
  }

  if (bold_text == TRUE) {

    bold_text <- "bold"

  } else bold_text <- "normal"

  if (box_shadow == TRUE) {

    box_shadow <- "0 6px 6px -4px #888888"

  } else box_shadow <- NULL

  cell <- function(value, index, name) {

    if (is.null(color_ref) & is.null(color_by) & !is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else {

      label <- number_fmt(value)

    }

    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)

    if (is.logical(span)) {

      if (span) {

        normalized <- (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))

        ### width of data_bars
        size <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {

          paste0(abs(value) / max(dplyr::select_if(data, is.numeric), na.rm = TRUE) * 100, "px")

          ### min_value provided
        } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {

          paste0((abs(value) - min_value) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min_value) * 100, "px")

          ### max_value provided
        } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {

          paste0((abs(value) / max_value) * 100, "px")

          ### min and max provided
        } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {

          paste0((abs(value) - min_value) / (max_value - min_value) * 100, "px")

        } else if (!is.numeric(value)) {

          return(value)
        }

      } else if (!is.null(color_ref)) {

        normalized <- dplyr::ntile(data[[name]], n = length(colors))

      } else {

      ### color_by
      if (is.character(color_by)) {

        # color_by column must be numeric
        if (all(color_by %in% names(which(sapply(data, is.numeric))))) {

          if (is.character(color_by)) { color_by <- which(names(data) %in% color_by) }

          # if there is no variance in the column, assign the same color to each value
          if (is.numeric(data[[color_by]]) & mean((data[[color_by]] - mean(data[[color_by]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

            normalized <- 1

          } else {

            normalized <- (data[[color_by]][index] - min(data[[color_by]], na.rm = TRUE)) / (max(data[[color_by]], na.rm = TRUE) - min(data[[color_by]], na.rm = TRUE))

          }

          cell_color <- color_pal(normalized)
          cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
          font_color <- assign_color(normalized)

          ### width of data_bars
          size <- if (is.numeric(data[[color_by]][index]) & is.null(max_value) & is.null(min_value)) {

            paste0(abs(data[[color_by]][index]) / max(abs(data[[color_by]]), na.rm = TRUE) * 100, "px")

            ### min_value provided
          } else if (is.numeric(data[[color_by]][index]) & is.null(max_value) & !is.null(min_value)) {

            paste0((abs(data[[color_by]][index]) - min_value) / (max(abs(data[[color_by]]), na.rm = TRUE) - min_value) * 100, "px")

            ### max_value provided
          } else if (is.numeric(data[[color_by]][index]) & !is.null(max_value) & is.null(min_value)) {

            paste0((abs(data[[color_by]][index]) / max_value) * 100, "px")

            ### min and max provided
          } else if (is.numeric(data[[color_by]][index]) & !is.null(max_value) & !is.null(min_value)) {

            paste0((abs(data[[color_by]][index]) - min_value) / (max_value - min_value) * 100, "px")

          }

        } else {

          stop("Attempted to select non-existing column or non-numeric column with color_by")
        }

      } else {

          # standard normalization (no variance check)
          if (is.numeric(value) & mean((data[[name]] - mean(data[[name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

            normalized <- 1

          } else {

            # standard normalization
            normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

          }

        cell_color <- color_pal(normalized)
        cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
        font_color <- assign_color(normalized)

        ### width of data_bars
        size <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {

          paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "px")

          ### min_value provided
        } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {

          paste0((abs(value) - min_value) / (max(abs(data[[name]]), na.rm = TRUE) - min_value) * 100, "px")

          ### max_value provided
        } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {

          paste0((abs(value) / max_value) * 100, "px")

          ### min and max provided
        } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {

          paste0((abs(value) - min_value) / (max_value - min_value) * 100, "px")

          }

        }

      }

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
          cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))

          rgb_sum <- rowSums(grDevices::colorRamp(c(cell_color), bias = bias)(1))

          font_color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)

        } else {

          stop("Attempted to select non-existing column or non-character column with fill_color_ref")
        }

      } else {

        cell_color <- color_pal(normalized)
        cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
        font_color <- assign_color(normalized)

      }

    } else if (is.numeric(span) | is.character(span)) {

      if (all(span %in% which(sapply(data, is.numeric))) | all(span %in% names(which(sapply(data, is.numeric))))) {

        if (is.character(span)) { span <- which(names(data) %in% span) }

        normalized <- (value - min(dplyr::select(data, !!span), na.rm = TRUE)) / (max(dplyr::select(data, !!span), na.rm = TRUE) - min(dplyr::select(data, !!span), na.rm = TRUE))
        cell_color <- if (name %in% colnames(data)[span]) { suppressWarnings(grDevices::adjustcolor(color_pal(normalized), alpha.f = opacity)) }
        font_color <- if (name %in% colnames(data)[span]) { assign_color(normalized) }

      } else {

        stop("Attempted to select non-existing or non-numeric columns with span")

      }

    }

    # adjust border radius based on shape
    if (shape == "circles") {
      radius <- "50%"
    } else radius <- NULL

    if (brighten_text == FALSE & show_text == TRUE) {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
           style = list(background = cell_color,
                        color = text_color,
                        display = "inline-flex",
                        justifyContent = "center",
                        alignItems = "center",
                        textAlign = "center",
                        height = size,
                        width = size,
                        borderRadius = radius,
                        fontWeight = bold_text,
                        boxShadow = box_shadow,
                        fontSize = text_size,
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     followCursor = TRUE,
                     tooltip = tooltip_label)
      )

      } else {

        htmltools::div(label,
          style = list(background = cell_color,
                       color = text_color,
                       display = "inline-flex",
                       justifyContent = "center",
                       alignItems = "center",
                       textAlign = "center",
                       height = size,
                       width = size,
                       borderRadius = radius,
                       fontWeight = bold_text,
                       boxShadow = box_shadow,
                       fontSize = text_size,
                       transition = animation))
      }

    } else if (brighten_text == TRUE & !is.null(text_color_ref) & show_text == TRUE) {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
           style = list(background = cell_color,
                        color = text_color,
                        display = "inline-flex",
                        justifyContent = "center",
                        alignItems = "center",
                        textAlign = "center",
                        height = size,
                        width = size,
                        borderRadius = radius,
                        boxShadow = box_shadow,
                        fontSize = text_size,
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     followCursor = TRUE,
                     tooltip = tooltip_label)
      )

      } else {

      htmltools::div(label,
        style = list(background = cell_color,
                     color = text_color,
                     display = "inline-flex",
                     justifyContent = "center",
                     alignItems = "center",
                     textAlign = "center",
                     height = size,
                     width = size,
                     borderRadius = radius,
                     boxShadow = box_shadow,
                     fontSize = text_size,
                     transition = animation))
      }

    } else if (brighten_text == FALSE & show_text == FALSE) {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
           style = list(background = cell_color,
                        display = "inline-flex",
                        justifyContent = "center",
                        alignItems = "center",
                        textAlign = "center",
                        height = size,
                        width = size,
                        borderRadius = radius,
                        color = "transparent",
                        boxShadow = box_shadow,
                        fontSize = text_size,
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     followCursor = TRUE,
                     tooltip = tooltip_label)
      )

      } else {

        htmltools::div(label,
          style = list(background = cell_color,
                       display = "inline-flex",
                       justifyContent = "center",
                       alignItems = "center",
                       textAlign = "center",
                       height = size,
                       width = size,
                       borderRadius = radius,
                       fontSize = 0,
                       boxShadow = box_shadow,
                       fontSize = text_size,
                       transition = animation))
      }

    } else if (brighten_text == TRUE & show_text == FALSE) {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
           style = list(background = cell_color,
                        display = "inline-flex",
                        justifyContent = "center",
                        alignItems = "center",
                        textAlign = "center",
                        height = size,
                        width = size,
                        borderRadius = radius,
                        color = "transparent",
                        boxShadow = box_shadow,
                        fontSize = text_size,
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     followCursor = TRUE,
                     tooltip = tooltip_label)
      )

      } else {

        htmltools::div(label,
          style = list(background = cell_color,
                       display = "inline-flex",
                       justifyContent = "center",
                       alignItems = "center",
                       textAlign = "center",
                       height = size,
                       width = size,
                       borderRadius = radius,
                       color = "transparent",
                       boxShadow = box_shadow,
                       fontSize = text_size,
                       transition = animation))
      }

    } else {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
          style = list(background = cell_color,
                       color = font_color,
                       display = "inline-flex",
                       justifyContent = "center",
                       alignItems = "center",
                       textAlign = "center",
                       height = size,
                       width = size,
                       borderRadius = radius,
                       boxShadow = box_shadow,
                       fontWeight = bold_text,
                       fontSize = text_size,
                       transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     followCursor = TRUE,
                     tooltip = tooltip_label)
      )

      } else {

      htmltools::div(label,
        style = list(background = cell_color,
                     color = font_color,
                     display = "inline-flex",
                     justifyContent = "center",
                     alignItems = "center",
                     textAlign = "center",
                     height = size,
                     width = size,
                     borderRadius = radius,
                     boxShadow = box_shadow,
                     fontWeight = bold_text,
                     fontSize = text_size,
                     transition = animation))

      }
    }
  }
}
