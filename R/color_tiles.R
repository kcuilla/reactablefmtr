#' Add color tiles to cells in a column
#'
#' The `color_tiles()` function conditionally colors the background of each cell similarly to color_scales().
#'     The difference is that color_tiles() uses round colored tiles around values instead of the entire background of the cell.
#'     Another difference is color_tiles() allows number formatting with number_fmt whereas color_scales() does not.
#'     The colors can be provided within a vector in `colors` or via another column in the dataset by referencing the column by name with `color_ref`.
#'     The opacity of the colors provided can be adjusted by providing a value between 0 and 1 in `opacity`.
#'     `text_color` can be used to change the color of the values.
#'     If values are displayed within a dark-colored background, `brighten_text` will display the values in white text so they are more visible.
#'     The color of `brighten_text_color` can be changed to a color other than white if desired.
#'     If the user wants to assign colors row-wise instead of column-wise, set `span` equal to TRUE to apply across all columns.
#'     Or can provide the names of the columns by either column name or column position number to apply to only a subset of the columns.
#'     `color_tiles()` needs to placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A vector of colors to color the cells.
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
#' @param opacity A value between 0 and 1 that adjusts the opacity in colors.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param bias A positive value that determines the spacing between multiple colors.
#'     A higher value spaces out the colors at the higher end more than a lower number.
#'     Default is 1.
#'
#' @param min_value The minimum value used for the color assignments.
#'     This value must expand the range of the data within the column.
#'     Therefore, the value must be less than or equal to the minimum value within the column.
#'     Default is NULL.
#'
#' @param max_value The maximum value used for the color assignments.
#'     This value must expand the range of the data within the column.
#'     Therefore, the value must be greater than or equal to the maximum value within the column.
#'     Default is NULL.
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
#'     Default is "background 1s ease".
#'
#' @return a function that applies conditional color tiles
#'     to a column of numeric values.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## By default, the colors_tiles() function uses a blue-white-orange three-color pattern
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(cell = color_tiles(data))))
#'
#' ## If only two colors are desired,
#' ## you can specify them with colors = 'c(color1, color2)';
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(cell = color_tiles(data,
#'  colors = c("red", "green")))))
#'
#' ## Use span to apply colors to values in relation to the entire dataset
#' reactable(data,
#' defaultColDef = colDef(cell = color_tiles(data, span = TRUE)))
#'
#' ## Span can take column names
#' reactable(data,
#' defaultColDef = colDef(cell = color_tiles(data, span = c("Sepal.Length", "Sepal.Width"))))
#'
#' ## Or it can also take column positions instead
#' reactable(data,
#' defaultColDef = colDef(cell = color_tiles(data, span = 1:2)))
#'
#' ## Use number_fmt to format numbers using the scales package
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#' defaultColDef = colDef(cell = color_tiles(car_prices,
#' number_fmt = scales::dollar)))
#' @export

color_tiles <- function(data,
                        colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                        color_ref = NULL,
                        color_by = NULL,
                        opacity = 1,
                        bias = 1,
                        min_value = NULL,
                        max_value = NULL,
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

  if (!is.null(min_value) & !is.numeric(min_value)) {

    stop("`min_value` must be numeric")
  }

  if (!is.null(max_value) & !is.numeric(max_value)) {

    stop("`max_value` must be numeric")
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

      # user supplied min and max values
      if (is.null(min_value)) {
      min_value_span <- min(dplyr::select_if(data, is.numeric), na.rm = TRUE)
      } else { min_value_span <- min_value }

      if (is.null(max_value)) {
      max_value_span <- max(dplyr::select_if(data, is.numeric), na.rm = TRUE)
      } else { max_value_span <- max_value }

      if (span) {

        normalized <- (value - min_value_span) / (max_value_span - min_value_span)

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

            # user supplied min and max values
            if (is.null(min_value)) {
            min_value_color_by <- min(data[[color_by]], na.rm = TRUE)
            } else { min_value_color_by <- min_value }

            if (is.null(max_value)) {
            max_value_color_by <- max(data[[color_by]], na.rm = TRUE)
            } else { max_value_color_by <- max_value }

            normalized <- (data[[color_by]][index] - min_value_color_by) / (max_value_color_by - min_value_color_by)

          }

          cell_color <- color_pal(normalized)
          cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
          font_color <- assign_color(normalized)

        } else {

          stop("Attempted to select non-existing column or non-numeric column with color_by")
        }

      } else {

          # standard normalization (no variance check)
          if (is.numeric(value) & mean((data[[name]] - mean(data[[name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

            normalized <- 1

          } else {

            # user supplied min and max values
            if (is.null(min_value)) {
            min_value_normal <- min(data[[name]], na.rm = TRUE)
            } else { min_value_normal <- min_value }

            if (is.null(max_value)) {
            max_value_normal <- max(data[[name]], na.rm = TRUE)
            } else { max_value_normal <- max_value }

            # standard normalization
            normalized <- (value - min_value_normal) / (max_value_normal - min_value_normal)

          }

            if (!is.null(min_value) & isTRUE(min_value > min(data[[name]], na.rm = TRUE))) {

              stop("`min_value` must be less than the minimum value observed in the data")
            }

            if (!is.null(max_value) & isTRUE(max_value < max(data[[name]], na.rm = TRUE))) {

              stop("`max_value` must be greater than the maximum value observed in the data")
            }

        cell_color <- color_pal(normalized)
        cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
        font_color <- assign_color(normalized)

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

        # user supplied min and max values
        if (is.null(min_value)) {
        min_value_span2 <- min(dplyr::select(data, !!span), na.rm = TRUE)
        } else { min_value_span2 <- min_value }

        if (is.null(max_value)) {
        max_value_span2 <- max(dplyr::select(data, !!span), na.rm = TRUE)
        } else { max_value_span2 <- max_value }

        normalized <- (value - min_value_span2) / (max_value_span2 - min_value_span2)

        cell_color <- if (name %in% colnames(data)[span]) { suppressWarnings(grDevices::adjustcolor(color_pal(normalized), alpha.f = opacity)) }
        font_color <- if (name %in% colnames(data)[span]) { assign_color(normalized) }

      } else {

        stop("Attempted to select non-existing or non-numeric columns with span")

      }

    }

    if (brighten_text == FALSE & show_text == TRUE) {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
           style = list(background = cell_color,
                        color = text_color,
                        display = "flex",
                        flexDirection = "column",
                        justifyContent = "center",
                        alignItems = "center",
                        borderRadius = "6px",
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
                       display = "flex",
                       flexDirection = "column",
                       justifyContent = "center",
                       alignItems = "center",
                       borderRadius = "6px",
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
                        display = "flex",
                        justifyContent = "center",
                        height = "18px",
                        borderRadius = "6px",
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
                     display = "flex",
                     justifyContent = "center",
                     height = "18px",
                     borderRadius = "6px",
                     boxShadow = box_shadow,
                     fontSize = text_size,
                     transition = animation))
      }

    } else if (brighten_text == FALSE & show_text == FALSE) {

      if (tooltip == TRUE) {

      htmltools::tagAppendChild(
        htmltools::div(
           style = list(background = cell_color,
                        display = "flex",
                        justifyContent = "center",
                        height = "18px",
                        borderRadius = "6px",
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
                       display = "flex",
                       justifyContent = "center",
                       height = "18px",
                       borderRadius = "6px",
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
                        display = "flex",
                        justifyContent = "center",
                        height = "18px",
                        borderRadius = "6px",
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
                       display = "flex",
                       justifyContent = "center",
                       height = "18px",
                       borderRadius = "6px",
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
                       display = "flex",
                       flexDirection = "column",
                       justifyContent = "center",
                       alignItems = "center",
                       borderRadius = "6px",
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
                     display = "flex",
                     flexDirection = "column",
                     justifyContent = "center",
                     alignItems = "center",
                     borderRadius = "6px",
                     boxShadow = box_shadow,
                     fontWeight = bold_text,
                     fontSize = text_size,
                     transition = animation))

      }
    }
  }
}
