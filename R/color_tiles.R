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
#'     Default is "1s ease".
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
#' ## Use number_fmt to format numbers using the scales package
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#' defaultColDef = colDef(cell = color_tiles(car_prices,
#' number_fmt = scales::dollar)))
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
#' @export


color_tiles <- function(data,
                        colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                        color_ref = NULL,
                        opacity = 1,
                        bias = 1,
                        number_fmt = NULL,
                        text_color = "black",
                        text_color_ref = NULL,
                        show_text = TRUE,
                        brighten_text = TRUE,
                        brighten_text_color = "white",
                        bold_text = FALSE,
                        span = FALSE,
                        box_shadow = FALSE,
                        tooltip = FALSE,
                        animation = "1s ease") {

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

    if (is.null(color_ref) & !is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else {

      label <- number_fmt(value)

    }

    if (is.logical(span)) {

      if (span) {

        normalized <- (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))

      } else if (!is.null(color_ref)) {

        normalized <- dplyr::ntile(data[[name]], n = length(colors))

      } else {

          ### normalization for color palette
          if (is.numeric(value) & mean((data[[name]] - mean(data[[name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

            normalized <- 1

          } else {

            normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

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
          cell_color <- grDevices::adjustcolor(cell_color, alpha.f = opacity)

          rgb_sum <- rowSums(grDevices::colorRamp(c(cell_color), bias = bias)(1))

          font_color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)

        } else {

          stop("Attempted to select non-existing column or non-character column with fill_color_ref")
        }

      } else {

        cell_color <- color_pal(normalized)
        cell_color <- grDevices::adjustcolor(cell_color, alpha.f = opacity)
        font_color <- assign_color(normalized)

      }

    } else if (is.numeric(span) | is.character(span)) {

      if (all(span %in% which(sapply(data, is.numeric))) | all(span %in% names(which(sapply(data, is.numeric))))) {

        if (is.character(span)) { span <- which(names(data) %in% span) }

        normalized <- (value - min(dplyr::select(data, !!span), na.rm = TRUE)) / (max(dplyr::select(data, !!span), na.rm = TRUE) - min(dplyr::select(data, !!span), na.rm = TRUE))
        cell_color <- if (name %in% colnames(data)[span]) { grDevices::adjustcolor(color_pal(normalized), alpha.f = opacity) }
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
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     arrow = "small",
                     tooltip = label)
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
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     arrow = "small",
                     tooltip = label)
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
                        fontSize = 0,
                        boxShadow = box_shadow,
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     arrow = "small",
                     tooltip = label)
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
                        transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     arrow = "small",
                     tooltip = label)
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
                       transition = animation)),
        tippy::tippy(label,
                     animateFill = FALSE,
                     arrow = "small",
                     tooltip = label)
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
                     transition = animation))

      }
    }
  }
}
