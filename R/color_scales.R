#' Add color scales to cells in a column
#'
#' The `color_scales()` function conditionally colors each cell of a column depending on their value in relation to other values in that particular column.
#'     The colors can be provided within a vector in `colors` or via another column in the dataset by referencing the column by name with `color_ref`.
#'     The opacity of the colors provided can be adjusted by providing a value between 0 and 1 in `opacity`.
#'     `text_color` can be used to change the color of the values.
#'     If values are displayed within a dark-colored background, `brighten_text` will display the values in white text so they are more visible.
#'     The color of `brighten_text_color` can be changed to a color other than white if desired.
#'     If the user wants to assign colors row-wise instead of column-wise, set `span` equal to TRUE to apply across all columns.
#'     Or can provide the names of the columns by either column name or column position number to apply to only a subset of the columns.
#'     `color_scales()` should be placed within the style argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A vector of colors to color the cells.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are blue-white-orange: c("#15607A", "#FFFFFF", "#FA8C00").
#'     Can use R's built-in colors or other color packages.
#'
#' @param color_ref Assign colors from another column that contains the colors for each row.
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
#' @param even_breaks Logical: if TRUE, the colors will be assigned to values in distinct quantile bins rather than on a normalized scale.
#'      The number of breaks in the quantile bins is equal to the number of colors provided within `colors`.
#'      For example, if 4 colors are provided within `colors`, the values in the bottom 25% of the column will be assigned the lowest color,
#'      the values within 25-50% will be assigned the next color, etc. until all 4 colors are used.
#'      Default is FALSE.
#'
#' @param text_size Numeric value representing the size of the text labels.
#'     Default is NULL.
#'
#' @param text_color Assigns text color to values.
#'     Default is black.
#'
#' @param text_color_ref Assign text color from another column
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
#' @param border_width The width of the four-sided border around the cell.
#'      Options are "thin", "medium", "thick", or a numeric value.
#'      Default is NULL.
#'
#' @param border_style The style of the four-sided border around the cell.
#'      Options are "solid", "dashed", "dotted", "double", "groove", "ridge", "inset", "outset", or "none".
#'      Default is NULL.
#'
#' @param border_color The color of the four-sided border around the cell.
#'      Default is NULL.
#'
#' @param span Optionally apply colors to values across multiple columns instead of by each column.
#'     To apply across all columns set to TRUE.
#'     If applying to a set of columns, can provide either column names or column positions.
#'     Default is set to FALSE.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "background 1s ease".
#'
#' @return a function that applies conditional colors
#'     to a column of numeric values.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## By default, the colors_scales() function uses a blue-white-orange three-color pattern
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(style = color_scales(data))))
#'
#' ## If only two colors are desired,
#' ## you can specify them with colors = 'c(color1, color2)';
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(style = color_scales(data,
#'  colors = c("red", "green")))))
#'
#' ## Apply color_scales() across all numeric columns using reactable::defaultColDef
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data)))
#'
#' ## Use span to apply colors to values in relation to the entire dataset
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data, span = TRUE)))
#'
#' ## Span can take column names
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data, span = c("Sepal.Length", "Sepal.Width"))))
#'
#' ## Or it can also take column positions instead
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data, span = 1:2)))
#'
#' @export

color_scales <- function(data,
                         colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                         color_ref = NULL,
                         color_by = NULL,
                         opacity = 1,
                         bias = 1,
                         min_value = NULL,
                         max_value = NULL,
                         even_breaks = FALSE,
                         text_size = NULL,
                         text_color = "black",
                         text_color_ref = NULL,
                         show_text = TRUE,
                         brighten_text = TRUE,
                         brighten_text_color = "white",
                         bold_text = FALSE,
                         border_width = NULL,
                         border_style = NULL,
                         border_color = NULL,
                         span = FALSE,
                         animation = "background 1s ease") {

  if (!is.logical(bold_text)) {

    stop("`bold_text` must be TRUE or FALSE")
  }

  if (!is.logical(brighten_text)) {

    stop("`brighten_text` must be TRUE or FALSE")
  }

  if (!is.numeric(opacity)) {

    stop("`opacity` must be numeric")
  }

  if (!is.numeric(bias)) {

    stop("`bias` must be numeric")
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

  if (!is.null(border_width) & !is.numeric(border_width) && !border_width %in% c("thin", "medium", "thick") == TRUE) {

    stop("`border_width` must be either 'thin', 'medium', or 'thick' or a numeric value.")
  }

  if (!is.null(border_style) && !border_style %in% c("solid", "dotted", "dashed", "double", "groove", "ridge", "inset", "outset", "none") == TRUE) {

    stop("`border_style` must be either 'solid', 'dotted', 'dashed', 'double', 'groove', 'ridge', 'inset', 'outset', or 'none'.")
  }

  if (is.null(border_width) & is.null(border_style) & is.null(border_color)) {
    border_width = ""
    border_style = ""
    border_color = ""
  }

  if (is.null(border_width)) {
    border_width = "thin"
  }

  if (is.null(border_style)) {
    border_style = "solid"
  }

  if (is.null(border_color)) {
    border_color = "lightgrey"
  }

  if (!is.null(border_width)) {

    if (border_width == "thin") {

      border_width = "1px 1px 1px 1px"

    } else if (border_width == "medium") {

      border_width = "2px 2px 2px 2px"

    } else if (border_width == "thick") {

      border_width = "3px 3px 3px 3px"

    } else border_width = paste0("",border_width,"px")
  }

  style <- function(value, index, name) {

    if (is.null(color_ref) & is.null(color_by) & !is.numeric(value)) return(value)

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

    color_buckets <- dplyr::ntile(data[[name]], n = length(colors))
    color_assign <- color_buckets[index]
    colors <- grDevices::adjustcolor(colors, alpha.f = opacity)

    if (even_breaks == FALSE) {
      cell_color = cell_color
    } else cell_color = colors[[color_assign]]

    if (brighten_text == FALSE & show_text == TRUE) {

      list(background = cell_color, color = text_color, fontSize = text_size, fontWeight = bold_text, transition = animation, borderStyle = border_style, borderWidth = border_width, borderColor = border_color)

    } else if (brighten_text == TRUE & !is.null(text_color_ref) & show_text == TRUE) {

      list(background = cell_color, color = text_color, fontSize = text_size, fontWeight = bold_text, transition = animation, borderStyle = border_style, borderWidth = border_width, borderColor = border_color)

    } else if (show_text == FALSE) {

      list(background = cell_color, color = "transparent", fontWeight = bold_text, transition = animation, borderStyle = border_style, borderWidth = border_width, borderColor = border_color)

    } else list(background = cell_color, color = font_color, fontSize = text_size, fontWeight = bold_text, transition = animation, borderStyle = border_style, borderWidth = border_width, borderColor = border_color)

  }
}
