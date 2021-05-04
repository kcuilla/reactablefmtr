#' Add color tiles to rows in a column
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
#'     Default colors provided are red-white-blue: c("#ff3030", "#ffffff", "#1e90ff").
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
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @param text_color Assigns text color to values.
#'     Default is black.
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
#'     Default is set to FALSE.
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
#' ## By default, the colors_tiles() function uses a red-white-blue three-color pattern
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
                        colors = c("#ff3030", "#ffffff", "#1e90ff"),
                        color_ref = NULL,
                        opacity = 1,
                        number_fmt = NULL,
                        text_color = "black",
                        brighten_text = TRUE,
                        brighten_text_color = "white",
                        bold_text = FALSE,
                        span = FALSE) {

  if (!is.logical(bold_text)) {

    stop("`bold_text` must be TRUE or FALSE")
  }

  if (!is.logical(brighten_text)) {

    stop("`brighten_text` must be TRUE or FALSE")
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
      rgb(colorRamp(c(colors))(x), maxColorValue = 255)
    else
      NULL
  }

  assign_color <- function(x) {

    if (!is.na(x)) {
      rgb_sum <- rowSums(colorRamp(c(colors))(x))
      color <- ifelse(rgb_sum >= 375, "black", "white")
      color
    } else
      NULL
  }

  if (bold_text == TRUE) {

    bold_text <- "bold"

  } else bold_text <- "normal"

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    if (is.logical(span)) {

      if (span) {

        normalized <- (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))

      } else {

        normalized <- (value - min(data[[name]], na.rm = TRUE))/(max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

      }

      ### conditional fill color and font color
      if (is.character(color_ref)) {

        if (all(color_ref %in% names(which(sapply(data, is.character))))) {

          if (is.character(color_ref)) { color_ref <- which(names(data) %in% color_ref) }

          cell_color <- data[[color_ref]][index]
          cell_color <- grDevices::adjustcolor(cell_color, alpha.f = opacity)

          rgb_sum <- rowSums(grDevices::colorRamp(c(cell_color))(1))

          font_color <- ifelse(rgb_sum >= 375, text_color, brighten_text_color)

        } else {

          stop("Attempted to select non-existing column or non-character column with fill_color_ref")
        }

      } else {

        cell_color <- color_pal(normalized)
        font_color <- assign_color(normalized)

      }

    } else if (is.numeric(span) | is.character(span)) {

      if (all(span %in% which(sapply(data, is.numeric))) | all(span %in% names(which(sapply(data, is.numeric))))) {

        if (is.character(span)) { span <- which(names(data) %in% span) }

        normalized <- (value - min(dplyr::select(data, !!span), na.rm = TRUE)) / (max(dplyr::select(data, !!span), na.rm = TRUE) - min(dplyr::select(data, !!span), na.rm = TRUE))
        cell_color <- if (name %in% colnames(data)[span]) { color_pal(normalized) }
        font_color <- if (name %in% colnames(data)[span]) { assign_color(normalized) }

      } else {

        stop("Attempted to select non-existing or non-numeric columns with span")

      }

    }

    if (brighten_text == FALSE) {

      htmltools::div(label,
                     style = list(background = cell_color,
                                  display = "flex",
                                  justifyContent = "center",
                                  borderRadius = "4px",
                                  fontWeight = bold_text,
                                  height = "18px"))

    } else

      htmltools::div(label,
                     style = list(background = cell_color,
                                  color = font_color,
                                  display = "flex",
                                  justifyContent = "center",
                                  borderRadius = "4px",
                                  fontWeight = bold_text,
                                  height = "18px"))

  }
}

