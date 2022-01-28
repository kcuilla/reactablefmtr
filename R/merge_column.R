#' Merge two columns together in a specified arrangement.
#'
#' The `merge_column()` function can be used to merge and style values from two columns within a reactable table.
#'     `merge_column()` works with both numeric and non-numeric columns.
#'     The style/format of both the current column and merged column can be controlled via size, color, weight, and text decoration.
#'     Any style parameters that start with "merged_" will control the column that is being merged and specified by `merged_name`.
#'     Any style parameters that do not start with "merged_" control the current column you are within.
#'     The position of the column to be merged relative to the current column can be controlled with `merged_position`.
#'     The position options for the merged column are above, below, left, or right to the current column.
#'     The spacing between the current column and the merged column can be controlled with `spacing`.
#'     `merge_column()` needs to placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing either a text or numeric column.
#'
#' @param merged_name The name of the column to merge with the existing column.
#'     The column can contain either numeric or non-numeric data.
#'     Only a single column can be provided.
#'     Default is NULL.
#'
#' @param merged_position The position of the merged column in relation to the existing column.
#'     Options are 'right', 'left', 'above', or 'below'.
#'     Default is right.
#'
#' @param merged_size The size of the text of the merged column.
#'     Default is 12.
#'
#' @param merged_color The color of the text of the merged column.
#'     Default is #777.
#'
#' @param merged_weight The font weight of the text of the merged column.
#'      Options are "bold" or "normal".
#'      Default is "normal".
#'
#' @param merged_style The style of the text of the merged column.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param merged_decoration The decoration of the text of the merged column.
#'      For example, add an underline, overline, or line-through to the text.
#'      Default is NULL.
#'
#' @param size The size of the text displayed in the current column.
#'     Default is 12.
#'
#' @param color The color of the text displayed in the current column.
#'     Default is NULL.
#'
#' @param weight The font weight of the text displayed in the current column.
#'      Options are "bold" or "normal".
#'      Default is "normal".
#'
#' @param style The style of the text displayed in the current column.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param decoration The decoration of the text displayed in the current column.
#'      For example, add an underline, overline, or line-through to the text.
#'      Default is NULL.
#'
#' @param spacing The spacing between the merged and existing columns.
#'      A value greater than 0 creates more space between, a value less than 0 creates less space.
#'      Default is 0.
#'
#' @return a function that merges two columns together.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Manufacturer", "Model", "MPG.city", "MPG.highway")]
#'
#' ## Stack text from one column with another column:
#' reactable(
#' data,
#' columns = list(
#' Manufacturer = colDef(name = "Manufacturer/Model",
#'                       cell = merge_column(data, merged_name = "Model"
#'                       )),
#' Model = colDef(show = FALSE)))
#'
#' ## Control the appearance of both the current and merged columns:
#' reactable(
#' data,
#' columns = list(
#' Manufacturer = colDef(name = "Manufacturer/Model",
#'                       cell = merge_column(data,
#'                                           merged_name = "Model",
#'                                           merged_size = 16,
#'                                           merged_color = "blue",
#'                                           merged_style = "italic",
#'                                           size = 18,
#'                                           color = "red"
#'                                           )),
#' Model = colDef(show = FALSE)))
#'
#' ## Combine both numeric and non-numeric columns together:
#' reactable(
#' data,
#' columns = list(
#' Model = colDef(name = "Model/MPG Highway",
#'                   cell = merge_column(data,
#'                                       merged_name = "MPG.highway",
#'                                       merged_position = "below",
#'                                       merged_size = 20,
#'                                       merged_color = "green"
#' )),
#' MPG.highway = colDef(show = FALSE),
#' MPG.city = colDef(show = FALSE)))
#' @export

merge_column <- function(data,
                         merged_name = NULL,
                         merged_position = "right",
                         merged_size = 12,
                         merged_color = "#777",
                         merged_weight = "normal",
                         merged_style = "normal",
                         merged_decoration = "normal",
                         size = 14,
                         color = NULL,
                         weight = "bold",
                         style = "normal",
                         decoration = NULL,
                         spacing = 0) {

  '%notin%' <- Negate('%in%')

  if (merged_position %notin% c("right", "left", "above", "below") == TRUE) {

    stop("merged_position must be either 'right', 'left', 'above', or 'below'")
  }

  if (style %notin% c("normal", "italic") == TRUE) {

    stop("style must be either 'normal' or 'italic'")
  }

  if (merged_style %notin% c("normal", "italic") == TRUE) {

    stop("merged_style must be either 'normal' or 'italic'")
  }

  if (weight %notin% c("normal", "bold") == TRUE) {

    stop("weight must be either 'normal' or 'bold'")
  }

  if (merged_weight %notin% c("normal", "bold") == TRUE) {

    stop("merged_weight must be either 'normal' or 'bold'")
  }

  cell <- function(value, index, name) {

    if (is.character(merged_name)) {

      if (all(merged_name %in% names(data))) {

        if (is.character(merged_name)) { merged_name <- which(names(data) %in% merged_name) }

        col2_value <- data[[merged_name]][index]

      } else {

        stop("Attempted to select non-existing column with merged_name")
      }

      if (merged_position == "below") {
      htmltools::div(
        htmltools::div(style = list(fontSize = size,
                                    color = color,
                                    fontWeight = weight,
                                    textDecoration = decoration,
                                    fontStyle = style,
                                    marginBottom = paste0(spacing+3,"px")), value),
        htmltools::div(style = list(fontSize = merged_size,
                                    color = merged_color,
                                    fontWeight = merged_weight,
                                    textDecoration = merged_decoration,
                                    fontStyle = merged_style), col2_value)
      )
      } else if (merged_position == "above") {
        htmltools::div(
          htmltools::div(style = list(fontSize = merged_size,
                                      color = merged_color,
                                      fontWeight = merged_weight,
                                      textDecoration = merged_decoration,
                                      fontStyle = merged_style,
                                      marginBottom = paste0(spacing+3,"px")), col2_value),
          htmltools::div(style = list(fontSize = size,
                                      color = color,
                                      fontWeight = weight,
                                      textDecoration = decoration,
                                      fontStyle = style), value)
        )
      } else if (merged_position == "left") {
        htmltools::tagList(
          htmltools::span(style = list(fontSize = merged_size,
                                      color = merged_color,
                                      fontWeight = merged_weight,
                                      textDecoration = merged_decoration,
                                      fontStyle = merged_style,
                                      marginRight = paste0(spacing+3,"px")), col2_value),
          htmltools::span(style =
                            list(fontSize = size,
                                 color = color,
                                 fontWeight = weight,
                                 textDecoration = decoration,
                                 fontStyle = style), value)
        )
      } else {
        htmltools::tagList(
          htmltools::span(style =
                            list(fontSize = size,
                                 color = color,
                                 fontWeight = weight,
                                 textDecoration = decoration,
                                 fontStyle = style,
                                 marginRight = paste0(spacing+3,"px")), value),
          htmltools::span(style = list(fontSize = merged_size,
                                       color = merged_color,
                                       fontWeight = merged_weight,
                                       textDecoration = merged_decoration,
                                       fontStyle = merged_style), col2_value)
        )
      }
    }
  }
}
