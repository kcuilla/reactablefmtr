#' Merge two columns together in a specified arrangement.
#'
#' The `merge_column()` function can be used to merge and style values from two columns within a reactable table.
#'     `merge_column()` works with both numeric and non-numeric columns.
#'     The column in which you would like to be merged with an existing column has style properties that can be controlled with a preceding 'merged_'.
#'     The existing column style properties can be controlled with a preceding 'existing_'.
#'     The position of the column to be merged relative to the existing column can be controlled with `merged_position`.
#'     When merging another column, you can place the values from that column above, below, left, or right to the existing column.
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
#' @param existing_size The size of the text displayed on right.
#'     Default is 12.
#'
#' @param existing_color The color of the text displayed on right.
#'     Default is NULL.
#'
#' @param existing_weight The font weight of the text displayed on right.
#'      Options are "bold" or "normal".
#'      Default is "normal".
#'
#' @param existing_style The style of the text displayed on right.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param existing_decoration The decoration of the text displayed on right.
#'      For example, add an underline, overline, or line-through to the text.
#'      Default is NULL.
#'
#' @param spacing The spacing between the merged and existing columns.
#'      Default is 3.
#'      A value greater than 3 creates more space between, a value less than 3 creates less space.
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
#' Manufacturer = colDef(cell = merge_column(data, merged_name = "Model")),
#' Model = colDef(show = FALSE)))
#'
#' ## Control the appearance of both the existing and merged columns:
#' reactable(
#' data,
#' columns = list(
#' Manufacturer = colDef(name = "Manufacturer/Model",
#'                       cell = merge_column(data,
#'                                           merged_name = "Model",
#'                                           merged_size = 16,
#'                                           merged_color = "blue",
#'                                           merged_style = "italic",
#'                                           existing_size = 18,
#'                                           existing_color = "red"
#' )),
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
                         existing_size = 14,
                         existing_color = NULL,
                         existing_weight = "bold",
                         existing_style = "normal",
                         existing_decoration = NULL,
                         spacing = 3) {

  '%notin%' <- Negate('%in%')

  if (existing_style %notin% c("normal", "italic") == TRUE) {

    stop("existing_style must be either 'normal' or 'italic'")
  }

  if (merged_style %notin% c("normal", "italic") == TRUE) {

    stop("merged_style must be either 'normal' or 'italic'")
  }

  if (existing_weight %notin% c("normal", "bold") == TRUE) {

    stop("existing_weight must be either 'normal' or 'bold'")
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
        htmltools::div(style = list(fontSize = existing_size,
                                    color = existing_color,
                                    fontWeight = existing_weight,
                                    textDecoration = existing_decoration,
                                    fontStyle = existing_style,
                                    marginBottom = paste0(spacing,"px")), value),
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
                                      marginBottom = paste0(spacing,"px")), col2_value),
          htmltools::div(style = list(fontSize = existing_size,
                                      color = existing_color,
                                      fontWeight = existing_weight,
                                      textDecoration = existing_decoration,
                                      fontStyle = existing_style), value)
        )
      } else if (merged_position == "left") {
        htmltools::tagList(
          htmltools::span(style = list(fontSize = merged_size,
                                      color = merged_color,
                                      fontWeight = merged_weight,
                                      textDecoration = merged_decoration,
                                      fontStyle = merged_style,
                                      marginRight = paste0(spacing,"px")), col2_value),
          htmltools::span(style =
                            list(fontSize = existing_size,
                                 color = existing_color,
                                 fontWeight = existing_weight,
                                 textDecoration = existing_decoration,
                                 fontStyle = existing_style), value)
        )
      } else {
        htmltools::tagList(
          htmltools::span(style =
                            list(fontSize = existing_size,
                                 color = existing_color,
                                 fontWeight = existing_weight,
                                 textDecoration = existing_decoration,
                                 fontStyle = existing_style,
                                 marginRight = paste0(spacing,"px")), value),
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
