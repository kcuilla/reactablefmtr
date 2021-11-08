#' Merge two columns together and stack the values on top of each other
#'
#' The `column_stack()` function can be used to two values from two different columns in one row.
#'     When `column_stack()` is called within a column, the values from that column will be displayed on top, while the values from the other column given within `col_name` will be shown underneath.
#'     The size, color, weight, and style of the text can be controlled for both the top and bottom values/text shown.
#'     To adjust the values shown on the top row, use `top_size`, `top_color`, `top_weight`, `top_style`, or `top_decoration`.
#'     To adjust the values shown on the bottom row, replace 'top' with 'bottom' in the options listed above.
#'     `column_stack()` works with both numeric and non-numeric columns.
#'     `column_stack()` needs to placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing either a text or numeric column.
#'
#' @param col_name The name of the column in which it's values will be stacked underneath.
#'     The column can contain either numeric or non-numeric data.
#'     Only a single column can be provided.
#'     Default is NULL.
#'
#' @param top_size The size of the text displayed on top.
#'     Default is 14.
#'
#' @param top_color The color of the text displayed on top.
#'     Default is NULL.
#'
#' @param top_weight The font weight of the text displayed on top.
#'      Options are "bold" or "normal".
#'      Default is "bold".
#'
#' @param top_style The style of the text displayed on top.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param top_decoration The decoration of the text displayed on top.
#'      For example, add an underline, overline, or line-through to the text.
#'      Default is NULL.
#'
#' @param bottom_size The size of the text displayed on bottom.
#'     Default is 12.
#'
#' @param bottom_color The color of the text displayed on bottom.
#'     Default is NULL.
#'
#' @param bottom_weight The font weight of the text displayed on bottom.
#'      Options are "bold" or "normal".
#'      Default is "normal".
#'
#' @param bottom_style The style of the text displayed on bottom.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param bottom_decoration The decoration of the text displayed on bottom.
#'      For example, add an underline, overline, or line-through to the text.
#'      Default is NULL.
#'
#' @return a function that surrounds text/values in a column
#'     with a colored pill button.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Manufacturer", "Model", "MPG.city", "MPG.highway")]
#'
#' ## Stack text from one column underneath another column:
#' reactable(
#' data,
#' columns = list(
#' Manufacturer = colDef(cell = column_stack(data, col_name = "Model")),
#' Model = colDef(show = FALSE)))
#'
#' ## Control the appearance of both the top and bottom text:
#' reactable(
#' data,
#' columns = list(
#' Manufacturer = colDef(name = "Manufacturer/Model",
#'                       cell = column_stack(data,
#'                                           col_name = "Model",
#'                                           top_size = 18,
#'                                           top_color = "red",
#'                                           bottom_size = 16,
#'                                           bottom_color = "blue",
#'                                           bottom_style = "italic")),
#' Model = colDef(show = FALSE)))
#'
#' ## Can be used with columns containing numeric data:
#' reactable(
#' data,
#' columns = list(
#' MPG.city = colDef(name = "MPG City/Highway",
#'                   cell = column_stack(data,
#'                                       col_name = "MPG.highway",
#'                                       top_decoration = "underline",
#'                                       bottom_color = "green",
#'                                       bottom_size = 14,
#'                                       bottom_weight = "bold")),
#' MPG.highway = colDef(show = FALSE)))
#'
#' ## Combine both numeric and non-numeric columns together:
#' reactable(
#' data,
#' columns = list(
#' Model = colDef(name = "Model/MPG Highway",
#'                   cell = column_stack(data,
#'                                       col_name = "MPG.highway",
#'                                       top_weight = "normal",
#'                                       bottom_size = 20,
#'                                       bottom_color = "green",
#'                                       bottom_decoration = "underline")),
#' MPG.highway = colDef(show = FALSE),
#' MPG.city = colDef(show = FALSE)))
#'
#' @export


column_stack <- function(data,
                         col_name = NULL,
                         top_size = 14,
                         top_color = NULL,
                         top_weight = "bold",
                         top_style = "normal",
                         top_decoration = NULL,
                         bottom_size = 12,
                         bottom_color = NULL,
                         bottom_weight = "normal",
                         bottom_style = "normal",
                         bottom_decoration = NULL) {

  '%notin%' <- Negate('%in%')

  if (top_style %notin% c("normal", "italic") == TRUE) {

    stop("top_style must be either 'normal' or 'italic'")
  }

  if (bottom_style %notin% c("normal", "italic") == TRUE) {

    stop("bottom_style must be either 'normal' or 'italic'")
  }

  if (top_weight %notin% c("normal", "bold") == TRUE) {

    stop("top_weight must be either 'normal' or 'bold'")
  }

  if (bottom_weight %notin% c("normal", "bold") == TRUE) {

    stop("bottom_weight must be either 'normal' or 'bold'")
  }

  cell <- function(value, index, name) {

      if (is.character(col_name)) {

        if (all(col_name %in% names(data))) {

          if (is.character(col_name)) { col_name <- which(names(data) %in% col_name) }

          col2_value <- data[[col_name]][index]

        } else {

          stop("Attempted to select non-existing column with col_name")
        }

        htmltools::div(
          htmltools::div(style = list(fontSize = top_size,
                                      color = top_color,
                                      fontWeight = top_weight,
                                      textDecoration = top_decoration,
                                      fontStyle = top_style), value),
          htmltools::div(style = list(fontSize = bottom_size,
                                      color = bottom_color,
                                      fontWeight = bottom_weight,
                                      textDecoration = bottom_decoration,
                                      fontStyle = bottom_style), col2_value)
        )
      }
  }
}
