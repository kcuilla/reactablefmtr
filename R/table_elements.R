#' Add a title above a reactable table
#'
#' Use `add_title()` to place a title above a {reactable} or {reactablefmtr} table.
#'      The title can be aligned to the left, right, or center with the align option.
#'      The text properties of the title, such as the font size, font family, and font style can be customized.
#'      The background color of the title can also be adjusted as well as the margin around the title.
#'
#' @param table A reactable table.
#'
#' @param title A string to be displayed as the title.
#'
#' @param align The alignment of the table.
#'      Options are "left", "right", "center".
#'      Default is "left".
#'
#' @param font_color Color of the title text.
#'      Default is #000.
#'
#' @param font_family Font family of the title.
#'      Default is -apple-system, BlinkMacSystemFont, Helvetica, Arial, sans-serif.
#'
#' @param font_size Numeric value representing the size of the font of the title (in px).
#'      Default is 32.
#'
#' @param font_style Style of the title font.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param font_weight The font weight of the title.
#'      Options are "bold" or "normal".
#'      Default is "bold".
#'
#' @param text_decoration Optionally add an underline, overline, or line-through title.
#'      Options are "underline", "overline", "underline overline", or "line-through".
#'      Default is NULL.
#'
#' @param background_color Color of the title background.
#'      Default is #FFFFFF.
#'
#' @param margin Numeric value representing the four-sided margin around the title (in px).
#'      Default is 2.
#'
#' @return a function that adds a title above a reactable table.
#'
#' @import reactable
#'
#' @examples
#' \dontrun{
#' ## Create the reactable table and then pipe in the title
#' table <- reactable(iris[10:29, ])
#'
#' table %>%
#'   add_title("This is a title")
#'
#' ## Use options to adjust the style and position of the title
#' table %>%
#'   add_title("This is a title", align = "center", font_color = "red")
#' }
#' @export

add_title <- function(table = NULL,
                      title = NULL,
                      align = "left",
                      font_color = "#000",
                      font_family = "-apple-system,BlinkMacSystemFont,Helvetica,Arial,sans-serif",
                      font_size = 32,
                      font_style = "normal",
                      font_weight = "bold",
                      text_decoration = NULL,
                      background_color = "#FFFFFF",
                      margin = 2) {

  '%notin%' <- Negate('%in%')

  if (align %notin% c("left", "right", "center") == TRUE) {

    stop("align must be either 'left', 'right', or 'center'")
  }

  if (font_style %notin% c("normal", "italic") == TRUE) {

    stop("font_style must be either 'normal' or 'italic'")
  }

  if (font_weight %notin% c("normal", "bold") == TRUE) {

    stop("font_weight must be either 'normal' or 'bold'")
  }

  if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

    stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
  }

  htmlwidgets::prependContent(
    table,
    htmltools::tags$h1(title,
                       style = paste0("color:", font_color, ";",
                                      "background:", background_color, ";",
                                      "text-align:", align, ";",
                                      "font-family:", font_family, ";",
                                      "font-size:", font_size, "px;",
                                      "font-style:", font_style, ";",
                                      "font-weight:", font_weight, ";",
                                      "text-decoration:", text_decoration, ";",
                                      "margin:", margin, "px")
    )
  )
}


#' Add a subtitle above a reactable table
#'
#' Use `add_subtitle()` to place a subtitle above a {reactable} or {reactablefmtr} table.
#'      The same options that are present in `add_title()` and `add_source()` are also available in `add_subtitle()`.
#'      The subtitle can be aligned to the left, right, or center with the align option.
#'      The text properties of the subtitle, such as the font size, font family, and font style can be customized.
#'      The background color of the subtitle can also be adjusted as well as the margin around the subtitle.
#'
#' @param table A reactable table.
#'
#' @param subtitle A string to be displayed as the subtitle.
#'
#' @param align The alignment of the subtitle.
#'      Options are "left", "right", "center".
#'      Default is "left".
#'
#' @param font_color Color of the subtitle text.
#'      Default is #333.
#'
#' @param font_family Font family of the subtitle.
#'      Default is -apple-system, BlinkMacSystemFont, Helvetica, Arial, sans-serif.
#'
#' @param font_size Numeric value representing the size of the font of the subtitle (in px).
#'      Default is 24.
#'
#' @param font_style Style of the subtitle font.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param font_weight The font weight of the subtitle.
#'      Options are "bold" or "normal".
#'      Default is "bold".
#'
#' @param text_decoration Optionally add an underline, overline, or line-through subtitle.
#'      Options are "underline", "overline", "underline overline", or "line-through".
#'      Default is NULL.
#'
#' @param background_color Color of the subtitle background.
#'      Default is #FFFFFF.
#'
#' @param margin Numeric value representing the four-sided margin around the subtitle (in px).
#'      Default is 2.
#'
#' @return a function that adds a subtitle above a reactable table.
#'
#' @import reactable
#'
#' @examples
#' \dontrun{
#' ## Create the reactable table and then pipe in the subtitle
#' table <- reactable(iris[10:29, ])
#'
#' table %>%
#'   add_subtitle("This is a subtitle")
#'
#' ## If a title proceeds a subtitle, the subtite will be placed below the title
#' table %>%
#'   add_title("This is a title") %>%
#'   add_subtitle("This is a subtitle")
#'
#' ## Use options to adjust the style and position of the subtitle
#' table %>%
#'   add_subtitle("This is a subtitle", align = "center", font_color = "red")
#' }
#' @export

add_subtitle <- function(table = NULL,
                         subtitle = NULL,
                         align = "left",
                         font_color = "#333",
                         font_family = "-apple-system,BlinkMacSystemFont,Helvetica,Arial,sans-serif",
                         font_size = 24,
                         font_style = "normal",
                         font_weight = "bold",
                         text_decoration = NULL,
                         background_color = "#FFFFFF",
                         margin = 2) {

  '%notin%' <- Negate('%in%')

  if (align %notin% c("left", "right", "center") == TRUE) {

    stop("align must be either 'left', 'right', or 'center'")
  }

  if (font_style %notin% c("normal", "italic") == TRUE) {

    stop("font_style must be either 'normal' or 'italic'")
  }

  if (font_weight %notin% c("normal", "bold") == TRUE) {

    stop("font_weight must be either 'normal' or 'bold'")
  }

  if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

    stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
  }

  htmlwidgets::prependContent(
    table,
    htmltools::tags$h2(subtitle,
                       style = paste0("color:", font_color, ";",
                                      "background:", background_color, ";",
                                      "text-align:", align, ";",
                                      "font-family:", font_family, ";",
                                      "font-size:", font_size, "px;",
                                      "font-style:", font_style, ";",
                                      "font-weight:", font_weight, ";",
                                      "text-decoration:", text_decoration, ";",
                                      "margin:", margin, "px")
    )
  )
}


#' Add a source below a reactable table
#'
#' Use `add_source()` to place a source below a {reactable} or {reactablefmtr} table.
#'      The same options that are present in `add_title()` and `add_subtitle()` are also available in `add_source()`.
#'      The source can be aligned to the left, right, or center with the align option.
#'      The text properties of the source, such as the font size, font family, and font style can be customized.
#'      The background color of the source can also be adjusted as well as the margin around the source.
#'
#' @param table A reactable table.
#'
#' @param source A string to be displayed as the source.
#'
#' @param align The alignment of the source.
#'      Options are "left", "right", "center".
#'      Default is "left".
#'
#' @param font_color Color of the source text.
#'      Default is #000.
#'
#' @param font_family Font family of the source.
#'      Default is -apple-system, BlinkMacSystemFont, Helvetica, Arial, sans-serif.
#'
#' @param font_size Numeric value representing the size of the font of the source (in px).
#'      Default is 16.
#'
#' @param font_style Style of the source font.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param font_weight The font weight of the source.
#'      Options are "bold" or "normal".
#'      Default is "normal".
#'
#' @param text_decoration Optionally add an underline, overline, or line-through source.
#'      Options are "underline", "overline", "underline overline", or "line-through".
#'      Default is NULL.
#'
#' @param background_color Color of the source background.
#'      Default is #FFFFFF.
#'
#' @param margin Numeric value representing the four-sided margin around the source (in px).
#'      Default is 4.
#'
#' @return a function that adds a source below a reactable table.
#'
#' @import reactable
#'
#' @examples
#' \dontrun{
#' ## Create the reactable table and then pipe in the source
#' table <- reactable(iris[10:29, ])
#'
#' table %>%
#'   add_source("This is a source")
#'
#' ## Use options to adjust the style and position of the source
#' table %>%
#'   add_source("This is a source", font_style = "italic", font_color = "grey")
#' }
#' @export

add_source <- function(table = NULL,
                        source = NULL,
                        align = "left",
                        font_color = "#000",
                        font_family = "-apple-system,BlinkMacSystemFont,Helvetica,Arial,sans-serif",
                        font_size = 16,
                        font_style = "normal",
                        font_weight = "normal",
                        text_decoration = NULL,
                        background_color = "#FFFFFF",
                        margin = 4) {

  '%notin%' <- Negate('%in%')

  if (align %notin% c("left", "right", "center") == TRUE) {

    stop("align must be either 'left', 'right', or 'center'")
  }

  if (font_style %notin% c("normal", "italic") == TRUE) {

    stop("font_style must be either 'normal' or 'italic'")
  }

  if (font_weight %notin% c("normal", "bold") == TRUE) {

    stop("font_weight must be either 'normal' or 'bold'")
  }

  if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

    stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
  }

  htmlwidgets::appendContent(
    table,
    htmltools::tags$p(source,
                      style = paste0("color:", font_color, ";",
                                     "background:", background_color, ";",
                                     "text-align:", align, ";",
                                     "font-family:", font_family, ";",
                                     "font-size:", font_size, "px;",
                                     "font-style:", font_style, ";",
                                     "font-weight:", font_weight, ";",
                                     "text-decoration:", text_decoration, ";",
                                     "margin:", margin, "px")
    )
  )
}


#' Add custom styles to cells
#'
#' Use `cell_style()` to customize the appearance of certain cells in a {reactable} or {reactablefmtr} table.
#'      Assign custom styles by either row number(s) or by values within a particular column.
#'      The font color, font size, font style, and font weight can all be modified.
#'      Borders can also be placed around cells and customized by style, width, and color.
#'      By default, animation is applied to the cells that are styled, but can be turned off by setting to 'none'.
#'      Some options within `cell_style()` will work with other {reactablefmtr} formatters (such as data_bars() and color_tiles()),
#'      but it is not fully supported and should be used separately, not together.
#'      `cell_style()` needs to be placed within the style argument of reactable::colDef.
#'
#' @param data A dataset to be displayed within a {reactable} table.
#'
#' @param rows Numeric value representing the row number to apply the custom style.
#'      Can provide a vector of rows if applying to more than one row.
#'      If no rows are provided, styles are applied to all rows/values.
#'
#' @param values A value, either numeric or character, that is present within a column.
#'      Can provide a vector of values if applying to more than one value.
#'      If no values are provided, styles are applied to all rows/values.
#'
#' @param font_color Color of the text.
#'
#' @param font_size Numeric value representing the size of the font of the text (in px).
#'      Default is 16.
#'
#' @param font_style Style of the text font.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @param font_weight The font weight of the text
#'      Options are "normal", "bold", "bolder", "lighter" or a value between 100 and 900.
#'      Default is "normal".
#'
#' @param text_decoration Optionally add an underline, overline, or line-through to the text
#'      Options are "underline", "overline", "underline overline", or "line-through".
#'      Default is NULL.
#'
#' @param border_width The width of the border around the cell.
#'      Options are "thin", "medium", "thick", or a numeric value such as "2px".
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-width) for more options.
#'
#' @param border_style The style of the border around the cell.
#'      Options are "solid", "dashed", "dotted", "double", "groove", "ridge", "inset", "outset", "none", or "hidden".
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style) for more options.
#'
#' @param border_color The color of the border around the cell.
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-color) for more options.
#'
#' @param background_color Color of the background of the cell.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "1s ease".
#'
#' @return a function that adds a custom style to a row or rows in a reactable table.
#'
#' @import reactable
#'
#' @examples
#' \dontrun{
#' ## Add a dotted blue border around the third row in Sepal.Length
#' data <- iris[10:29, ]
#' reactable(data,
#'          columns = list(
#'            Sepal.Length = colDef(
#'              style = cell_style(data,
#'                                 rows = 3,
#'                                 border_width = "thick",
#'                                 border_color = "blue",
#'                                 border_style = "dotted"))))
#'
#' ## For all setosa species, highlight cell yellow and assign red font color
#' data <- iris[10:100, ]
#' reactable(data,
#'          columns = list(
#'          Species = colDef(
#'              style = cell_style(data,
#'                                 values = "setosa",
#'                                 font_color = "red",
#'                                 background_color = "yellow"))))
#' }
#' @export

cell_style <- function(data,
                       rows = NULL,
                       values = NULL,
                       font_color = NULL,
                       font_size = NULL,
                       font_style = "normal",
                       font_weight = "normal",
                       text_decoration = NULL,
                       border_width = NULL,
                       border_style = NULL,
                       border_color = NULL,
                       background_color = NULL,
                       animation = "1s ease") {

  '%notin%' <- Negate('%in%')

  style <- function(value, index, name) {

    if (!is.null(values) && values %in% data[[name]] == FALSE) {

      stop("values do not exist in dataset")

      if (!is.null(border_style) & grepl("solid | dashed | dotted | double | groove | ridge | inset | outset | none | hidden", border_style) == FALSE) {

        stop("border_style type must be either solid, dashed, dotted, double, groove, ridge, inset, outside, none, or hidden.")
      }

      if (!is.null(font_weight) & !is.numeric(font_weight) & grepl("normal | bold | bolder | lighter", font_weight) == FALSE) {

        stop("font_weight must either be a numeric value between 100 and 900 or one of normal, bold, bolder, or lighter.")
      }

      if (font_style %notin% c("normal", "italic") == TRUE) {

        stop("font_style must be either 'normal' or 'italic'")
      }

      if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

        stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
      }

    } else if (value %in% values | index %in% rows) {

      list(transition = animation,
           borderColor = border_color,
           borderWidth = border_width,
           borderStyle = border_style,
           color = font_color,
           background = background_color,
           textDecoration = text_decoration,
           fontStyle = font_style,
           fontWeight = font_weight,
           fontSize = font_size)

    } else if (is.null(values) & is.null(rows)) {

      list(transition = animation,
           borderColor = border_color,
           borderWidth = border_width,
           borderStyle = border_style,
           color = font_color,
           background = background_color,
           textDecoration = text_decoration,
           fontStyle = font_style,
           fontWeight = font_weight,
           fontSize = font_size)
      }
  }
}
