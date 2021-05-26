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
