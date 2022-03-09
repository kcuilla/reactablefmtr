#' Add a title above a reactable table
#'
#' Use `add_title()` to place a title above a {reactable} or {reactablefmtr} table.
#'      The title can be aligned to the left, right, or center with the align option.
#'      The text properties of the title, such as the font size and font style can be customized.
#'      The background color of the title can also be adjusted as well as the margin around the title.
#'
#' @param table A reactable table.
#'
#' @param title A string to be displayed as the title.
#'
#' @param align The alignment of the table.
#'     Options are "left", "right", "center".
#'     Default is "left".
#'
#' @param font_color Color of the title text.
#'     Default is #000.
#'
#' @param font_size Numeric value representing the size of the font of the title (in px).
#'     Default is 32.
#'
#' @param font_style Style of the title font.
#'     Options are "normal" or "italic".
#'     Default is "normal".
#'
#' @param font_weight The font weight of the title.
#'     Options are "bold" or "normal".
#'     Default is "bold".
#'
#' @param text_decoration Add an underline, overline, or line-through title.
#'     Default is NULL.
#'
#' @param text_transform Specify how to capitalize the title.
#'     Options are "uppercase", "lowercase", or "capitalize".
#'     Default is NULL.
#'
#' @param letter_spacing Numeric value that adjusts the horizontal spacing between letters.
#'     A number above 0 adds more spacing between letters, a number below 0 decreases the spacing.
#'     Default is NULL.
#'
#' @param word_spacing Numeric value that adjusts the horizontal spacing between words.
#'     A number above 0 adds more spacing between words, a number below 0 decreases the spacing.
#'     Default is NULL.
#'
#' @param text_shadow Apply a shadow around the title.
#'     See <https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow> for options.
#'     Default is NULL.
#'
#' @param background_color Color of the title background.
#'     Default is #FFFFFF.
#'
#' @param margin Use margin() to set the margin around the text (top, right, bottom, left).
#'     Default is NULL.
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
                      font_size = 32,
                      font_style = "normal",
                      font_weight = "bold",
                      text_decoration = NULL,
                      text_transform = NULL,
                      letter_spacing = NULL,
                      word_spacing = NULL,
                      text_shadow = NULL,
                      background_color = "#FFFFFF",
                      margin = NULL) {

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

  if (!is.null(text_transform) && text_transform %notin% c("uppercase", "lowercase", "capitalize") == TRUE) {

    stop("text_transform must be either 'uppercase', 'lowercase', or 'capitalize'")
  }

  if (!is.null(margin) && length(margin)<4) {

    stop("please provide margin dimensions within `margin()`. Ex. margin = margin(t=10)")
  }

  if (is.null(margin)) {

    margin <- margin(t=0,r=0,b=0,l=0)

  } else {margin <- margin}

  htmlwidgets::prependContent(
    table,
    htmltools::tags$h1(title,
                       style = paste0("color:", font_color, ";",
                                      "background:", background_color, ";",
                                      "text-align:", align, ";",
                                      "font-size:", font_size, "px;",
                                      "font-style:", font_style, ";",
                                      "font-weight:", font_weight, ";",
                                      "text-decoration:", text_decoration, ";",
                                      "letter-spacing:", letter_spacing, "px;",
                                      "word-spacing:", word_spacing, "px;",
                                      "text-transform:", text_transform, ";",
                                      "text-shadow:", text_shadow, ";",
                                      "margin-top:", margin[[1]], "px;",
                                      "margin-right:", margin[[2]], "px;",
                                      "margin-bottom:", margin[[3]], "px;",
                                      "margin-left:", margin[[4]], "px")
    )
  )
}


#' Add a subtitle above a reactable table
#'
#' Use `add_subtitle()` to place a subtitle above a {reactable} or {reactablefmtr} table.
#'      The same options that are present in `add_title()` and `add_source()` are also available in `add_subtitle()`.
#'      The subtitle can be aligned to the left, right, or center with the align option.
#'      The text properties of the subtitle, such as the font size and font style can be customized.
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
#'      Default is #000.
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
#' @param text_decoration Add an underline, overline, or line-through subtitle.
#'      Options are "underline", "overline", "underline overline", or "line-through".
#'      Default is NULL.
#'
#' @param text_transform Specify how to capitalize the title.
#'     Options are "uppercase", "lowercase", or "capitalize".
#'     Default is NULL.
#'
#' @param letter_spacing Numeric value that adjusts the horizontal spacing between letters.
#'     A number above 0 adds more spacing between letters, a number below 0 decreases the spacing.
#'     Default is NULL.
#'
#' @param word_spacing Numeric value that adjusts the horizontal spacing between words.
#'     A number above 0 adds more spacing between words, a number below 0 decreases the spacing.
#'     Default is NULL.
#'
#' @param text_shadow Apply a shadow around the title.
#'     See <https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow> for options.
#'     Default is NULL.
#'
#' @param background_color Color of the subtitle background.
#'      Default is #FFFFFF.
#'
#' @param margin Use margin() to set the margin around the text (top, right, bottom, left).
#'      Default is NULL.
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
                         font_color = "#000",
                         font_size = 24,
                         font_style = "normal",
                         font_weight = "bold",
                         text_decoration = NULL,
                         text_transform = NULL,
                         letter_spacing = NULL,
                         word_spacing = NULL,
                         text_shadow = NULL,
                         background_color = "#FFFFFF",
                         margin = NULL) {

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

  if (!is.null(text_transform) && text_transform %notin% c("uppercase", "lowercase", "capitalize") == TRUE) {

    stop("text_transform must be either 'uppercase', 'lowercase', or 'capitalize'")
  }

  if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

    stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
  }

  if (!is.null(margin) && length(margin)<4) {

    stop("please provide margin dimensions within `margin()`. Ex. margin = margin(t=10)")
  }

  if (is.null(margin)) {

    margin <- margin(t=0,r=0,b=0,l=0)

  } else {margin <- margin}


  htmlwidgets::prependContent(
    table,
    htmltools::tags$h2(subtitle,
                       style = paste0("color:", font_color, ";",
                                      "background:", background_color, ";",
                                      "text-align:", align, ";",
                                      "font-size:", font_size, "px;",
                                      "font-style:", font_style, ";",
                                      "font-weight:", font_weight, ";",
                                      "text-decoration:", text_decoration, ";",
                                      "letter-spacing:", letter_spacing, "px;",
                                      "word-spacing:", word_spacing, "px;",
                                      "text-transform:", text_transform, ";",
                                      "text-shadow:", text_shadow, ";",
                                      "margin-top:", margin[[1]], "px;",
                                      "margin-right:", margin[[2]], "px;",
                                      "margin-bottom:", margin[[3]], "px;",
                                      "margin-left:", margin[[4]], "px")
    )
  )
}


#' Add a source below a reactable table
#'
#' Use `add_source()` to place a source below a {reactable} or {reactablefmtr} table.
#'      The same options that are present in `add_title()` and `add_subtitle()` are also available in `add_source()`.
#'      The source can be aligned to the left, right, or center with the align option.
#'      The text properties of the source, such as the font size and font style can be customized.
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
#' @param text_decoration Add an underline, overline, or line-through source.
#'      Options are "underline", "overline", "underline overline", or "line-through".
#'      Default is NULL.
#'
#' @param text_transform Specify how to capitalize the title.
#'     Options are "uppercase", "lowercase", or "capitalize".
#'     Default is NULL.
#'
#' @param letter_spacing Numeric value that adjusts the horizontal spacing between letters.
#'     A number above 0 adds more spacing between letters, a number below 0 decreases the spacing.
#'     Default is NULL.
#'
#' @param word_spacing Numeric value that adjusts the horizontal spacing between words.
#'     A number above 0 adds more spacing between words, a number below 0 decreases the spacing.
#'     Default is NULL.
#'
#' @param text_shadow Apply a shadow around the title.
#'     See <https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow> for options.
#'     Default is NULL.
#'
#' @param background_color Color of the source background.
#'      Default is #FFFFFF.
#'
#' @param margin Use margin() to set the margin around the text (top, right, bottom, left).
#'      Default is NULL.
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
                        font_size = 16,
                        font_style = "normal",
                        font_weight = "normal",
                        text_decoration = NULL,
                        text_transform = NULL,
                        letter_spacing = NULL,
                        word_spacing = NULL,
                        text_shadow = NULL,
                        background_color = "#FFFFFF",
                        margin = NULL) {

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

  if (!is.null(text_transform) && text_transform %notin% c("uppercase", "lowercase", "capitalize") == TRUE) {

    stop("text_transform must be either 'uppercase', 'lowercase', or 'capitalize'")
  }

  if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

    stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
  }

  if (!is.null(margin) && length(margin)<4) {

    stop("please provide margin dimensions within `margin()`. Ex. margin = margin(t=10)")
  }

  if (is.null(margin)) {

    margin <- margin(t=0,r=0,b=0,l=0)

  } else {margin <- margin}

  htmlwidgets::appendContent(
    table,
    htmltools::tags$p(source,
                      style = paste0("color:", font_color, ";",
                                     "background:", background_color, ";",
                                     "text-align:", align, ";",
                                     "font-size:", font_size, "px;",
                                     "font-style:", font_style, ";",
                                     "font-weight:", font_weight, ";",
                                     "text-decoration:", text_decoration, ";",
                                     "letter-spacing:", letter_spacing, "px;",
                                     "word-spacing:", word_spacing, "px;",
                                     "text-transform:", text_transform, ";",
                                     "text-shadow:", text_shadow, ";",
                                     "margin-top:", margin[[1]], "px;",
                                     "margin-right:", margin[[2]], "px;",
                                     "margin-bottom:", margin[[3]], "px;",
                                     "margin-left:", margin[[4]], "px")
    )
  )
}


#' Add a legend to a reactable table
#'
#' Use `add_legend()` to place a legend below a {reactable} table.
#'      The legend can be used to display the color scale of a color palette used within the table.
#'      Supply the name of the dataset used with `data` and the name of the column you would like to show a legend for with `col_name`.
#'      By default, the colors within `colors` are the default color palette used in `color_tiles()` and `color_scales()`,
#'      but can be modified to match the color palette used in the column of the reactable table.
#'      The number of bins for the legend can be changed to any number. By default, label bins are given.
#'      The labels under the bins can be formatted with `number_fmt` or hidden by setting `labels` to FALSE.
#'      Use `title` to place a title above the legend, and `footer` to place a footer below the legend.
#'      The legend can be aligned to either the bottom-left or bottom-right of the table.
#'
#' @param table A reactable table.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param col_name The name of a column containing numeric data within the dataset.
#'
#' @param bins The number of bins for the legend.
#'     Default is 5.
#'
#' @param colors The color palette to be displayed in the legend.
#'     By default, the colors are shown to match the default colors used in
#'     `color_tiles()` and v`color_scales()`.
#'
#' @param bias A positive value that determines the spacing between multiple colors.
#'     A higher value spaces out the colors at the higher end more than a lower number.
#'     Default is 1.
#'
#' @param labels Logical. Show or hide the labels next to the legend.
#'     Default is TRUE.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
#'
#' @param title The title above the legend.
#'     Default is NULL.
#'
#' @param footer The footer below the legend.
#'     Default is NULL.
#'
#' @param align The horizontal alignment of the legend.
#'     Options are 'left' or 'right'.
#'     Default is 'right'.
#'
#' @return a function that adds a legend below a reactable table.
#'
#' @import reactable
#' @importFrom stats quantile
#' @importFrom magrittr %>%
#'
#' @examples
#' library(magrittr)
#' ## Create the reactable table and then pipe in the legend
#' data <- iris[10:29, ]
#' table <- reactable(data,
#' columns = list(Sepal.Length = colDef(
#' cell = color_tiles(data))))
#'
#' table %>%
#'   add_legend(data = data,
#'              col_name = "Sepal.Length")
#'
#' ## The legend can be aligned to either the left or right side
#' table %>%
#'   add_legend(data = data,
#'              col_name = "Sepal.Length",
#'              align = "left")
#'
#' ## Change the number of bins within the legend
#' table %>%
#'   add_legend(data = data,
#'              col_name = "Sepal.Length",
#'              bins = 9)
#'
#' ## Add a title and footer to the legend
#' table %>%
#'   add_legend(data = data,
#'              col_name = "Sepal.Length",
#'              title = "Sepal Length",
#'              footer = "measured in cm")
#'
#' ## If custom colors are used in the table, you can assign those to the legend as well
#' table <- reactable(data,
#' columns = list(Sepal.Length = colDef(
#' style = color_scales(data, colors = c("red","white","blue")))))
#'
#' table %>%
#'   add_legend(data = data,
#'              col_name = "Sepal.Length",
#'              colors = c("red","white","blue"))
#' @export

add_legend <- function(table,
                       data = NULL,
                       col_name = NULL,
                       bins = 5,
                       colors = NULL,
                       bias = 1,
                       labels = TRUE,
                       number_fmt = NULL,
                       title = NULL,
                       footer = NULL,
                       align = "right") {

  '%notin%' <- Negate('%in%')

  if (align %notin% c("left", "right") == TRUE) {

    stop("`align` must be either 'left' or 'right'")
  }

  if (!is.logical(labels)) {

    stop("`labels` must either be TRUE or FALSE.")
  }

  if (!is.numeric(bins)) {

    stop("`bins` must numeric.")
  }

  if (is.null(colors)) {
    cols <- c("#15607A", "#FFFFFF", "#FA8C00")
  } else {
    cols <- colors
  }

  colassign <- function(x) {
    rgb(colorRamp(c(cols), bias = bias)(x), maxColorValue = 255)
  }

  ### conditional fill color and font color
  if (is.character(col_name)) {

    if (all(col_name %in% names(which(sapply(data, is.numeric))))) {

          calc_bins <- function(x) {
            as.numeric(format(stats::quantile(data[[col_name]], na.rm = TRUE, seq(0, 1, by=1/(x-1))), digits = 2))
          }

          buckets <- calc_bins(bins)

          if (!is.null(number_fmt)) {

            legend_labels <- number_fmt(buckets)

          } else legend_labels <- buckets

          normalization <- function(x) {
              x <- as.numeric(x)
              (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
            }

          # standard normalization (no variance check)
          if (mean((data[[col_name]] - mean(data[[col_name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

            normalized <- 1

          } else {

            # standard normalization
            normalized <- normalization(buckets)

          }

          # assign colors to normalized bins
          palette <- normalized %>% purrr::map(colassign)

    } else {

      stop("Attempted to select non-existing column or non-numeric column with `col_name`")
    }

  }

  legend_bins_l <- function(x, y) {
    htmltools::tags$ul(
          style = "list-style: none; margin: 0; padding: 0;",
          htmltools::tags$li(style = "display: block; float: left; width: 30px; margin-bottom: 6px; text-align: center; font-size: 80%; list-style: none;",
                             htmltools::tags$span(
                               style = paste0(
                                 "display: block; float: left; height: 14px; width: 30px; background:",
                                 x,
                                 ";"
                               )
                             ), if (labels == TRUE) { y } else { })
        )
  }

  if (align == "left") {

  legend <- htmltools::tagList(
      htmltools::tags$div(title,
                          style = "text-align: left; margin-bottom: 8px; font-weight: bold; font-size: 90%"),
      htmltools::tags$div(
        style = "margin: 0;, padding: 0;, float: left; list-style: none;",

        purrr::map2(palette, legend_labels, legend_bins_l)
      ),
      htmltools::tags$div(footer,
                          style = "text-align: left; clear: both; font-size: 70%; color: #999")
    )

  } else {

  legend_bins_r <- function(x, y) {
    htmltools::tags$ul(
          style = "list-style: none; margin: 0; padding: 0;",
          htmltools::tags$li(style = "display: block; float: right; width: 30px; margin-bottom: 6px; text-align: center; font-size: 80%; list-style: none;",
                             htmltools::tags$span(
                               style = paste0(
                                 "display: block; float: left; height: 14px; width: 30px; background:",
                                 x,
                                 ";"
                               )
                             ), if (labels == TRUE) { y } else { })
        )
  }

  legend <- htmltools::tagList(
      htmltools::tags$div(title,
                          style = "text-align: right; margin-bottom: 8px; font-weight: bold; font-size: 90%"),
      htmltools::tags$div(
        style = "margin: 0;, padding: 0;, float: left; list-style: none;",

        purrr::map2(rev(palette), rev(legend_labels), legend_bins_r)
      ),
      htmltools::tags$div(footer,
                          style = "text-align: right; clear: both; font-size: 70%; color: #999")

    )
  }

  htmlwidgets::appendContent(
    table,
    htmltools::tags$div(legend)
    )
}


#' Add an icon legend to a reactable table
#'
#' Use `add_icon_legend()` to place a legend below a reactable table.
#'      The legend can be used to display the icon set used within `icon_sets()`.
#'      The legend can be aligned to the right or left of the table.
#'      Custom labels can be applied to the upper and lower bounds of the legend.
#'
#' @param table A reactable table.
#'
#' @param icon_set The icon set to be displayed in the legend.
#'     Options are "ski rating", "medals", and "batteries".
#'     Default is NULL.
#'
#' @param show_labels Logical. Show or hide the labels next to the legend.
#'      Default is TRUE.
#'
#' @param labels Assign label to each icon in the specified icon set.
#'      Number of labels must match the number of icons in the icon set.
#'      Default is NULL.
#'
#' @param align The horizontal alignment of the legend.
#'      Options are 'left' or 'right'.
#'      Default is 'right'.
#'
#' @param title The title above the legend.
#'     Default is NULL.
#'
#' @param footer The footer below the legend.
#'     Default is NULL.
#'
#' @param margin Use margin() to set the margin around the legend (top, right, bottom, left).
#'      Default is NULL.
#'
#' @return a function that adds a source below a reactable table.
#'
#' @import reactable
#'
#' @examples
#' \dontrun{
#' ## Create the reactable table and then pipe in the legend
#' library(dplyr)
#' data <- iris[10:29, ]
#' table <- reactable(data,
#' columns = list(Petal.Length = colDef(
#' cell = icon_sets(data, icon_set = "medals"))))
#'
#' table %>%
#'   add_icon_legend(icon_set = "medals")
#'
#' ## The legend can be aligned to the left or right of the table
#' table %>%
#'   add_icon_legend(icon_set = "medals", align = "left")
#'
#' ## Add custom labels to each icon in the legend
#' table %>%
#'   add_icon_legend(icon_set = "medals", labels = c("Shortest Length","Avg Length","Longest Length"))
#'
#' ## Add a title and footer to the legend
#' table %>%
#'   add_icon_legend(icon_set = "medals", title = "Icon Legend Title", footer = "Icon Legend Footer")
#' }
#' @export

add_icon_legend <- function(table = NULL,
                            icon_set = NULL,
                            show_labels = TRUE,
                            labels = NULL,
                            align = "right",
                            title = NULL,
                            footer = NULL,
                            margin = NULL) {

  '%notin%' <- Negate('%in%')

  if (!is.null(icon_set) && icon_set %notin% c("ski rating", "medals", "batteries") == TRUE) {

    stop("icon_set must be either 'ski rating', 'medals', or 'batteries'")
  }

  if (align %notin% c("left", "right") == TRUE) {

    stop("`align` must be either 'left' or 'right'")
  }

  if (!is.logical(show_labels)) {

    stop("`show_labels` must either be TRUE or FALSE.")
  }

  if (is.null(margin)) {

    margin <- margin(t=0,r=0,b=0,l=0)

  } else {margin <- margin}

  if (!is.null(icon_set) && icon_set == "ski rating") {

    if (!is.null(labels) && length(labels) != 4) {
      stop("must provide four labels for ski rating. Ex: `labels` = c('easy','moderate','difficult','most difficult')")
    } else {

        legend <- htmltools::tags$span(
          htmltools::tagAppendAttributes(
            shiny::icon("circle"),
            style = paste0("font-size:16px; color:#39b54a;")
          ),
          if (show_labels == TRUE & is.null(labels)) {
            "Easy"
          } else if (show_labels == TRUE & !is.null(labels)) {
            labels[[1]]
          } else {
            ""
          },
          htmltools::tagAppendAttributes(
            shiny::icon("square-full"),
            style = paste0("font-size:4px; color:", "transparent;")
          ),
          htmltools::tagAppendAttributes(
            shiny::icon("square-full"),
            style = paste0("font-size:16px; color:#0f75bc")
          ),
          if (show_labels == TRUE & is.null(labels)) {
            "Moderate"
          } else if (show_labels == TRUE & !is.null(labels)) {
            labels[[2]]
          } else {
            ""
          },
          htmltools::tagAppendAttributes(
            shiny::icon("square-full"),
            style = paste0("font-size:4px; color:", "transparent;")
          ),
          htmltools::tagAppendAttributes(
            shiny::icon("square"),
            style = paste0("transform: rotate(45deg); font-size:16px; color:#000000")
          ),
          if (show_labels == TRUE & is.null(labels)) {
            "Difficult"
          } else if (show_labels == TRUE & !is.null(labels)) {
            labels[[3]]
          } else {
            ""
          },
          htmltools::tagAppendAttributes(
            shiny::icon("square-full"),
            style = paste0("font-size:4px; color:", "transparent;")
          ),
          list(
            htmltools::tagAppendAttributes(
              shiny::icon("square"),
              style = paste0("transform: rotate(45deg); font-size:16px; color:#000000")
            ),
            htmltools::tagAppendAttributes(
              shiny::icon("square"),
              style = paste0("transform: rotate(45deg); font-size:16px; color:#000000")
            )
          ),
          if (show_labels == TRUE & is.null(labels)) {
            "Most Difficult"
          } else if (show_labels == TRUE & !is.null(labels)) {
            labels[[4]]
          } else {
            ""
          }
        )
    }

} else if (!is.null(icon_set) && icon_set == "medals") {

  if (!is.null(labels) && length(labels) != 3) {
    stop("must provide three labels for medals. Ex: `labels` = c('bronze','silver','gold')")
  } else {

    legend <- htmltools::tags$span(
      htmltools::tagAppendAttributes(
        shiny::icon("medal"),
        style = paste0("font-size:16px; color:#A77044")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "Bronze"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[1]]
        } else {
          ""
        },
      htmltools::tagAppendAttributes(
        shiny::icon("medal"),
        style = paste0("font-size:16px; color:#D7D7D7")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "Silver"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[2]]
        } else {
          ""
        },
      htmltools::tagAppendAttributes(
        shiny::icon("medal"),
        style = paste0("font-size:16px; color:#D6AF36")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "Gold"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[3]]
        } else {
          ""
        }
      )
  }

} else if (!is.null(icon_set) && icon_set == "batteries") {

  if (!is.null(labels) && length(labels) != 4) {
    stop("must provide three labels for batteries. Ex: `labels` = c('one-quarter','half','three-quarters','full')")
  } else {

    legend <- htmltools::tags$span(
      htmltools::tagAppendAttributes(
        shiny::icon("battery-quarter"),
        style = paste0("font-size:16px; color:#d7191c")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "0-25%"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[1]]
        } else {
          ""
        },
      htmltools::tagAppendAttributes(
        shiny::icon("square-full"),
        style = paste0("font-size:1px; color:", "transparent;")
      ),
      htmltools::tagAppendAttributes(
        shiny::icon("battery-half"),
        style = paste0("font-size:16px; color:#fdae61")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "25-50%"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[2]]
        } else {
          ""
        },
      htmltools::tagAppendAttributes(
        shiny::icon("square-full"),
        style = paste0("font-size:1px; color:", "transparent;")
      ),
      htmltools::tagAppendAttributes(
        shiny::icon("battery-three-quarters"),
        style = paste0("font-size:16px; color:#a6d96a")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "50-75%"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[3]]
        } else {
          ""
        },
      htmltools::tagAppendAttributes(
        shiny::icon("square-full"),
        style = paste0("font-size:1px; color:", "transparent;")
      ),
      htmltools::tagAppendAttributes(
        shiny::icon("battery-full"),
        style = paste0("font-size:16px; color:#1a9641")
      ),
        if (show_labels == TRUE & is.null(labels)) {
          "75-100%"
        } else if (show_labels == TRUE & !is.null(labels)) {
          labels[[4]]
        } else {
          ""
        }
      )
  }
}

  if (align == "right") {

    htmlwidgets::appendContent(
    table,
    htmltools::tagList(
      htmltools::tags$div(title,
                          style = "text-align: right; margin-bottom: 8px; font-weight: bold; font-size: 90%"),
      htmltools::tags$p(legend,
                        style = paste0("text-align:",align,";
                                        font-size:13px;
                                        word-spacing:1px;")
    ),
      htmltools::tags$div(footer,
                          style = "text-align: right; clear: both; font-size: 70%; color: #999")
    ))

  } else {

    htmlwidgets::appendContent(
    table,
    htmltools::tagList(
      htmltools::tags$div(title,
                          style = "text-align: left; margin-bottom: 8px; font-weight: bold; font-size: 90%"),
      htmltools::tags$p(legend,
                        style = paste0("text-align:",align,";
                                        font-size:13px;
                                        word-spacing:1px;")
    ),
      htmltools::tags$div(footer,
                          style = "text-align: left; clear: both; font-size: 70%; color: #999")
    ))
  }
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
#' @param horizontal_align The horizontal alignment of the text within a cell.
#'      Options are "left", "right", or "center".
#'      Default is "right".
#'
#' @param vertical_align The vertical alignment of the text within a cell.
#'      Options are "top", "bottom", or "center".
#'      Default is "top".
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
                       horizontal_align = "right",
                       vertical_align = "top",
                       text_decoration = NULL,
                       border_width = NULL,
                       border_style = NULL,
                       border_color = NULL,
                       background_color = NULL,
                       animation = "1s ease") {

  '%notin%' <- Negate('%in%')

  if (!is.null(horizontal_align) && horizontal_align %notin% c("left", "right", "center") == TRUE) {

    stop("horizontal_align must be either 'left', 'right', or 'center'")
  }

  if (!is.null(vertical_align) && vertical_align %notin% c("top", "bottom", "center") == TRUE) {

    stop("vertical_align must be either 'top', 'bottom', or 'center'")
  }

  # assign vertical align
  if (vertical_align == "top") {

    vertical_align_css <- "start"

  } else if (vertical_align == "bottom") {

    vertical_align_css <- "end"

  } else vertical_align_css <- "center"

  # assign horizontal align
  if (horizontal_align == "left") {

    horizontal_align_css <- "flex-start"

  } else if (horizontal_align == "right") {

    horizontal_align_css <- "flex-end"

  } else horizontal_align_css <- "center"

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
           display = "flex",
           alignItems = vertical_align_css,
           justifyContent = horizontal_align_css,
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
           display = "flex",
           alignItems = vertical_align_css,
           justifyContent = horizontal_align_css,
           fontSize = font_size)
      }
  }
}


#' Apply HTML attributes to title, subtitle, and source text.
#'
#' Use `html()` to apply HTML attributes to text within `add_title()`, `add_subtitle()`, and `add_source()`.
#'
#' @param text,... The text provided within the title, subtitle or source with HTML attributes applied.
#'
#' @return an object of class HTML.
#'
#' @examples
#' \dontrun{
#' ## Change the title color to blue
#' data <- iris[10:29, ]
#' reactable(data) %>%
#' add_title(html("Normal title. <span style='color:DodgerBlue;'>Blue title.</span>"))
#'
#' ## Add emojis to the source
#' data <- iris[10:100, ]
#' reactable(data) %>%
#' add_source(html("<p>Made with &#128151; by: John Doe &#128512;</p>"))
#' }
#' @export

html <- function(text, ...) {

  htmltools::HTML(text, ...)
}


#' Margin dimensions.
#'
#' @param t,r,b,l The dimensions of the top, right, bottom, and left margins.
#'
#' @return a function that provides margin dimensions.
#'
#' @export

margin <- function(t = 0, r = 0, b = 0, l = 0) {
  m <- c(t, r, b, l)
  m
}


#' Apply a font from Google Fonts <https://fonts.google.com/> to the table.
#'
#' @param table Null.
#'
#' @param font_family Color of the font for the text within the table.
#'      Default is #222222.
#'
#' @param font_weight The numeric weight of the font.
#'      Must be a value between 100 and 900.
#'      Note: not every font on Google Fonts has all font weights available.
#'      Please check <https://fonts.google.com/> for available weights for desired font family.
#'      Default is 400.
#'
#' @param font_style Style of the text font.
#'      Options are "normal" or "italic".
#'      Default is "normal".
#'
#' @return a function that applies a font to a reactable table.
#'
#' @import reactable
#' @import sass
#'
#' @examples
#' \dontrun{
#' data <- iris[10:29, ]
#'
#' ## Default 'Poppins' font from Google Fonts
#' reactable(data) %>%
#' google_font()
#'
#' ## Apply styles to fonts
#' reactable(data) %>%
#' google_font("Roboto Mono", font_weight = 500, font_style = "italic")
#' }
#' @export

google_font <- function(table = NULL,
                        font_family = "Poppins",
                        font_weight = 400,
                        font_style = "normal") {

  '%notin%' <- Negate('%in%')

  if (font_style %notin% c("normal", "italic") == TRUE) {

    stop("font_style must be either 'normal' or 'italic'")

  } else if (font_style == "normal") {

    style = "0"

  } else { style = "1" }

  if (!is.null(font_weight) & !is.numeric(font_weight)) {

    stop("font_weight must either be a numeric value between 100 and 900")
  }

  pull_font <- list("my-font" = font_google(font_family, wght = font_weight, ital = style))

  css <- sass(
    list(
      pull_font,
      list("body {font-family: $my-font}")
      )
  )

  htmlwidgets::appendContent(
    table,
    htmltools::tags$style(css)
    )

}


#' Apply a tooltip to cells.
#'
#' @param data Null.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
#'
#' @return a function that applies a tooltip to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' \dontrun{
#' data <- iris[10:29, ]
#'
#' ## Apply a tooltip to color_scales()
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(
#'  cell = tooltip(),
#'  style = color_scales(data))
#'  ))
#' }
#' @export

tooltip <- function(data,
                    number_fmt = NULL) {

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else {

      label <- number_fmt(value)

    }

    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)

    tippy::tippy(label,
                 animateFill = FALSE,
                 followCursor = TRUE,
                 tooltip = tooltip_label)
  }
}


#' Hide rows containing duplicate values on sort. Must be placed within reactable::colDef(style).
#' Credit to Greg Lin, creator of {reactable} for writing the JS function.
#'
#' @param col_name Name of the column.
#'
#' @return a function that hides duplicate values on sort in a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[1:20, c("Manufacturer", "Model", "Type", "MPG.city")]
#'
#' ## Merge unique groups in a column:
#' reactable(data,
#' pagination = FALSE,
#' columns = list(Manufacturer = colDef(
#' style = group_merge_sort("Manufacturer")
#' ))
#' )
#'
#' ## Works with columns containing numeric data as well:
#' reactable(data,
#' pagination = FALSE,
#' columns = list(MPG.city = colDef(
#' style = group_merge_sort("MPG.city")
#' ))
#' )
#' @export

group_merge_sort = function(col_name = NULL) {

  # JS function written by Greg Lin, creator of reactable
  htmlwidgets::JS(htmltools::HTML(paste0("
  function(rowInfo, colInfo, state) {
    const firstSorted = state.sorted[0]
    if (!firstSorted || firstSorted.id === '",col_name,"') {
      const prevRow = state.pageRows[rowInfo.viewIndex - 1]
      if (prevRow && rowInfo.row['",col_name,"'] === prevRow['",col_name,"']) {
        return {visibility: 'hidden'}
      }
    }
  }"
  )))
}


#' Add a styled border beneath rows of specified groups on sort. Must be placed within reactable::rowStyle().
#' Credit to Greg Lin, creator of {reactable} for writing the JS function.
#'
#' @param columns Name of the column(s).
#'      Can provide up to four column names.
#'
#' @param border_width The width of the border.
#'      Options are "thin", "medium", "thick", or a numeric value such as "2px".
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-width) for more options.
#'      Default is 'thin'.
#'
#' @param border_style The style of the border.
#'      Options are "solid", "dashed", "dotted", "double", "groove", "ridge", "inset", "outset", "none", or "hidden".
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style) for more options.
#'      Default is 'solid'.
#'
#' @param border_color The color of the border.
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-color) for more options.
#'      Default is #777.
#'
#' @return a function that applies a bottom border to each group in a column of a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[1:20, c("Manufacturer", "Model", "Type", "MPG.city")]
#'
#' ## Add border beneath each unique group within a column on sort:
#' reactable(data,
#' pagination = FALSE,
#' rowStyle = group_border_sort("Manufacturer")
#' )
#'
#' ## Can specify up to 4 columns:
#' reactable(data,
#' pagination = FALSE,
#' rowStyle = group_border_sort(columns = c("Manufacturer","Model","Type"))
#' )
#'
#' ## Apply styles to the border:
#' reactable(data,
#' pagination = FALSE,
#' rowStyle = group_border_sort(columns = c("Manufacturer","Model","Type"),
#'                              border_color = "red",
#'                              border_style = "dashed",
#'                              border_width = "3px")
#' )
#' @export

group_border_sort = function(columns = NULL,
                             border_width = "thin",
                             border_color = "#777",
                             border_style = "solid") {

  if (length(columns) > 4) {
    stop("must provide only 4 column names or less.")
  }

  if (length(columns) == 1) {
    columns <- c(columns, columns, columns, columns)
  } else if (length(columns) == 2) {
    columns <- c(columns[[1]], columns[[2]], columns[[1]], columns[[2]])
  } else if (length(columns) == 3) {
    columns <- c(columns[[1]], columns[[2]], columns[[3]], columns[[1]])
  } else { columns <- columns }

  # JS function written by Greg Lin, creator of reactable
  htmlwidgets::JS(htmltools::HTML(paste0("
  function(rowInfo, state) {
    const firstSorted = state.sorted[0]
    if (firstSorted && firstSorted.id === '",columns[[1]],"') {
      const nextRow = state.pageRows[rowInfo.viewIndex + 1]
      if (nextRow && rowInfo.row.",columns[[1]]," !== nextRow.",columns[[1]],") {
        return {borderBottom: '",border_width," ",border_style," ",border_color,"'}
      }
    } else if (firstSorted && firstSorted.id === '",columns[[2]],"') {
      const nextRow = state.pageRows[rowInfo.viewIndex + 1]
      if (nextRow && rowInfo.row.",columns[[2]]," !== nextRow.",columns[[2]],") {
        return {borderBottom: '",border_width," ",border_style," ",border_color,"'}
      }
    } else if (firstSorted && firstSorted.id === '",columns[[3]],"') {
      const nextRow = state.pageRows[rowInfo.viewIndex + 1]
      if (nextRow && rowInfo.row.",columns[[3]]," !== nextRow.",columns[[3]],") {
        return {borderBottom: '",border_width," ",border_style," ",border_color,"'}
      }
    } else if (firstSorted && firstSorted.id === '",columns[[4]],"') {
      const nextRow = state.pageRows[rowInfo.viewIndex + 1]
      if (nextRow && rowInfo.row.",columns[[4]]," !== nextRow.",columns[[4]],") {
        return {borderBottom: '",border_width," ",border_style," ",border_color,"'}
      }
    }
  }"
  )))
}
