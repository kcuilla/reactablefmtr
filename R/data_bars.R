#' Add horizontal bars to cells in a column
#'
#' The `data_bars()` function adds a horizontal bar to each row of a column.
#'     The length of the bars are relative to the value of the row in relation to other values within the same column.
#'     The maximum width of the filled bars can be adjusted. Ex. if you are displaying percentages,
#'     and the maximum value in your column is 50% but you would like the bars to show only half-filled,
#'     you could increase the maximum fill to 100% with `max_value`.
#'     The values for the bars can be displayed inside or outside the filled bars with the `text_position` option.
#'     By default, the values are displayed on the outside-end of the filled bars.
#'     The fill_color of both the fill and the background of the bars can be adjusted.
#'     To adjust the fill_color of the filled bar, use `fill_color`.
#'     If more than one color is provided, a conditional color palette will be applied to to the values,
#'     or if `fill_gradient` is set to TRUE, a left-to-right gradient fill color will be applied.
#'     The fill colors can also be provided via another column in the dataset by referencing the column by name with `fill_color_ref`.
#'     `text_color` can be used to change the color of the text_position. By default, the label color is black.
#'     If values are displayed inside the bars and a dark color palette is used to fill the bars,
#'     `brighten_text` will display the values in white text so the values are visible by default.
#'     The color of `brighten_text_color` can be changed to a color other than white if desired.
#'     The border around the filled bars can be controlled via three different border options:
#'     `border_style`, `border_width`, and `border_color`.
#'     An icon or image can be added to the data bars with `icon` or `img`.
#'     Alternatively, icons and images can be assigned from another column with `icon_ref` and `img_ref`, similar to `fill_color_ref`.
#'     The color of the icons can be assigned through either `icon_color` (a single color) or `icon_color_ref` (from another column).
#'     The size of the images can be adjusted using `img_height` and `img_width`.
#'     The size of the icons can be adjusted using `icon_size`.
#'     `data_bars()` works with columns containing both positive and negative values.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param fill_color A single color or a vector of fill_color for the fill of the data bars.
#'     fill_color should be given in order from low values to high values.
#'     Can use R's built-in fill_color or other color packages.
#'     Default is #15607A.
#'
#' @param fill_color_ref Optionally assign fill_color to from another column
#'     by providing the name of the column containing the fill colors in quotes.
#'     Only one color can be provided per row, and therefore will not work with fill_gradient.
#'     Default is NULL.
#'
#' @param fill_opacity A value between 0 and 1 that adjusts the opacity in fill_color.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param fill_gradient Logical: if two or more colors are provided in fill_color,
#'     the colors in the fill of the bars are converted to a left-to-right gradient.
#'     Default is FALSE.
#'
#' @param background The color for the background of the data bars.
#'     Default is #EEEEEE.
#'
#' @param max_value A value to use as the maximum value for the width of the filled bars.
#'     The default maximum value is the maximum value in the column.
#'     Default is NULL.
#'
#' @param min_value A value to use as the minimum value for the width of the filled bars.
#'     Default is NULL.
#'
#' @param align_bars Display filled bars from left-to-right or right-to-left.
#'     Options are "left" or "right".
#'     Default is left.
#'
#' @param bar_height Numeric height of the data bars in px.
#'     Default is 19.
#'
#' @param text_position Choose where to display the text labels relative to the filled data bars.
#'     Text labels can be displayed within the filled bars ("inside-end" or "inside-base"),
#'     outside of the filled bars ("outside-end" or "outside-base"),
#'     within the center of the filled bars ("center"),
#'     above the filled bars ("above"),
#'     or not displayed at all ("none").
#'     Default is inside-end.
#'
#' @param force_outside Optionally force a range of values to display their text labels on the outside-end of the filled bars
#'     when the text_position is set to either "inside-end", "inside-base", or "center".
#'     Must provide a start and a stop number for the range of values to be forced to outside-end. Ex. c(0, 100).
#'     Default is NULL.
#'
#' @param text_color The color of the text labels.
#'     Default is black.
#'
#' @param text_color_ref Optionally assign text color from another column
#'     by providing the name of the column containing the text colors in quotes.
#'     Only one color can be provided per cell.
#'     Default is NULL.
#'
#' @param text_size Numeric value representing the size of the text labels (in px).
#'     Default is 14.
#'
#' @param brighten_text Logical: automatically assign color to text labels based on filled color
#'     when the text labels are positioned within the filled bars.
#'     Text within dark-colored filled bars will turn white, text within light-colored bars will be black.
#'     Default is TRUE.
#'
#' @param brighten_text_color Assigns color to text labels if values are within a dark-colored filled bar.
#'     Default is white.
#'
#' @param bold_text Logical: display the text labels in bold.
#'     Default is FALSE.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
#'
#' @param border_width The width of the border around the filled data bars
#'      Options are "thin", "medium", "thick", or a numeric value with the units included such as "2px" or "2mm".
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-width](https://developer.mozilla.org/en-US/docs/Web/CSS/border-width) for more options.
#'      Default is NULL.
#'
#' @param border_style The style of the border around the filled data bars
#'      Options are "solid", "dashed", "dotted", "double", "groove", "ridge", "inset", "outset", "none", or "hidden".
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-style](https://developer.mozilla.org/en-US/docs/Web/CSS/border-style) for more options.
#'      Default is NULL.
#'
#' @param border_color The color of the border around the filled data bars
#'      May be specified using one, two, three, or four values.
#'      See [CSS border-color](https://developer.mozilla.org/en-US/docs/Web/CSS/border-color) for more options.
#'      Default is NULL.
#'
#' @param icon Assign an icon label from the Font Awesome library (via shiny).
#'     If an icon is provided, it will be positioned so that it does not overlap the text for the data bars.
#'     Default is NULL.
#'
#' @param icon_ref Optionally assign icons from another column
#'     by providing the name of the column containing the icons in quotes.
#'     Only one icon can be provided per cell.
#'     Default is NULL.
#'
#' @param icon_size A value representing the size of the icon in px.
#'     Default is 20.
#'
#' @param icon_color The color for the icon.
#'     If no color is provided, default is set to the color of the filled bars.
#'     Default is NULL.
#'
#' @param icon_color_ref Optionally assign color to the icons from another column
#'     by providing the name of the column containing the icon colors in quotes.
#'     Only one color can be provided per cell.
#'     Default is NULL.
#'
#' @param img Optionally assign an image label via a valid URL.
#'
#' @param img_ref Optionally assign images from another column
#'     by providing the name of the column containing the image URLs in quotes.
#'     Only one image can be provided per cell.
#'     Default is NULL.
#'
#' @param img_height A value for the height of the image in px.
#'     Default is 20.
#'
#' @param img_width A value for the width of the image in px.
#'     Default is 20.
#'
#' @param box_shadow Logical: add a box shadow to the bars.
#'     Default is FALSE.
#'
#' @param round_edges Logical: round the edges around the data bars.
#'     Default is FALSE.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "1s ease".
#'
#' @return a function that applies data bars
#'     to a column of numeric values.
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]
#'
#' ## By default, data bars are aligned left and text_position are placed on the inside end
#' reactable(data,
#'           defaultColDef = colDef(
#'             cell = data_bars(data)))
#'
#' ## Align the bars to the right
#' reactable(data,
#'           defaultColDef = colDef(
#'             cell = data_bars(data,
#'                              align_bars = "right")))
#'
#' ## Move the text labels outside of the filled bars
#' reactable(data,
#'           defaultColDef = colDef(
#'             cell = data_bars(data,
#'                              text_position = "outside-end")))
#'
#' ## Apply multiple fill_color to the filled bars
#' reactable(data,
#'           defaultColDef = colDef(
#'             cell = data_bars(data,
#'                              fill_color = c("lightblue","royalblue","navy"))))
#'
#' ## Apply a fill_gradient pattern to the filled bars
#' reactable(data,
#'           defaultColDef = colDef(
#'             cell = data_bars(data,
#'                              fill_color = c("lightblue","royalblue","navy"),
#'                              fill_gradient = TRUE,
#'                              text_position = "outside-end")))
#'
#'
#' @export


data_bars <- function(data,
                      fill_color = "#15607A",
                      fill_color_ref = NULL,
                      fill_opacity = 1,
                      fill_gradient = FALSE,
                      background = "#EEEEEE",
                      max_value = NULL,
                      min_value = NULL,
                      align_bars = "left",
                      bar_height = 19,
                      text_position = "inside-end",
                      force_outside = NULL,
                      text_color = "black",
                      text_color_ref = NULL,
                      text_size = 14,
                      brighten_text = TRUE,
                      brighten_text_color = "white",
                      bold_text = FALSE,
                      number_fmt = NULL,
                      border_width = NULL,
                      border_style = NULL,
                      border_color = NULL,
                      icon = NULL,
                      icon_ref = NULL,
                      icon_size = 20,
                      icon_color = NULL,
                      icon_color_ref = NULL,
                      img = NULL,
                      img_ref = NULL,
                      img_height = 20,
                      img_width = 20,
                      box_shadow = FALSE,
                      round_edges = FALSE,
                      animation = "1s ease") {

  cell <- function(value, index, name) {

    if (is.na(value) || !is.numeric(value)) return(value)

    ### stop messages
    if (!is.logical(fill_gradient)) {

      stop("`fill_gradient` must be TRUE or FALSE")
    }

    if (!is.logical(bold_text)) {

      stop("`bold_text` must be TRUE or FALSE")
    }

    if (!is.logical(box_shadow)) {

      stop("`box_shadow` must be TRUE or FALSE")
    }

    if (!is.logical(round_edges)) {

      stop("`round_edges` must be TRUE or FALSE")
    }

    if (!is.logical(brighten_text)) {

      stop("`brighten_text` must be TRUE or FALSE")
    }

    if (!is.null(max_value) & !is.numeric(max_value)) {

      stop("`max_value` must be numeric")
    }

    if (!is.numeric(fill_opacity)) {

      stop("`fill_opacity` must be numeric")
    }

    if (fill_opacity < 0 | fill_opacity > 1) {

      stop("`fill_opacity` must be a value between 0 and 1")
    }

    if (!align_bars %in% c("left","right")) {

      stop("`align_bars` must be either left or right")
    }

    if (!text_position %in% c("inside-base","inside-end","outside-base","outside-end","center","above","none")) {

      stop("`text_position` must be either above, center, inside-base, inside-end, outside-base, outside-end, or none")
    }

    if (length(text_color) > 1) {

      stop("multiple colors detected in `text_color`. only one color can be used.")
    }

    if (length(brighten_text_color) > 1) {

      stop("multiple colors detected in `brighten_text_color` only one color can be used.")
    }

    if (length(background) > 1) {

      stop("multiple colors detected in `background`. only one color can be used.")
    }

    if (!is.null(force_outside) & length(force_outside) != 2) {

      stop("must provide both a stop and start number for the range in `force_outside`. Ex. c(0,100).")
    }

    if (box_shadow == TRUE) {

      box_shadow <- "0 6px 6px -4px #888888"

    } else box_shadow <- NULL

    if (round_edges == TRUE) {

      radius <- "8px"

    } else radius <- "0px"

    ### optional formatter from scales package
    if (is.null(number_fmt)) {

      text_label <- value

    } else text_label <- number_fmt(value)

    ### color palette function
    color_pal <- function(x) {

      if (!is.na(x))
        rgb(grDevices::colorRamp(c(fill_color))(x), maxColorValue = 255)
      else
        NULL
    }

    assign_color <- function(x) {

      if (!is.na(x)) {
        rgb_sum <- rowSums(grDevices::colorRamp(c(fill_color))(x))
        color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)
        color
      } else
        NULL
    }

    ### normalization for color palette
    data_var <- stats::var(data[[name]], na.rm = TRUE)

    if (is.na(data_var) || data_var == 0) {

      normalized <- 1

    } else {

      normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

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
    if (is.character(fill_color_ref)) {

      if (all(fill_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(fill_color_ref)) { fill_color_ref <- which(names(data) %in% fill_color_ref) }

        fill_color_pal <- data[[fill_color_ref]][index]
        fill_color_pal <- grDevices::adjustcolor(fill_color_pal, alpha.f = fill_opacity)

        rgb_sum <- rowSums(grDevices::colorRamp(c(fill_color_pal))(1))

        if (brighten_text == TRUE & text_position == "inside-end" | brighten_text == TRUE & text_position == "inside-base" | brighten_text == TRUE & text_position == "center") {

          font_color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)

        } else font_color <- text_color

      } else {

        stop("Attempted to select non-existing column or non-character column with fill_color_ref")
      }

    } else {

      fill_color_pal <- color_pal(normalized)
      fill_color_pal <- grDevices::adjustcolor(fill_color_pal, alpha.f = fill_opacity)

      if (brighten_text == TRUE & is.null(text_color_ref) & text_position == "inside-end" | brighten_text == TRUE & is.null(text_color_ref) & text_position == "inside-base" | brighten_text == TRUE & is.null(text_color_ref) & text_position == "center") {

        font_color <- assign_color(normalized)

      } else font_color <- text_color
    }

    ### fill_gradient fill_color
    if (fill_gradient == TRUE & length(fill_color) > 1) {

      fill_color <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)

      gradient <- paste0("linear-gradient(to right,", paste(fill_color, collapse = ", "))

      fill_color_pal <- NULL

    } else if (fill_gradient == TRUE & length(fill_color) == 1) {

      stop("must provide at least two colors in `fill_color` when using `fill_gradient = TRUE`")

    } else gradient <- NULL

    ### icon_ref
    if (is.character(icon_ref)) {

      if (all(icon_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(icon_ref)) { icon_ref <- which(names(data) %in% icon_ref) }

        icon <- data[[icon_ref]][index]

        ### icon_color
        if (!is.null(icon_color)) {
          colors <- icon_color
        } else colors <- fill_color_pal

        ### icon_color_ref
        if (is.character(icon_color_ref)) {
          if (all(icon_color_ref %in% names(which(sapply(data, is.character))))) {

            if (is.character(icon_color_ref)) { icon_color_ref <- which(names(data) %in% icon_color_ref) }

            colors <- data[[icon_color_ref]][index]

          } else { stop("Attempted to select non-existing column or non-character column with icon_color_ref") }
        }

        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icon),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors, "; margin-left:7px"))

      } else {

        stop("Attempted to select non-existing column or non-character column with icon_ref")

      }

    } else if (!is.null(icon)) {

      ### icon_color
      if (!is.null(icon_color)) {
        colors <- icon_color
      } else colors <- fill_color_pal

      ### icon_color_ref
      if (is.character(icon_color_ref)) {
        if (all(icon_color_ref %in% names(which(sapply(data, is.character))))) {
          if (is.character(icon_color_ref)) { icon_color_ref <- which(names(data) %in% icon_color_ref) }

          colors <- data[[icon_color_ref]][index]

        } else { stop("Attempted to select non-existing column or non-character column with icon_color_ref") }
      }

      icon_label <- htmltools::tagAppendAttributes(shiny::icon(icon),
                                                   style = paste0("font-size:", icon_size, "px", "; color:", colors, "; margin-left:7px"))

    } else icon_label <- NULL

    ### img_ref
    if (is.character(img_ref)) {
      if (all(img_ref %in% names(which(sapply(data, is.character))))) {
        if (is.character(img_ref)) { img_ref <- which(names(data) %in% img_ref) }

        imgs <- data[[img_ref]][index]

        if (!is.character(imgs)) return(imgs)

        if (grepl("https|http|", imgs) == FALSE) {

          stop("must provide valid link to image")
        }

        img_label <- htmltools::img(src = imgs, height = img_height, width = img_width)

      } else { stop("Attempted to select non-existing column or non-character column with img_ref") }

    } else if (!is.null(img)) {

      if (!is.character(img)) return(img)

      if (grepl("https|http", img) == FALSE) {

        stop("must provide valid link to image")
      }

      img_label <- htmltools::img(src = img, height = img_height, width = img_width)

    } else img_label <- NULL

    ### height of data_bars
    height <- bar_height

    ### width of data_bars
    width <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {

      paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "%")

      ### min_value provided
    } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {

      paste0((abs(value) - min_value) / (max(abs(data[[name]]), na.rm = TRUE) - min_value) * 100, "%")

      ### max_value provided
    } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {

      paste0((abs(value) / max_value) * 100, "%")

      ### min and max provided
    } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {

      paste0((abs(value) - min_value) / (max_value - min_value) * 100, "%")

    } else if (!is.numeric(value)) {

      return(value)
    }

    if (bold_text == TRUE) {

      bold_text <- "bold"

    } else bold_text <- "normal"

    #### pos_neg charts
    neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
    pos_chart <- htmltools::div(style = list(flex = "1 1 0"))

    ### neg side
    if ((min(data[[name]], na.rm = TRUE) < 0)) {

      ### add opacity to fill
      fill_color <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)

      if (value < 0 & text_position == "outside-end") {

        ### assign negative color if two fill_color are provided
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, textAlign = "left", display = "flex", alignItems = "center"), icon_label, img_label)
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end", color = font_color, fontWeight = bold_text, fontSize = text_size, background = "transparent"), text_label, bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

      } else if (value < 0 & text_position == "inside-end") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {
          bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, textAlign = "left", display = "flex", alignItems = "center"), icon_label, img_label)
          chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end", color = text_color, fontWeight = bold_text, fontSize = text_size, background = "transparent"), text_label, bar)
          neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

        } else {

          bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                                icon_label, img_label,
                                htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "left", display = "flex", alignItems = "center", justifyContent = "flex-start", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontSize = text_size, fontWeight = bold_text, width = width, height = height, transition = animation, textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label))
          chart <-  htmltools::div(style = list(flexGrow = 1, background = "transparent"), bar)
          neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        }

      } else if (value < 0 & text_position == "outside-base") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              "", icon_label, img_label,
                              htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "left", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center")))
        chart <-  htmltools::div(style = list(flexGrow = 1, background = "transparent"), bar)
        bar_chart <- htmltools::div(style = list(display = "flex", alignItems = "center", color = font_color, fontSize = text_size, fontWeight = bold_text), chart, text_label)
        neg_chart <- htmltools::tagAppendChild(neg_chart, bar_chart)

      } else if (value < 0 & text_position == "inside-base") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {
          bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, textAlign = "left", display = "flex", alignItems = "center"), icon_label, img_label)
          chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end", color = text_color, fontWeight = bold_text, fontSize = text_size, background = "transparent"), text_label, bar)
          neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

        } else {

          bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                                icon_label, img_label,
                                htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, display = "flex", alignItems = "center", justifyContent = "flex-end", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontSize = text_size, fontWeight = bold_text, width = width, height = height, transition = animation, textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label))
          chart <-  htmltools::div(style = list(background = "transparent"), bar)
          neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        }

      } else if (value < 0 & text_position == "center") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {
          bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, textAlign = "left", display = "flex", alignItems = "center"), icon_label, img_label)
          chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end", color = text_color, fontWeight = bold_text, fontSize = text_size, background = "transparent"), text_label, bar)
          neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

        } else {

          bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                                icon_label, img_label,
                                htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "center", display = "flex", alignItems = "center", justifyContent = "center", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontSize = text_size, fontWeight = bold_text, width = width, height = height, transition = animation, textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label))
          chart <-  htmltools::div(style = list(flexGrow = 1, background = "transparent"), bar)
          neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        }

      } else if (value < 0 & text_position == "none") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "left", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontWeight = bold_text, fontSize = text_size, width = width, height = height, transition = animation, display = "flex", alignItems = "center"), icon_label, img_label))
        chart <-  htmltools::div(style = list(flexGrow = 1, background = "transparent"), bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

        ### pos side
      } else if (value >= 0 & text_position == "outside-end") {

        ### assign positive color if two fill_color are provided
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginRight = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = "transparent", color = font_color, fontSize = text_size, fontWeight = bold_text), bar, text_label)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)

      } else if (value >= 0 & text_position == "inside-end") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {
          bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginRight = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
          chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = "transparent", color = text_color, fontSize = text_size, fontWeight = bold_text), bar, text_label)
          pos_chart <- htmltools::tagAppendChild(pos_chart, chart)

        } else {

          bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                                htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "right", display = "flex", alignItems = "center", justifyContent = "flex-end", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontWeight = bold_text, fontSize = text_size, width = width, height = height, transition = animation, textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label), icon_label, img_label)
          chart <- htmltools::div(style = list(background = "transparent"), bar)
          pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        }

      } else if (value >= 0 & text_position == "outside-base") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                              htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center"), ""),
                              icon_label, img_label)
        chart <- htmltools::div(style = list(flexGrow = 1, background = "transparent"), bar)
        bar_chart <- htmltools::div(style = list(display = "flex", alignItems = "center", color = font_color, fontWeight = bold_text, fontSize = text_size, display = "flex", alignItems = "center"), text_label, chart)
        pos_chart <- htmltools::tagAppendChild(pos_chart, bar_chart)

      } else if (value >= 0 & text_position == "inside-base") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {
          bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginRight = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
          chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = "transparent", color = text_color, fontSize = text_size, fontWeight = bold_text), bar, text_label)
          pos_chart <- htmltools::tagAppendChild(pos_chart, chart)

        } else {

          bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                                htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "left", display = "flex", alignItems = "center", justifyContent = "flex-start", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontWeight = bold_text, fontSize = text_size, width = width, height = height, transition = animation, textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label),
                                icon_label, img_label)
          chart <- htmltools::div(style = list(background = "transparent"), bar)
          pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        }

      } else if (value >= 0 & text_position == "center") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal

        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color

        if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {
          bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, marginRight = "7px", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
          chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = "transparent", color = text_color, fontSize = text_size, fontWeight = bold_text), bar, text_label)
          pos_chart <- htmltools::tagAppendChild(pos_chart, chart)

        } else {

          bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                                htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "center", display = "flex", alignItems = "center", justifyContent = "center", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), color = font_color, fontWeight = bold_text, fontSize = text_size, width = width, height = height, transition = animation, textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label), icon_label, img_label)
          chart <- htmltools::div(style = list(background = "transparent"), bar)
          pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        }

      } else if (value >= 0 & text_position == "none") {

        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal

        bar <- htmltools::div(style = list(boxShadow = box_shadow, borderTopLeftRadius = radius, borderBottomLeftRadius = radius, borderTopRightRadius = radius, borderBottomRightRadius = radius, textAlign = "right", background = fill_color_pal, backgroundImage = gradient, border = paste0("", border_width, " ", border_style, " ", border_color, ""), width = width, height = height, transition = animation, display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = "transparent"),
                                bar)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
      }

      htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)

    } else {

      ### data_bars for positive values only
      bar_chart <-
        function(text_label,
                 icon_label,
                 img_label,
                 width = "100%",
                 height = "100%",
                 fill = NULL,
                 background = NULL,
                 color = NULL,
                 fontWeight = NULL) {

          if (align_bars == "right" & text_position == "outside-end") {

            chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                text_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginLeft = "6px",
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))

            back <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              chart)

            htmltools::div(back)

          } else if (align_bars == "right" & text_position == "above") {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                htmltools::div(style = list(
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  boxShadow = box_shadow,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = 5,
                  transition = animation
                ),
                icon_label,
                img_label))

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background),
              fill_chart)

            htmltools::div(
            htmltools::div(text_label,
                  style = list(display = "flex",
                               alignItems = "center",
                               justifyContent = "flex-end",
                               textAlign = "right",
                               color = text_color,
                               fontWeight = bold_text,
                               fontSize = text_size)),
                  back_chart)

          } else if (align_bars == "right" & text_position == "inside-end") {

            if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {

            chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                text_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginLeft = "6px",
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))

            back <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              chart)

            htmltools::div(back)

            } else {

              fill_chart <-
                htmltools::div(style = list(
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end"),
                  icon_label,
                  img_label,
                  htmltools::div(style = list(
                    textAlign = "left",
                    display = "flex",
                    alignItems = "center",
                    justifyContent = "flex-start",
                    boxShadow = box_shadow,
                    borderTopLeftRadius = radius,
                    borderBottomLeftRadius = radius,
                    borderTopRightRadius = radius,
                    borderBottomRightRadius = radius,
                    border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                    background = fill,
                    backgroundImage = gradient,
                    color = font_color,
                    fontWeight = bold_text,
                    fontSize = text_size,
                    width = width,
                    height = height,
                    transition = animation,
                    textOverflow = "ellipsis",
                    whiteSpace = "nowrap"
                  ), text_label))

              back_chart <-
                htmltools::div(style = list(
                  flexGrow = 1,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  background = background
                ),
                fill_chart)

              htmltools::div(back_chart)
            }

          } else if (align_bars == "right" & text_position == "inside-base") {

            if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {

            chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                text_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginLeft = "6px",
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))

            back <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              chart)

            htmltools::div(back)

            } else {

              fill_chart <-
                htmltools::div(style = list(
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end"),
                  icon_label,
                  img_label,
                  htmltools::div(style = list(
                    textAlign = "right",
                    display = "flex",
                    alignItems = "center",
                    justifyContent = "flex-end",
                    boxShadow = box_shadow,
                    borderTopLeftRadius = radius,
                    borderBottomLeftRadius = radius,
                    borderTopRightRadius = radius,
                    borderBottomRightRadius = radius,
                    border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                    background = fill,
                    backgroundImage = gradient,
                    color = font_color,
                    fontWeight = bold_text,
                    fontSize = text_size,
                    width = width,
                    height = height,
                    transition = animation,
                    textOverflow = "ellipsis",
                    whiteSpace = "nowrap"
                  ), text_label))

              back_chart <-
                htmltools::div(style = list(
                  flexGrow = 1,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  background = background
                ),
                fill_chart)

              htmltools::div(back_chart)
            }

          } else if (align_bars == "right" & text_position == "outside-base") {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                "",
                icon_label,
                img_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  backgroundImage = gradient,
                  color = text_color,
                  fontWeight = bold_text,
                  fontSize = text_size,
                  width = width,
                  height = height,
                  transition = animation,
                  display = "flex",
                  alignItems = "center"
                )))

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                marginRight = "7px",
                background = background
              ),
              fill_chart)

            htmltools::div(style = list(display = "flex",
                                        alignItems = "center",
                                        color = font_color),
                           back_chart,
                           text_label)

          } else if (align_bars == "right" & text_position == "center") {

            if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {

            chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                text_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginLeft = "6px",
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))

            back <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              chart)

            htmltools::div(back)

            } else {

              fill_chart <-
                htmltools::div(style = list(
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end"),
                  icon_label,
                  img_label,
                  htmltools::div(style = list(
                    textAlign = "center",
                    display = "flex",
                    alignItems = "center",
                    justifyContent = "center",
                    boxShadow = box_shadow,
                    borderTopLeftRadius = radius,
                    borderBottomLeftRadius = radius,
                    borderTopRightRadius = radius,
                    borderBottomRightRadius = radius,
                    border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                    background = fill,
                    backgroundImage = gradient,
                    color = font_color,
                    fontWeight = bold_text,
                    fontSize = text_size,
                    width = width,
                    height = height,
                    transition = animation,
                    textOverflow = "ellipsis",
                    whiteSpace = "nowrap"
                  ), text_label))

              back_chart <-
                htmltools::div(style = list(
                  flexGrow = 1,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  # marginLeft = "7px",
                  background = background
                ),
                fill_chart)

              htmltools::div(back_chart)
            }

          } else if (align_bars == "right" & text_position == "none") {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                htmltools::div(style = list(
                  textAlign = "left",
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                borderBottomLeftRadius = radius,
                # marginLeft = "7px",
                background = background
              ),
              fill_chart)

            htmltools::div(style = list(display = "flex", alignItems = "center"),
                           back_chart)

          } else if (align_bars == "left" & text_position == "outside-end") {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                color = font_color,
                fontSize = text_size,
                fontWeight = bold_text),
                htmltools::div(style = list(
                  alignText = "right",
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end",
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  boxShadow = box_shadow,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginRight = "6px"
                ),
                icon_label,
                img_label),
                text_label)

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius =  radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              fill_chart)

            htmltools::div(back_chart)

          } else if (align_bars == "left" & text_position == "above") {

            fill_chart <-
                htmltools::div(style = list(
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  boxShadow = box_shadow,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = 5,
                  transition = animation
                ),
                icon_label,
                img_label)

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background),
              fill_chart)

            htmltools::div(
            htmltools::div(text_label,
                  style = list(display = "flex",
                               alignItems = "center",
                               justifyContent = "flex-start",
                               textAlign = "left",
                               color = text_color,
                               fontWeight = bold_text,
                               fontSize = text_size)),
                  back_chart)

          } else if (align_bars == "left" & text_position == "inside-end") {

            if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                htmltools::div(style = list(
                  alignText = "right",
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end",
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  boxShadow = box_shadow,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginRight = "6px"
                ),
                icon_label,
                img_label),
                text_label)

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius =  radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              fill_chart)

            htmltools::div(back_chart)

            } else {

              fill_chart <-
                htmltools::div(style = list(
                  display = "flex",
                  alignItems = "center"),
                  htmltools::div(style = list(
                    boxShadow = box_shadow,
                    textAlign = "right",
                    display = "flex",
                    alignItems = "center",
                    justifyContent = "flex-end",
                    boxShadow = box_shadow,
                    borderTopLeftRadius = radius,
                    borderBottomLeftRadius = radius,
                    borderTopRightRadius = radius,
                    borderBottomRightRadius = radius,
                    border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                    background = fill,
                    backgroundImage = gradient,
                    color = font_color,
                    fontWeight = bold_text,
                    fontSize = text_size,
                    width = width,
                    height = height,
                    transition = animation,
                    textOverflow = "ellipsis",
                    whiteSpace = "nowrap"
                    # marginRight = "7px"
                  ), text_label),
                  icon_label,
                  img_label)

              back_chart <-
                htmltools::div(style = list(
                  flexGrow = 1,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  background = background
                ),
                fill_chart)

              htmltools::div(back_chart)
            }

          } else if (align_bars == "left" & text_position == "inside-base") {

            if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                htmltools::div(style = list(
                  alignText = "right",
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end",
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  boxShadow = box_shadow,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginRight = "6px"
                ),
                icon_label,
                img_label),
                text_label)

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius =  radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              fill_chart)

            htmltools::div(back_chart)

            } else {

              fill_chart <-
                htmltools::div(style = list(
                  display = "flex",
                  alignItems = "center"),
                  htmltools::div(style = list(
                    textAlign = "left",
                    display = "flex",
                    alignItems = "center",
                    justifyContent = "flex-start",
                    boxShadow = box_shadow,
                    borderTopLeftRadius = radius,
                    borderBottomLeftRadius = radius,
                    borderTopRightRadius = radius,
                    borderBottomRightRadius = radius,
                    border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                    background = fill,
                    backgroundImage = gradient,
                    color = font_color,
                    fontWeight = bold_text,
                    fontSize = text_size,
                    width = width,
                    height = height,
                    transition = animation,
                    textOverflow = "ellipsis",
                    whiteSpace = "nowrap"
                    # marginRight = "6px"
                  ), text_label),
                  icon_label,
                  img_label)

              back_chart <-
                htmltools::div(style = list(
                  flexGrow = 1,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  background = background
                ),
                fill_chart)

              htmltools::div(back_chart)
            }

          } else if (align_bars == "left" & text_position == "outside-base") {

            bar <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  background = fill,
                  backgroundImage = gradient,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  color = font_color,
                  width = width,
                  height = height,
                  transition = animation,
                  display = "flex",
                  alignItems = "center"
                ), ""),
                icon_label,
                img_label)

            chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                marginLeft = "7px",
                background = background
              ),
              bar)

            htmltools::div(style = list(display = "flex",
                                        alignItems = "center",
                                        color = font_color,
                                        fontSize = text_size,
                                        fontWeight = bold_text),
                           text_label,
                           chart)

          } else if (align_bars == "left" & text_position == "center") {

            if (!is.null(force_outside) && dplyr::between(value, force_outside[[1]], force_outside[[2]]) == TRUE) {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                color = text_color,
                fontSize = text_size,
                fontWeight = bold_text),
                htmltools::div(style = list(
                  alignText = "right",
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end",
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  boxShadow = box_shadow,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  marginRight = "6px"
                ),
                icon_label,
                img_label),
                text_label)

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius =  radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              fill_chart)

            htmltools::div(back_chart)

            } else {

              fill_chart <-
                htmltools::div(style = list(
                  display = "flex",
                  alignItems = "center"),
                  htmltools::div(style = list(
                    textAlign = "center",
                    display = "flex",
                    alignItems = "center",
                    justifyContent = "center",
                    boxShadow = box_shadow,
                    borderTopLeftRadius = radius,
                    borderBottomLeftRadius = radius,
                    borderTopRightRadius = radius,
                    borderBottomRightRadius = radius,
                    border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                    background = fill,
                    backgroundImage = gradient,
                    color = font_color,
                    fontWeight = bold_text,
                    fontSize = text_size,
                    width = width,
                    height = height,
                    transition = animation,
                    textOverflow = "ellipsis",
                    whiteSpace = "nowrap"
                    # marginRight = "6px"
                  ), text_label),
                  icon_label,
                  img_label)

              back_chart <-
                htmltools::div(style = list(
                  flexGrow = 1,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  background = background
                ),
                fill_chart)

              htmltools::div(back_chart)
            }

          } else if (align_bars == "left" & text_position == "none" | is.null(align_bars) & text_position == "none") {

            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  textAlign = "right",
                  boxShadow = box_shadow,
                  borderTopLeftRadius = radius,
                  borderBottomLeftRadius = radius,
                  borderTopRightRadius = radius,
                  borderBottomRightRadius = radius,
                  border = paste0("", border_width, " ", border_style, " ", border_color, ""),
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = animation,
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end"
                ),
                icon_label,
                img_label))

            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                borderTopLeftRadius = radius,
                borderBottomLeftRadius = radius,
                borderTopRightRadius = radius,
                borderBottomRightRadius = radius,
                background = background
              ),
              fill_chart)

            htmltools::div(back_chart)
          }
        }

      ### adjust spacing so numbers align properly in outside-base
      if (text_position == "outside-base") {

        max_digits <- max(nchar(data[[name]]), na.rm = TRUE)+1

        text_label <- stringr::str_pad(text_label, max_digits)

      } else text_label <- text_label

      bar_chart(text_label,
                icon_label,
                img_label,
                height = height,
                width = width,
                fill = fill_color_pal,
                background = background,
                color = font_color,
                fontWeight = bold_text)
    }
  }
}

