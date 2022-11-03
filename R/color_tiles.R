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
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
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
#' @param box_shadow Logical: add a box shadow to the tiles.
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
#'     Default is FALSE.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "background 1s ease".
#'
#' @param tooltip Logical: hover tooltip.
#'     Default is FALSE.
#'
#' @param tooltip_show_name Logical: If set to TRUE, shows the name of the column before the value.
#'     If set to FALSE, hides the name of the column and only shows the value.
#'     Default is FALSE.
#'
#' @param tooltip_number_fmt Format values using formatters from the scales package or a user-defined function.
#'     Default is NULL.
#'
#' @param tooltip_style Apply a CSS style to the value within the tooltip.
#'     Must provide valid CSS code, i.e. color:red; font-style:italic;, etc.
#'     Default is NULL.
#'
#' @param tooltip_dotted_line Add a dotted line underneath the values in the column to signal to users a tooltip is enabled.
#'     Default is NULL.
#'
#' @param tooltip_theme The theme of the tooltip.
#'     Options are: 'light', 'light-border', 'material', or 'translucent'.
#'     See https://atomiks.github.io/tippyjs/v5/themes/ for examples.
#'     Default is 'material'.
#'
#' @param tooltip_arrow Logical: determines if the tooltip box has an arrow.
#'     Default is TRUE.
#'
#' @param tooltip_trigger An event that causes the tooltip to show.
#'     Options are: 'click', 'focus', 'focusin', 'manual', or 'mouseenter'.
#'     Default is 'mouseenter'.
#'
#' @param tooltip_animation The type of transition animation for the tooltip.
#'     Options are: 'fade', 'perspective', 'shift-away', 'shift-toward', or 'scale'.
#'     Default is 'fade'.
#'
#' @param tooltip_duration The duration of the transition animation for the tooltip.
#'     Possible values are a single number or a vector of two numbers for the show/hide, i.e. c(200,300).
#'     If only one value is provided, it will be used for both the show/hide.
#'     Default is c(275,250).
#'
#' @param tooltip_distance How far in pixels the tooltip is from the cell.
#'     Possible values are a number, i.e. 5 or a string with units 'rem', i.e. '5rem'.
#'     Default is 10.
#'
#' @param tooltip_placement Where the tooltip appears relative to the cell.
#'     Options are: 'top', 'right', 'bottom', or 'left'.
#'     Default is 'top'.
#'
#' @param tooltip_auto_adjust Logical: if TRUE, then automatically adjust placement of tooltip show it can be viewed within viewport.
#'     Default is TRUE.
#'
#' @param tooltip_img_size The size (height, width) of the image displayed (if valid image link is present).
#'     Possible values are a single number or a vector of two numbers for height/width, i.e. c(50,40).
#'     Default is c(26,26).
#'
#' @param tooltip_secondary_columns Show text/values from other columns within the dataset.
#'     Can provide a single column name or a vector of column names, i.e. c('col1','col2','col3)
#'     Default is NULL.
#'
#' @param tooltip_show_name_secondary Logical: if TRUE, then show the name of the secondary column before the value.
#'     If set to FALSE, hides the name of the secondary column and only shows the value.
#'     Default is TRUE.
#'
#' @param tooltip_number_fmt_secondary Format secondary values using formatters from the scales package or a user-defined function.
#'     The number of formatters must be the same as the number of column names provided with `secondary_columns`.
#'     The order of the formatters must match the order of names provided within `secondary_columns`.
#'     If you do not want to format one or more of the columns, use NA for that column, i.e. c(scales::percent, NA, scales::dollar)
#'     Default is NULL.
#'
#' @param tooltip_style_secondary Apply a CSS style to the secondary values within the tooltip.
#'     Must provide valid CSS code, i.e. color:red; font-style:italic;, etc.
#'     Default is NULL.
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
#' ## Span can take column names
#' reactable(data,
#' defaultColDef = colDef(cell = color_tiles(data, span = c("Sepal.Length", "Sepal.Width"))))
#'
#' ## Or it can also take column positions instead
#' reactable(data,
#' defaultColDef = colDef(cell = color_tiles(data, span = 1:2)))
#'
#' ## Use number_fmt to format numbers using the scales package
#' car_prices <- MASS::Cars93[20:49, c("Make", "Price")]
#'
#' reactable(car_prices,
#' defaultColDef = colDef(cell = color_tiles(car_prices,
#' number_fmt = scales::dollar)))
#' @export

color_tiles <- function(data,
                        colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                        color_ref = NULL,
                        color_by = NULL,
                        opacity = 1,
                        bias = 1,
                        number_fmt = NULL,
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
                        box_shadow = FALSE,
                        border_width = NULL,
                        border_style = NULL,
                        border_color = NULL,
                        span = FALSE,
                        animation = "background 1s ease",
                        tooltip = FALSE,
                        tooltip_show_name = FALSE,
                        tooltip_number_fmt = NULL,
                        tooltip_style = NULL,
                        tooltip_dotted_line = FALSE,
                        tooltip_theme = "material",
                        tooltip_arrow = TRUE,
                        tooltip_trigger = "mouseenter",
                        tooltip_animation = "fade",
                        tooltip_duration = c(275,250),
                        tooltip_distance = 10,
                        tooltip_placement = "top",
                        tooltip_auto_adjust = TRUE,
                        tooltip_img_size = c(26,26),
                        tooltip_secondary_columns = NULL,
                        tooltip_show_name_secondary = TRUE,
                        tooltip_number_fmt_secondary = NULL,
                        tooltip_style_secondary = NULL) {

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

  if (!is.numeric(bias)) {

    stop("`bias` must be numeric")
  }

  if (!is.numeric(opacity)) {

    stop("`opacity` must be numeric")
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

  if (!is.logical(even_breaks)) {

    stop("`even_breaks` must be TRUE or FALSE.")
  }

  if (!is.logical(tooltip)) {

    stop("`tooltip` must be TRUE or FALSE.")
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

  if (!is.logical(even_breaks)) {

    stop("`even_breaks` must be TRUE or FALSE.")
  }

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

  cell <- function(value, index, name) {

    if (is.null(color_ref) & is.null(color_by) & !is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else {

      label <- number_fmt(value)

    }

    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)

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

          if (!is.null(min_value) & isTRUE(min_value > min(data[[name]], na.rm = TRUE))) {

            stop("`min_value` must be less than the minimum value observed in the data")
          }

          if (!is.null(max_value) & isTRUE(max_value < max(data[[name]], na.rm = TRUE))) {

            stop("`max_value` must be greater than the maximum value observed in the data")
          }
          
          null_replace <- function(a, b) if (is.null(a)) b else a
          effective_min_value <- null_replace(min_value, min(data[[name]], na.rm = TRUE)))
          effective_max_value <- null_replace(max_value, max(data[[name]], na.rm = TRUE)))          
          range <- effective_max_value - effective_min_value
          normalized <- if (range > 0) (value - min_value_normal) / range else 1
            
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
                                  fontSize = text_size,
                                  borderStyle = border_style,
                                  borderWidth = border_width,
                                  borderColor = border_color,
                                  transition = animation))

    } else if (brighten_text == TRUE & !is.null(text_color_ref) & show_text == TRUE) {

      htmltools::div(label,
                     style = list(background = cell_color,
                                  color = text_color,
                                  display = "flex",
                                  justifyContent = "center",
                                  height = "18px",
                                  borderRadius = "6px",
                                  boxShadow = box_shadow,
                                  fontSize = text_size,
                                  borderStyle = border_style,
                                  borderWidth = border_width,
                                  borderColor = border_color,
                                  transition = animation))

    } else if (show_text == FALSE) {

      htmltools::div(label,
                     style = list(background = cell_color,
                                  display = "flex",
                                  justifyContent = "center",
                                  height = "18px",
                                  borderRadius = "6px",
                                  color = "transparent",
                                  boxShadow = box_shadow,
                                  fontSize = text_size,
                                  borderStyle = border_style,
                                  borderWidth = border_width,
                                  borderColor = border_color,
                                  transition = animation))

    } else if (tooltip == TRUE) {

      if (!is.logical(tooltip_arrow)) {

        stop("`tooltip_arrow` must be TRUE or FALSE")
      }

      if (!is.logical(tooltip_dotted_line)) {

        stop("`tooltip_dotted_line` must be TRUE or FALSE")
      }

      if (!is.logical(tooltip_show_name)) {

        stop("`tooltip_show_name` must be TRUE or FALSE")
      }

      if (!is.logical(tooltip_show_name_secondary)) {

        stop("`tooltip_show_name_secondary` must be TRUE or FALSE")
      }

      if (!tooltip_theme %in% c("light", "light-border", "material", "translucent") == TRUE) {

        stop("`tooltip_theme` must be either 'light', 'light-border', 'material', or 'translucent'")
      }

      if (!tooltip_trigger %in% c("click", "focus", "focusin", "manual", "mouseenter") == TRUE) {

        stop("`tooltip_trigger` must be either 'click', 'focus', 'focusin', 'manual', or 'mouseenter'")
      }

      if (!tooltip_animation %in% c("fade", "perspective", "shift-away", "shift-toward", "scale") == TRUE) {

        stop("`tooltip_animation` must be either 'fade', 'perspective', 'shift-away', 'shift-toward', or 'scale'")
      }

      if (!tooltip_placement %in% c("top", "right", "bottom", "left") == TRUE) {

        stop("`tooltip_placement` must be either 'top', 'right', 'bottom', or 'left'")
      }

      if (!is.null(tooltip_duration) & !is.numeric(tooltip_duration)) {

        stop("`tooltip_duration` must be a single number (i.e. 100) or a vector of length 2 (i.e. c(100,50))")
      }

      if (!is.logical(tooltip_auto_adjust)) {

        stop("`tooltip_auto_adjust` must be TRUE or FALSE")
      }

      if (!is.null(tooltip_number_fmt_secondary) & (length(tooltip_number_fmt_secondary) != length(tooltip_secondary_columns))) {

        stop("`tooltip_number_fmt_secondary` length is ", length(tooltip_number_fmt_secondary), " but ", length(tooltip_secondary_columns), " were provided.
           Please provide ", length(tooltip_secondary_columns), " formatters within `tooltip_number_fmt_secondary`.
           If you do not wish to provide a formatter for a column, denote with NA for that column in `tooltip_number_fmt_secondary`.")
      }

      if (is.null(tooltip_style)) {

        tooltip_style <- ''

      } else tooltip_style

      if (is.null(tooltip_style_secondary)) {

        tooltip_style_secondary <- ''

      } else tooltip_style_secondary


      if (is.null(tooltip_number_fmt)) {

        label <- value

      } else {

        label <- tooltip_number_fmt(value)
      }


      if (is.null(tooltip_duration)) {

        tooltip_duration <- c(275,250)

      }

      if (!is.null(tooltip_img_size) & length(tooltip_img_size) > 2) {

        stop("`tooltip_img_size`accepts either 1 or 2 values.")
      }

      if (!is.null(tooltip_img_size) & length(tooltip_img_size) == 1) {

        height = tooltip_img_size[[1]]
        width = tooltip_img_size[[1]]

      } else {

        height = tooltip_img_size[[1]]
        width = tooltip_img_size[[2]]
      }

      if (grepl("https|http", label) == TRUE) {

        label <- htmltools::img(src = value, align = "center", height = height, width = width)

      } else label <- label


      # show column name
      if (tooltip_show_name == TRUE) {

        primary_label <- glue::glue("<caption>{name}: {label}</caption>")
        primary_label <- paste0('<span style="font-size:1.2em; display:block; font-weight:bold; text-align:left;',tooltip_style,'">',primary_label,'</span>')


      } else {

        primary_label <- glue::glue("<caption>{label}</caption>")
        primary_label <- paste0('<span style="font-size:1.2em; display:block; font-weight:bold; text-align:left;',tooltip_style,'">',primary_label,'</span>')

      }

      if (!is.null(tooltip_secondary_columns)) {

        if (all(tooltip_secondary_columns %in% names(data))) {

          # get values from other columns specified
          get_secondary_values <- function(tooltip_secondary_columns) {

            data[[tooltip_secondary_columns]][index]

          }

          secondary_values <- purrr::map(tooltip_secondary_columns, get_secondary_values)

          # if the cell contains a link to an image
          display_img <- function(secondary_values) {

            if (grepl("https|http", secondary_values) == TRUE) {
              htmltools::img(src = secondary_values, align = "center", height = height, width = width)
            } else secondary_values <- secondary_values

          }

          secondary_values <- purrr::map(secondary_values, display_img)

          # replace NA with empty function
          if (!is.null(tooltip_number_fmt_secondary)) {

            tooltip_number_fmt_secondary <- lapply(tooltip_number_fmt_secondary,
                                                   function(x) ifelse(suppressWarnings(is.na(x)) == TRUE,
                                                                      function(x) {return(x)},
                                                                      x))

            # apply formats to secondary_values
            result <- list()

            for(i in seq_along(tooltip_number_fmt_secondary)){

              formats_applied <- tooltip_number_fmt_secondary[[i]](secondary_values[[i]])

              result[[i]] <- list(formats_applied)

            }

            secondary_values <- result

          }

          inputs <- function(tooltip_secondary_columns, secondary_values) {

            if (tooltip_show_name_secondary == TRUE) {

              glue::glue("<td>{tooltip_secondary_columns}: </td>
                      <td>{secondary_values}</td>")

            } else glue::glue("<td>{secondary_values}</td>")
          }

          secondary_labels <- purrr::map2(tooltip_secondary_columns, secondary_values, inputs)

          secondary_label <- glue::glue_collapse(
            glue::glue("<tr>{secondary_labels}</tr>")
          )

          tooltip_label <- paste0(glue::glue('{primary_label}<span style="font-size:1.2em; text-align:left; ',tooltip_style_secondary,'"><table>',secondary_label,'</table></span>'))

        } else {

          stop("Attempted to select non-existing column or non-character column with `tooltip_secondary_columns`")
        }

      } else tooltip_label <- primary_label

      if (tooltip_dotted_line == TRUE) {

        textDecoration = "underline"
        textDecorationStyle = "dotted"
        cursor = "help"

      } else {

        textDecoration = ""
        textDecorationStyle = ""
        cursor = ""
      }

      htmltools::tagAppendChild(
        htmltools::div(
          style = list(textDecoration = textDecoration,
                       textDecorationStyle = textDecorationStyle,
                       cursor = cursor,
                       background = cell_color,
                       color = font_color,
                       display = "flex",
                       flexDirection = "column",
                       justifyContent = "center",
                       alignItems = "center",
                       borderRadius = "6px",
                       boxShadow = box_shadow,
                       fontWeight = bold_text,
                       fontSize = text_size,
                       borderStyle = border_style,
                       borderWidth = border_width,
                       borderColor = border_color,
                       transition = animation)),
        tippy::tippy(label,
                     arrow = tooltip_arrow,
                     animation = tooltip_animation,
                     duration = tooltip_duration,
                     distance = tooltip_distance,
                     followCursor = TRUE,
                     theme = tooltip_theme,
                     trigger = tooltip_trigger,
                     placement = tooltip_placement,
                     flip = tooltip_auto_adjust,
                     tooltip = tooltip_label)
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
                                  fontSize = text_size,
                                  borderStyle = border_style,
                                  borderWidth = border_width,
                                  borderColor = border_color,
                                  transition = animation))
    }
  }
}
