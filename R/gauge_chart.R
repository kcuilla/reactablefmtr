#' Display numeric values in a gauge chart
#'
#' The `gauge_chart()` function displays numeric values in a column in a gauge or aka a speedometer chart.
#'     The minimum and maximum values that are present in the column can be added to the gauge chart by setting `show_min_max` to TRUE.
#'     By default, the minimum and maximum bounds of the gauge chart are the min/max of the column, but can be changed with `min_value` and `max_value`.
#'     The format of the min/max values and the values shown within the gauge chart can be changed with `number_fmt`.
#'     There are two sizes available for the gauge. The smaller default size 1 and the bigger size 2. The size can be specified within `size`.
#'     Many options that are available in `data_bars()` are also available in `gauge_chart()`.
#'     There are a few different ways to color the fill of the gauge.
#'     One way would be to apply either a single or multiple colors within `fill_color`.
#'     Colors may be assigned via another column if referenced within `fill_color_ref`.
#'     The opacity of the fill color can be controlled with `opacity`.
#'     If multiple colors are used within `fill_color`, the bias of the color normalization can be controlled with `bias`.
#'     The empty fill of the gauge can be colored with `background`.
#'     The color of the values within the gauge can be changed using `text_color`.
#'     Or they can be assigned via another column with `text_color_ref`.
#'     `gauge_chart()` needs to placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param fill_color A single color or a vector of fill_color for the fill of the gauge.
#'     fill_color should be given in order from low values to high values.
#'     Can use R's built-in fill_color or other color packages.
#'     Default is #15607A.
#'
#' @param background The color for the background of the gauge.
#'     Default is #EEEEEE.
#'
#' @param show_min_max Show or hide the min and max values on the gauge.
#'     Default is FALSE.
#'
#' @param size Size of the gauge.
#'     Options are 1 (small gauge) or 2 (large gauge).
#'     Default is 1.
#'
#' @param min_value A value to use as the minimum value for the gauge.
#'     The default minimum value is 0.
#'     Default is NULL.
#'
#' @param max_value A value to use as the maximum value for the gauge.
#'     The default maximum value is the maximum value in the column.
#'     Default is NULL.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is NULL.
#'
#' @param fill_color_ref Assign colors to the fill of the gauge via another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one color can be provided per row.
#'     Default is NULL.
#'
#' @param text_color The color of the value shown within the gauge.
#'     Default is black.
#'
#' @param bold_text Logical: bold the text of the value within the gauge.
#'     Default is FALSE.
#'
#' @param min_text_color The color of the minimum value shown underneath the gauge.
#'     Default is black.
#'
#' @param max_text_color The color of the maximum value shown underneath the gauge.
#'     Default is black.
#'
#' @param text_color_ref Assign color to the text within the gauge via another column
#'     by providing the name of the column containing the text colors in quotes.
#'     Only one color can be provided per cell.
#'     Default is NULL.
#'
#' @param text_size Numeric value representing the size of the value within the gauge.
#'     Default is NULL.
#'
#' @param bias A positive value that determines the spacing between multiple colors.
#'     A higher value spaces out the colors at the higher end more than a lower number.
#'     Default is 1.
#'
#' @param opacity A value between 0 and 1 that adjusts the opacity of fill_color.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param tooltip Logical: hover tooltip.
#'     Default is FALSE.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "background 1s ease".
#'
#' @return a function that displays values in a column in a gauge chart.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' library(dplyr)
#' data <- iris[45:54, ]
#'
#' ## Show values within a gauge chart:
#' reactable(
#' data,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(data)))
#'
#' ## Show the min and max below the gauge:
#' reactable(
#' data,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(data, show_min_max = TRUE)))
#'
#' ## Adjust the min and max value of the gauge:
#' reactable(
#' data,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(data, show_min_max = TRUE, min_value = 0, max_value = 7)))
#'
#' ## Increase the size of the gauge chart:
#' reactable(
#' data,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(data, size = 2)))
#'
#' ## Assign multiple colors to create a normalized fill based on value:
#' reactable(
#' data,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(data, fill_color = c("blue","white","orange"))))
#'
#' ## Conditionally apply colors from another column:
#' data %>%
#' mutate(color_assign = case_when(
#' Species == "setosa" ~ "red",
#' Species == "versicolor" ~ "forestgreen",
#' TRUE ~ "grey")) %>%
#' reactable(
#' .,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(., fill_color_ref = "color_assign")))
#'
#' ## Change the color of the empty fill of the gauge:
#' reactable(
#' data,
#' defaultColDef = colDef(
#' align = "left",
#' maxWidth = 150,
#' cell = gauge_chart(data, background = "transparent")))
#' @export

gauge_chart <- function(data,
                        fill_color = "#15607A",
                        background = "#EEEEEE",
                        show_min_max = FALSE,
                        size = 1,
                        min_value = NULL,
                        max_value = NULL,
                        number_fmt = NULL,
                        fill_color_ref = NULL,
                        text_color = "black",
                        bold_text = FALSE,
                        min_text_color = "black",
                        max_text_color = "black",
                        text_color_ref = NULL,
                        text_size = NULL,
                        bias = 1,
                        opacity = 1,
                        tooltip = FALSE,
                        animation = "transform 1s ease") {

  if (!is.logical(bold_text)) {

    stop("`bold_text` must be TRUE or FALSE")
  }

  if (!is.logical(show_min_max)) {

    stop("`show_min_max` must be TRUE or FALSE")
  }

  if (!is.logical(tooltip)) {

    stop("`tooltip` must be TRUE or FALSE")
  }

  if (!is.null(min_value) & !is.numeric(min_value)) {

    stop("`min_value` must be numeric")
  }

  if (!is.null(max_value) & !is.numeric(max_value)) {

    stop("`max_value` must be numeric")
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

  if (length(background) > 1) {

    stop("multiple colors detected in `background`. only one color can be used.")
  }

  if (!is.numeric(size)) {

    stop("`size` must be numeric")
  }

  '%notin%' <- Negate('%in%')

  if (size %notin% c(1,2)) {

    stop("`size` must be either 1 or 2")
  }

  if (bold_text == TRUE) {

    bold_text <- "bold"

  } else bold_text <- "normal"

  ### color palette function
  color_pal <- function(x) {

    if (!is.na(x))
      rgb(grDevices::colorRamp(c(fill_color), bias = bias)(x), maxColorValue = 255)
    else
      NULL
  }

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    ### assign min value if applied
    if (!is.null(min_value)) {

      min_label <- min_value

    } else { min_label <- min(abs(data[[name]]), na.rm = TRUE) }

    ### assign max value if applied
    if (!is.null(max_value)) {

      max_label <- max_value

    } else { max_label <- max(abs(data[[name]]), na.rm = TRUE) }

    ### optional formatter from scales package
    if (is.null(number_fmt)) {

      label <- value

    } else {

      label <- number_fmt(value)
      min_label <- number_fmt(min_label)
      max_label <- number_fmt(max_label)
    }

    # normalization if contains zero variance
    if (is.numeric(value) & mean((data[[name]] - mean(data[[name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {

      normalized <- 1

    } else {

      # standard normalization
      normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))

    }

    fill <- color_pal(normalized)
    fill <- suppressWarnings(grDevices::adjustcolor(fill, alpha.f = opacity))

    ### conditional text color
    if (is.character(text_color_ref)) {

      if (all(text_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(text_color_ref)) { text_color_ref <- which(names(data) %in% text_color_ref) }

        font_color <- data[[text_color_ref]][index]

      } else {

        stop("Attempted to select non-existing column or non-character column with text_color_ref")
      }

    } else {

       font_color <- text_color
    }

    ### conditional fill color
    if (is.character(fill_color_ref)) {

      if (all(fill_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(fill_color_ref)) { fill_color_ref <- which(names(data) %in% fill_color_ref) }

        fill <- data[[fill_color_ref]][index]
        fill <- suppressWarnings(grDevices::adjustcolor(fill, alpha.f = opacity))

      } else {

        stop("Attempted to select non-existing column or non-character column with fill_color_ref")
      }

    } else {

      fill <- color_pal(normalized)
      fill <- suppressWarnings(grDevices::adjustcolor(fill, alpha.f = opacity))

    }

    ### empty fill for NA values
    if (is.na(value)) {

      fill <- NULL

    } else {

      fill <- fill
    }

    ### calculate rotation
    degrees <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {

      abs(value) / max(abs(data[[name]]), na.rm = TRUE)

      ### min_value provided
    } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {

      (abs(value) - min_value) / (max(abs(data[[name]]), na.rm = TRUE) - min_value)

      ### max_value provided
    } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {

      (abs(value) / max_value)

      ### min and max provided
    } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {

      (abs(value) - min_value) / (max_value - min_value)

    } else if (!is.numeric(value)) {

      return(value)
    }

  size <- size/2

  gauge_fill <- htmltools::div(style = list(
    position = "absolute",
    top = "100%",
    left = "0",
    width = "inherit",
    height= paste0(size*50,"px"),
    background = fill,
    transformOrigin = "center top",
    transform = paste0("rotate(",degrees/2,"turn)"),
    transition = animation
  ))

  # tooltip label and options
  tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)

  if (tooltip == TRUE) {

    label <- tippy::tippy(label,
                   animateFill = FALSE,
                   followCursor = TRUE,
                   tooltip = tooltip_label)
  } else {

    label <- label
  }

  gauge_cover <- htmltools::div(style = list(
    width = paste0(size*75,"px"),
    height = paste0(size*75,"px"),
    background = "#ffffff",
    borderRadius = "50%",
    position = "absolute",
    top = "20%",
    left = "50%",
    transform = "translateX(-50%)",
    display = "flex",
    alignItems = "center",
    justifyContent = "center",
    paddingBottom = "25%",
    boxSizing = "border-box",
    fontSize = text_size,
    color = font_color,
    fontWeight = bold_text
  ), label)

  gauge_body <- htmltools::div(style = list(
    width = paste0(size*100,"px"),
    height = "0",
    paddingBottom = "50%",
    background = background,
    position = "relative",
    borderTopLeftRadius = "100% 200%",
    borderTopRightRadius = "100% 200%",
    overflow = "hidden"
  ), gauge_fill, gauge_cover)

  gauge <- htmltools::div(style = list(
    width = paste0(size*100,"px")
  ), gauge_body)

  determine_sizing <- function(min,size) {
    # each number is about 10px, the maxWidth is 100px. therefore, min + max = 100px
    min_size <- nchar(min)*10
    max_size = 100 - min_size

    # adjust slightly based on size of gauge
    if (size == 0.5) {

      max_size = max_size*size*1.05

    } else { max_size = max_size*size*1.25 }

    return(max_size)
  }

  max_size <- determine_sizing(min_label,size)

  if (show_min_max == TRUE) {

    sc_min <- htmltools::span(style = list(
                              float= "left",
                              fontSize = "70%",
                              marginLeft = "-2px",
                              color = min_text_color
                              ),
                              min_label)

    sc_max <- htmltools::span(style = list(
                              fontSize = "70%",
                              maxWidth = paste0(max_size,"px"),
                              flexDirection = "row-reverse",
                              textAlign = "right",
                              display= "flex",
                              justifyContent= "flex-start",
                              color = max_text_color
                              ),
                              max_label)

    htmltools::tagList(
      gauge,
      sc_min,
      sc_max
    )

  } else gauge

  }
}
