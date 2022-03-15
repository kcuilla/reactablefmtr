#' Assign icons to cells in a column
#'
#' The `icon_assign()` function assigns icons from the Font Awesome library (via shiny) to each cell of a numeric column depending on the value in each row.
#'     By default, the number of icons assigned will be equal to the value in that cell. If the value is less than the max, it will receive empty icons.
#'     Both the icon shape, size, and color of the filled and empty icons can be modified through the parameters.
#'     Values can optionally be shown with the icons if desired.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param icon A single icon from the Font Awesome library (via shiny).
#'     Default icon is a circle.
#'
#' @param fill_color A single color for the filled icons.
#'     Default color is #1e90ff.
#'
#' @param empty_color A single color for the empty icons.
#'     Default color is lightgrey.
#'
#' @param fill_opacity A value between 0 and 1 that adjusts the opacity in fill_color.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param empty_opacity A value between 0 and 1 that adjusts the opacity in empty_color.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param align_icons Choose how to align the icons in a column.
#'     Options are left, right, or center.
#'     Default is left.
#'
#' @param icon_size A value representing the size of the icon in px.
#'     Default is 16.
#'
#' @param buckets Optionally divide values in a column into buckets by providing a numeric value.
#'     Icons are then assigned by rank from lowest to highest.
#'     Default is set to NULL.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @param seq_by A numerical input that determines what number each icon represents.
#'     Ex. instead of displaying 100 icons for the number 100, can set seq_by = 10 to show only 10 icons.
#'     Default value is set to 1.
#'
#' @param show_values Optionally display values next to icons.
#'     Options are "left", "right", above", "below", or "none".
#'     Default is none.
#'
#' @param animation Control the duration and timing function of the animation
#'     when sorting/updating values shown on a page.
#'     See [CSS transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)
#'     for available timing functions and examples.
#'     Animation can be turned off by setting to "none".
#'     Default is "1s ease".
#'
#' @import reactable
#'
#' @return a function that applies colored icons
#'     to a column of numeric values.
#'
#' @examples
#' data <- iris[10:29, ]
#' ## By default, icon_assign() assigns a cirlce icon for each value up to the maximum value.
#' ## If a value is 5 and the maximum value in the column is 6,
#' ## It will assign 5 blue icons and 1 grey icon.
#' reactable(data,
#' columns = list(
#' Sepal.Length = colDef(cell = icon_assign(data))))
#'
#' ## Assign colors to filled icons and empty icons
#' reactable(data,
#' columns = list(
#' Sepal.Length = colDef(cell = icon_assign(data,
#' fill_color = "red",
#' empty_color = "white"))))
#'
#' ## Assign any icon from the Font Awesome Library
#' reactable(data,
#' columns = list(
#' Sepal.Length = colDef(cell = icon_assign(data,
#' icon = "fan"))))
#'
#' ## Optionally divide values into buckets and assign icons based on rank.
#' reactable(data,
#' columns = list(
#' Sepal.Length = colDef(cell = icon_assign(data,
#' buckets = 3))))
#'
#' ## Optionally display values next to icons.
#' reactable(data,
#' columns = list(
#' Sepal.Length = colDef(cell = icon_assign(data,
#' show_values = "right"))))
#'
#' ## Change the alignment of the icons within a column.
#' reactable(data,
#' columns = list(
#' Sepal.Length = colDef(cell = icon_assign(data,
#' align_icons = "center"))))
#'
#' @export

icon_assign <- function(data,
                        icon = "circle",
                        fill_color = "#67a9cf",
                        empty_color = "lightgrey",
                        fill_opacity = 1,
                        empty_opacity = 1,
                        align_icons = "left",
                        icon_size = 16,
                        buckets = NULL,
                        number_fmt = NULL,
                        seq_by = 1,
                        show_values = "none",
                        animation = "1s ease") {


  '%notin%' <- Negate('%in%')

  if (show_values %notin% c("left", "right", "above", "below", "none") == TRUE) {

    stop("show_values must be either 'left', 'right', 'above', 'below', or 'none'")
  }

  if (align_icons %notin% c("left", "right", "center") == TRUE) {

    stop("align_icons must be either 'left', 'right', or 'center'")
  }

  if (!is.numeric(fill_opacity)) {

    stop("`fill_opacity` must be numeric")
  }

  if (fill_opacity < 0 | fill_opacity > 1) {

    stop("`fill_opacity` must be a value between 0 and 1")
  }

  if (!is.numeric(empty_opacity)) {

    stop("`empty_opacity` must be numeric")
  }

  if (empty_opacity < 0 | empty_opacity > 1) {

    stop("`empty_opacity` must be a value between 0 and 1")
  }

  fill_color <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)
  empty_color <- grDevices::adjustcolor(empty_color, alpha.f = empty_opacity)

  icons <- function(empty = FALSE) {

    htmltools::tagAppendAttributes(shiny::icon(icon),
                                   style = paste0("font-size:", icon_size, "px", "; color:", if (empty) empty_color else fill_color, sprintf("; transition: %s", animation)),
                                   "aria-hidden" = "true"
    )
  }

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(value) || is.na(value) || value == "NA" || value == "na" || stringr::str_detect(value, " ")) return("")

    if (!is.null(buckets) & !is.numeric(buckets)) {

      stop("must provide a number for buckets")
    }

    if (!is.null(buckets) & is.numeric(buckets)) {

      bucketed_data <- dplyr::ntile(data[[name]], n = buckets)

      bucket_value <- bucketed_data[index]

      value_rounded <- floor(bucket_value + 0.5)

      icon_seq <- lapply(seq_len(buckets), function(i) {

        if (i <= value_rounded) icons() else icons(empty = TRUE)
      })

      label <- sprintf("%s out of %s", bucket_value, buckets)

    } else {

      max_value <- max(floor(data[[name]] + 0.5), na.rm = TRUE)

      value_rounded <- floor(value + 0.5)

      if (max_value != 0) {

        icon_seq <- lapply(seq(1, max_value, by = seq_by), function(i) {

          if (i <= value_rounded) icons() else icons(empty = TRUE)
        })

      } else {

        icon_seq <- lapply(seq(0, max_value, by = seq_by), function(i) {

          if (i < value_rounded) icons() else icons(empty = TRUE)
        })
      }

      label <- sprintf("%s out of %s", value, max_value)

    }

    if (show_values == "right" & is.null(number_fmt)) {

      htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons, paste0("  ", value))

    } else if (show_values == "right" & !is.null(number_fmt)) {

      label <- number_fmt(value)

      htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons, paste0("  ", label))

    } else if (show_values == "left" & is.null(number_fmt)) {

      max_digits <- max(nchar(data[[name]]))+1

      label <- stringr::str_pad(value, max_digits)

      htmltools::div(paste0(label, "  "), title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons)

    } else if (show_values == "above" & is.null(number_fmt)) {

      max_digits <- max(nchar(data[[name]]))+1

      label <- stringr::str_pad(value, max_digits)

      htmltools::tagList(
        htmltools::div(label, align = align_icons),
        htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons)
      )

    } else if (show_values == "above" & !is.null(number_fmt)) {

      label <- number_fmt(value)

      max_digits <- max(nchar(data[[name]]))+1

      label <- stringr::str_pad(value, max_digits)

      htmltools::tagList(
        htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons),
        htmltools::div(label, align = align_icons)
      )

    } else if (show_values == "below" & is.null(number_fmt)) {

      max_digits <- max(nchar(data[[name]]))+1

      label <- stringr::str_pad(value, max_digits)

      htmltools::tagList(
        htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons),
        htmltools::div(label, align = align_icons)
      )

    } else if (show_values == "below" & !is.null(number_fmt)) {

      label <- number_fmt(value)

      max_digits <- max(nchar(data[[name]]))+1

      label <- stringr::str_pad(value, max_digits)

      htmltools::tagList(
        htmltools::div(label, align = align_icons),
        htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons)
      )

    } else if (show_values == "left" & !is.null(number_fmt)) {

      label <- number_fmt(value)

      max_digits <- max(nchar(data[[name]]))+1

      label <- stringr::str_pad(label, max_digits)

      htmltools::div(paste0(label, "  "), title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons)

    } else htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = align_icons)

  }
}
