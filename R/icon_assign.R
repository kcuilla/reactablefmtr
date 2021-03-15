#' Assign icons to rows in a column
#'
#' The `icon_assign()` function assigns icons from the Font Awesome library (via shiny) to each row of a numeric column depending on the value in each row.
#'     By default, the number of icons assigned will be equal to the value in that row. If the value is less than the max, it will receive empty icons.
#'     Both the icon shape and color of the filled and empty icons can be modified through the parameters.
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
#' @param buckets Optionally divide values in a column into buckets by providing a numeric value.
#'     Icons are then assigned by rank from lowest to highest.
#'     Default is set to NULL.
#'
#' @param seq_by A numerical input that determines what number each icon represents.
#'     Ex. instead of displaying 100 icons for the number 100, can set seq_by = 10 to show only 10 icons.
#'     Default value is set to 1.
#'
#' @param show_values Optionally display values next to icons.
#'     Icons can be displayed to the left of the icons with "left" or to the right with "right".
#'     Default is set to FALSE or no values.
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
#' @export


icon_assign <- function(data, icon = "circle", fill_color = "#1e90ff", empty_color = "lightgrey", buckets = NULL, seq_by = 1, show_values = FALSE) {

  icons <- function(empty = FALSE) {

    htmltools::tagAppendAttributes(shiny::icon(icon),
                        style = paste("color:", if (empty) empty_color else fill_color),
                        "aria-hidden" = "true"
    )
  }

  cell <- function(value, index, name) {

    if (!is.numeric(value) | is.na(value)) return(value)

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

      icon_seq <- lapply(seq(1, max_value, by = seq_by), function(i) {

        if (i <= value_rounded) icons() else icons(empty = TRUE)
      })

      label <- sprintf("%s out of %s", value, max_value)

    }

    if (show_values == "right") {

    htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = "left", paste0("  ", value))

    } else if (show_values == "left") {

    htmltools::div(paste0(value, "  "), title = label, "aria-label" = label, role = "img", icon_seq, align = "left")

    } else htmltools::div(title = label, "aria-label" = label, role = "img", icon_seq, align = "left")

  }
}

