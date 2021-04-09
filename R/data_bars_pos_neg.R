#' Add horizontal bars to rows in a column containing positive and negative values
#'
#' The `data_bars_pos_neg()` function is depreciated.
#'     The new version of `data_bars()` can handle both positive and negative values now.
#'     Please use `data_bars()` instead.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A minimum of two colors or a vector of colors.
#'     Colors should be given in order from negative values to positive values.
#'     Can use R's built-in colors or other color packages.
#'
#' @param number_fmt Optionally format numbers using formats from the scales package.
#'     Default is set to NULL.
#'
#' @return a function that applies positive and negative data bars
#'     to a column of numeric values.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- data.frame(
#' company = sprintf("Company%02d", 1:10),
#' profit_chg = c(0.2, 0.685, 0.917, 0.284, 0.105, -0.701, -0.528, -0.808, -0.957, -0.11))
#'
#' ## By default, the negative values are assigned a red bar,
#' ## and the positive values are assigned a green bar
#' reactable(data,
#' bordered = TRUE,
#' columns = list(
#'  company = colDef(name = "Company",
#'  minWidth = 100),
#'  profit_chg = colDef(
#'    name = "Change in Profit",
#'    defaultSortOrder = "desc",
#'    align = "center",
#'    minWidth = 400,
#'    cell = data_bars(data))))
#'
#' ## You can apply a relative color scale to the bars by assigning three or more colors
#' reactable(data,
#' bordered = TRUE,
#' columns = list(
#'   company = colDef(name = "Company",
#'   minWidth = 100),
#'   profit_chg = colDef(
#'   name = "Change in Profit",
#'   defaultSortOrder = "desc",
#'   align = "center",
#'   minWidth = 400,
#'   cell = data_bars(data,
#'   fill_color = c("#ff3030", "#ffffff", "#1e90ff")))))
#'
#' @export


data_bars_pos_neg <- function(data, colors = c("red","green"), number_fmt = NULL) {

  .Deprecated("data_bars()")

  cell <- function(value, index, name) {

    if (!is.numeric(value)) return(value)

    if (is.null(number_fmt)) {

      label <- value

    } else label <- number_fmt(value)

    if (length(colors) > 2) {

      color_pal <- function(x) {

        if (!is.na(x)) rgb(colorRamp(c(colors))(x), maxColorValue = 255) else NULL
      }

      normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))
      fill_color <- color_pal(normalized)

      neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
      pos_chart <- htmltools::div(style = list(flex = "1 1 0"))

      if (grepl("percent", deparse(substitute(number_fmt))) == TRUE) {

        width <- paste0(abs(value / 1) * 100, "%")

      } else width <- paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "%")

      if (value < 0) {

        bar <- htmltools::div(style = list(marginLeft = "7px", background = fill_color, width = width, height = "16px", transition = "width 1s"))
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

      } else {

        bar <- htmltools::div(style = list(marginRight = "7px", background = fill_color, width = width, height = "16px", transition = "width 1s"))
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center"), bar, label)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)}

      htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)

    } else {

      neg_color <- colors[[1]]
      pos_color <- colors[[2]]

      neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
      pos_chart <- htmltools::div(style = list(flex = "1 1 0"))

      if (grepl("percent", deparse(substitute(number_fmt))) == TRUE) {

        width <- paste0(abs(value / 1) * 100, "%")

      } else width <- paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "%")

      if (value < 0) {

        bar <- htmltools::div(style = list(marginLeft = "7px", background = neg_color, width = width, height = "16px", transition = "width 1s"))
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)

      } else {

        bar <- htmltools::div(style = list(marginRight = "7px", background = pos_color, width = width, height = "16px", transition = "width 1s"))
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center"), bar, label)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)}

      htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)

    }
  }
}
