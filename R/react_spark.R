#' Color of points used in `react_sparkline`.
#'
#' @param all,first,last,min,max The colors of all, first, last, min, and max points.
#'
#' @return a function that provides colors for specific points.
#'
#' @export

highlight_points <- function(all = "transparent",
                             first = "transparent",
                             last = "transparent",
                             min = "transparent",
                             max = "transparent") {

  col <- c(all, first, last, min, max)
  col
}


#' Add a sparkline line chart a reactable table
#'
#' The `react_sparkline()` function utilizes the {dataui} package <https://github.com/timelyportfolio/dataui> to create an interactive sparkline line chart.
#'     The data provided must be in a list format.
#'     The vertical height of the sparkline can be adjusted with `height`. By default, the height is matched to the height of a cell in a reactable table. However, when min/max/all labels are applied, the height is auto-increased to better show the labels. Further adjustment of the height may be needed to better see the patterns in the data.
#'     The four-sided margin around the sparkline can be controlled with `margin()`. When labels are added to the sparklines, the margin will auto-adjust (in most instances) to be able to display those labels.
#'     If the labels contain large values or values with many digits, the left and right margins may need to be increased slightly for the full numbers to be visible.
#'     By default, the sparkline line (the line that connects the data points) is shown but can be hidden by setting `show_line` to FALSE.
#'     The line color, line width, and line curve can be controlled with `line_color`, `line_width`, and `line_curve` respectively.
#'     The filled area beneath the line can be shown by setting `show_area` to TRUE. When the area is shown, the area color can be controlled with `area_color` or `area_color_ref` and opacity can be controlled with `area_opacity`.
#'     `statline` can be used to show a horizontal dotted line that represents either the mean, median, min, or max (your choice).
#'     The appearance of the statline and statline labels can be controlled with `statline_color` and `statline_label_size`.
#'     A bandline can be added by using `bandline`. The options are innerquartiles which highlights the innerquartiles of the data or range which highlights the full range of the data.
#'     By default, `react_sparkline()` is interactive and data points will be shown when hovering over the sparklines. This can be turned off by setting `tooltip` to FALSE.
#'     There are two tooltip types available within `tooltip_type`. The size and color of the tooltip labels can be adjusted with `tooltip_size` and `tooltip_color`.
#'     Also by default, there are no labels on the line itself. However, one could add labels to the first, last, min, max, or all values within `labels`.
#'     The labels that are shown on the sparkline and in the tooltip are automatically rounded to the nearest whole integer. But decimals can be shown by providing the number of decimal places in `decimals`.
#'     The minimum value of a data series is the minimum value shown for a sparkline, but this can be adjusted with `min_value` and the max can be adjusted with `max_value`.
#'     `react_sparkline()` should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing a column with numeric values in a list format.
#'
#' @param height Height of the sparkline.
#'     Default is 22.
#'
#' @param show_line Logical: show or hide the line.
#'     Default is TRUE.
#'
#' @param line_color The color of the line.
#'     Default is slategray.
#'
#' @param line_color_ref Optionally assign line colors from another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one color can be provided per row.
#'     Default is NULL.
#'
#' @param line_width Width of the line.
#'     Default is 1.
#'
#' @param line_curve The curvature of the line.
#'     Options are 'cardinal', 'linear', 'basis', or 'monotoneX'.
#'     Default is 'cardinal'.
#'
#' @param highlight_points Use `highlight_points()` to assign colors to particular points.
#'     Colors can be assigned to all, min, max, first, or last points.
#'     By default, transparent colors are assigned to each point.
#'
#' @param point_size Size of the points.
#'     Must first assigned colors to point(s) using `highlight_points`.
#'     Default is 1.1.
#'
#' @param labels Show labels for points of interest.
#'     Options are 'min', 'max', 'first', 'last', 'all', or 'none'.
#'     Default is 'none'.
#'
#' @param label_size Size of the labels.
#'     Default is '0.8em'.
#'
#' @param decimals The number of decimals displayed in the labels and tooltip.
#'     Default is 0.
#'
#' @param max_value The maximum value of the sparkline range.
#'     Default is NULL (automatically the maximum value of each sparkline series).
#'     Takes either:
#'
#'     1. a numeric vector of length 1
#'     2. a numeric vector of length equal to the number of rows
#'     3. a column name (as string) which holds the max_values to use
#'     4. a function which is applied to the maximum value of each row
#'
#' @param min_value The minimum value of the sparkline range.
#'     Default is NULL (automatically the minimum value of each sparkline series).
#'     Takes either:
#'
#'     1. a numeric vector of length 1
#'     2. a numeric vector of length equal to the number of rows
#'     3. a column name (as string) which holds the min_values to use
#'     4. a function which is applied to the minimum value of each row
#'
#' @param show_area Logical: show or hide area beneath line.
#'     Default is FALSE.
#'
#' @param area_color The color of the area.
#'     `show_area` must be set to TRUE for color to be shown.
#'     Default is NULL (inherited from line_color).
#'
#' @param area_color_ref Optionally assign area colors from another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one area color can be provided per row.
#'     Default is NULL.
#'     Default is FALSE.
#'
#' @param area_opacity A value between 0 and 1 that adjusts the opacity.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 0.1.
#'
#' @param statline Inserts a horizontal dotted line representing a statistic,
#'     and places the value of that statistic to the right of the line.
#'     Options are 'mean', 'median', 'min', or 'max'.
#'     Default is NULL.
#'
#' @param statline_color The color of the horizontal dotted statline.
#'     Default is red.
#'
#' @param statline_label_size The size of the label to the right of the statline.
#'     Default is '0.8em'.
#'
#' @param bandline Inserts a horizontal bandline to render ranges of interest.
#'     Options are 'innerquartiles' or 'range' (min to max).
#'     Default is NULL.
#'
#' @param bandline_color The color of the bandline.
#'     Default is red.
#'
#' @param bandline_opacity A value between 0 and 1 that adjusts the opacity.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 0.2.
#'
#' @param tooltip Logical: turn the tooltip on or off.
#'     Default is TRUE.
#'
#' @param tooltip_type The tooltip type.
#'     Options are 1 or 2.
#'     Default is 1.
#'
#' @param tooltip_color The color of the tooltip labels.
#'     Default is NULL.
#'
#' @param tooltip_size The size of the tooltip labels.
#'     Default is '1.1em'.
#'
#' @param margin The four-sided margin around the sparkline.
#'      Use margin() to assign the top, right, bottom, and left margins.
#'
#' @return a function that creates a sparkline chart
#'     from a column containing a list of values.
#'
#' @import reactable
#'
#' @examples
#' ## Default sparkline line chart
#' library(dplyr)
#' iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkline(.))))
#'
#' ## Highlight min and max data points
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(
#'  petal_width = colDef(cell = react_sparkline(.,
#'  decimals = 1,
#'  highlight_points = highlight_points(min="red",max="blue")))))
#'
#' ## Add labels to data points and change curvature of line
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkline(.,
#'  line_curve = "linear",
#'  decimals = 1,
#'  highlight_points = highlight_points(first="orange",last="blue"),
#'  labels = c("first","last")))))
#'
#' ## Conditionally assign line colors to groups
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  mutate(flower_cols = case_when(
#'    Species == "setosa" ~ "purple",
#'    Species == "versicolor" ~ "darkgreen",
#'    Species == "virginica" ~ "orange",
#'    TRUE ~ "grey"
#'  )) %>%
#'  reactable(.,
#'  columns = list(flower_cols = colDef(show=FALSE),
#'  petal_width = colDef(cell = react_sparkline(.,
#'  height = 80,
#'  line_color_ref = "flower_cols"))))
#'
#' ## Show area beneath the line
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkline(.,
#'  height = 80,
#'  line_color = "dodgerblue",
#'  show_area = TRUE))))
#'
#' ## Conditionally assign colors to the area below the line
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  mutate(flower_cols = case_when(
#'    Species == "setosa" ~ "purple",
#'    Species == "versicolor" ~ "darkgreen",
#'    Species == "virginica" ~ "orange",
#'    TRUE ~ "grey"
#'  )) %>%
#'  reactable(.,
#'  columns = list(flower_cols = colDef(show=FALSE),
#'  petal_width = colDef(cell = react_sparkline(.,
#'  height = 80,
#'  show_area = TRUE,
#'  line_color_ref = "flower_cols",
#'  area_color_ref = "flower_cols"))))
#'
#' ## Add bandline to show innerquartile range
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkline(.,
#'  height = 80,
#'  decimals = 1,
#'  highlight_points = highlight_points(max="red"),
#'  labels = c("max"),
#'  bandline = "innerquartiles",
#'  bandline_color = "darkgreen"))))
#'
#' ## Add statline to show the mean for each sparkline
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkline(.,
#'  height = 80,
#'  decimals = 1,
#'  statline = "mean",
#'  statline_color = "red"))))
#'
#' ## Combine multiple elements together
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkline(.,
#'  height = 80,
#'  decimals = 1,
#'  statline = "mean",
#'  statline_color = "red",
#'  bandline = "innerquartiles",
#'  bandline_color = "darkgreen"))))
#'
#' @export

react_sparkline <- function(data,
                            height = 22,
                            show_line = TRUE,
                            line_color = "slategray",
                            line_color_ref = NULL,
                            line_width = 1,
                            line_curve = "cardinal",
                            highlight_points = NULL,
                            point_size = 1.1,
                            labels = "none",
                            label_size = "0.8em",
                            decimals = 0,
                            min_value = NULL,
                            max_value = NULL,
                            show_area = FALSE,
                            area_color = NULL,
                            area_color_ref = NULL,
                            area_opacity = 0.1,
                            statline = NULL,
                            statline_color = "red",
                            statline_label_size = "0.8em",
                            bandline = NULL,
                            bandline_color = "red",
                            bandline_opacity = 0.2,
                            tooltip = TRUE,
                            tooltip_type = 1,
                            tooltip_color = NULL,
                            tooltip_size = "1.1em",
                            margin = NULL) {

  cell <- function(value, index, name) {

    if (!is.null(margin) && length(margin)<4) {

      stop("please provide margin dimensions within `margin()`. Ex. margin = margin(t=10)")
    }

    if (!is.null(highlight_points) && length(highlight_points)<5) {

      stop("please provide point color assignments within `highlight_points()`. Ex. highlight_points = highlight_points(max='red')")
    }

    if (is.null(highlight_points)) {

      highlight_points <- highlight_points(all = "transparent",
                                           first = "transparent",
                                           last = "transparent",
                                           min = "transparent",
                                           max = "transparent")

    } else {highlight_points <- highlight_points}

    if (!is.logical(show_line)) {

      stop("`show_line` must either be TRUE or FALSE.")
    }

    if (!is.logical(tooltip)) {

      stop("`tooltip` must either be TRUE or FALSE.")
    }

    if (!is.null(tooltip_type) && !any(tooltip_type %in% c(1,2))) {

      stop("`tooltip_type` must be either 1 or 2")
    }

    if (!is.logical(show_area)) {

      stop("`show_area` must either be TRUE or FALSE.")
    }

    if (!is.null(labels) && !any(labels %in% c("none","first","last","min","max","all"))) {

      stop("`labels` must be either first, last, min, max, all, or none")
    }

    if (!is.null(line_curve) && !any(line_curve %in% c("cardinal","linear","basis","monotoneX"))) {

      stop("`line_curve` must be either cardinal, linear, basis, or monotoneX")
    }

    if (!is.null(bandline) && !any(bandline %in% c("innerquartiles","range"))) {

      stop("`bandline` must be either innerquartiles or range")
    }

    if (!is.null(statline) && !any(statline %in% c("mean","median","min","max"))) {

      stop("`statline` must be either mean, median, min, or max")
    }

    ### find last index and min, max, and mean values
    last_index <- lapply(data[[name]], function(x) length(x)-1)
    value_max <- lapply(data[[name]], function(x) x[which.max(abs(x))])
    value_min <- lapply(data[[name]], function(x) x[which.min(abs(x))])
    value_mean <- lapply(data[[name]], mean)

    # Allow functions for max_ and min_value
    if(is.function(max_value)) {
      max_value <- lapply(value_max, max_value)
    }

    if(is.function(min_value)) {
      min_value <- lapply(value_min, min_value)
    }

    # Allow vectors of length equal to the number of rows (same as `length(value_max`)
    # for max_ and min_value
    if (length(max_value) > 1) {
      if (length(max_value) != length(value_max)) {
        stop(paste0("Error in `react_sparklines()`:\n",
                    "`max_value` must either be a numeric vector of length 1, ",
                    "a numeric vector of length equal to the number of rows, ",
                    "a function or a column name as string."))
      }
      max_value <- max_value[[index]]
    }

    if (length(min_value) > 1) {
      if (length(min_value) != length(value_min)) {
        stop(paste0("Error in `react_sparklines()`:\n",
                    "`min_value` must either be a numeric vector of length 1, ",
                    "a numeric vector of length equal to the number of rows, ",
                    "a function or a column name as string."))
      }
      min_value <- min_value[[index]]
    }

    # Allow strings to get max_ and min_values from other columns
    if (is.character(max_value)) {
      if (is.null(data[[max_value]][[index]])) {
        stop(paste0("Error in `react_sparklines()`: `max_value`.\nColumn `", max_value, "` doesn't exist."))
      }
      max_value <- data[[max_value]][[index]]
    }

    if (is.character(min_value)) {
      if (is.null(data[[min_value]][[index]])) {
        stop(paste0("Error in `react_sparklines()`: `min_value`.\nColumn `", min_value, "` doesn't exist."))
      }
      min_value <- data[[min_value]][[index]]
    }

    ### create a statline with a bold label to the right
    if (!is.null(statline) && statline %in% c("mean","median","min","max")) {

      statline <- dataui::dui_sparkhorizontalrefline(
        stroke = statline_color,
        strokeDasharray = "2, 2",
        strokeWidth = 1,
        strokeOpacity = 0.75,
        reference = statline,
        renderLabel = htmlwidgets::JS(htmltools::HTML(paste0(
          "(d) => React.createElement('tspan', {fill: '",statline_color,"', fontWeight: 'bold', fontSize: '",statline_label_size,"', stroke: 'transparent'}, d.toFixed(",decimals,"))"))),
        labelPosition = "right",
        labelOffset = 5)

        ### assign margins based on labeling
        if (any(labels %in% "none") && is.null(margin)) {

          margin <- margin(t=4,r=28,b=3,l=13)

        ### if labels are first/last but not min, max, or all
        } else if (any(labels %in% c("first","last")) && (!any(stringr::str_detect(labels, "min")) && !any(stringr::str_detect(labels, "max")) && !any(stringr::str_detect(labels, "all"))) && is.null(margin)) {

          margin <- margin(t=5,r=28,b=3,l=24)

        ### this applies to min, max, and/or all labels
        } else if (is.null(margin)) {

          margin <- margin(t=14,r=28,b=10,l=13)

          ### the default height needs to be increased to show all labels
          if (height == 22) {
            height <- 28
          } else { height <- height }
        }

    } else {

      statline <- dataui::dui_sparkhorizontalrefline(
        stroke = "transparent")

    }

    ### assign margins based on labeling
    if (any(labels %in% "none") && is.null(margin)) {

      # margin <- margin(t=2,r=5,b=2,l=5)
      margin <- margin(t=3,r=13,b=2,l=13)

    ### if labels are first/last but not min, max, or all
    } else if (any(labels %in% c("first","last")) && (!any(stringr::str_detect(labels, "min")) && !any(stringr::str_detect(labels, "max")) && !any(stringr::str_detect(labels, "all"))) && is.null(margin)) {

      margin <- margin(t=5,r=24,b=3,l=24)

    ### this applies to min, max, and/or all labels
    } else if (is.null(margin)) {

      margin <- margin(t=14,r=13,b=10,l=13)

      ### the default height needs to be increased to show all labels
      if (height == 22) {
        height <- 30
      } else { height <- height }
    }

    ### adjust label positioning based on labels
    ### if first/last labels then assign labels to left/right of points, otherwise show on top
    if (any(labels %in% c("first","last")) && (!any(stringr::str_detect(labels, "max")) && !any(stringr::str_detect(labels, "min")) && !any(stringr::str_detect(labels, "all")))) {

     label_position <- htmlwidgets::JS(paste0("{(d, i) => ((i === 0) ? 'left'
           : (i === ",last_index[index],") ? 'right'
           : 'top')}"))
     label_offset <- 6

    } else {

     # auto positioning doesn't seem to work within a condition:
     # label_position <- htmlwidgets::JS(paste0("{(d, i) => ((i === 0) ? 'left'
     #       : (i === ",last_index[index],") ? 'right'
     #       : 'auto')}"))

      label_position <- "auto"
      label_offset <- 7
    }

    if (!is.null(bandline) && bandline == "innerquartiles") {

      bandline_pattern <- dataui::dui_sparkpatternlines(
        id = "pattern",
        height = 4,
        width = 4,
        stroke = bandline_color,
        strokeWidth = 1,
        orientation= list("diagonal")
      )

      bandline <- dataui::dui_sparkbandline(
        band = "innerquartiles",
        fill = "url(#pattern)",
        fillOpacity = bandline_opacity
      )

    } else if (!is.null(bandline) && bandline == "range") {

      bandline_pattern <- dataui::dui_sparkpatternlines(
        id = "pattern",
        height = 4,
        width = 4,
        stroke = bandline_color,
        strokeWidth = 1,
        orientation= list("diagonal")
      )

      bandline <- dataui::dui_sparkbandline(
        band = list(from = list(y=min(value)), to = list(y=max(value))),
        fill = "url(#pattern)",
        fillOpacity = bandline_opacity
      )

    } else {

      bandline_pattern <- dataui::dui_sparkpatternlines(
        id = "NA",
        stroke = "transparent"
      )

      bandline <- dataui::dui_sparkbandline(
        fill = "transparent"
      )
    }

    ### conditional line color
    if (!is.null(line_color_ref) && is.character(line_color_ref)) {

      if (all(line_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(line_color_ref)) { line_color_ref <- which(names(data) %in% line_color_ref) }

        line_color <- data[[line_color_ref]][index]

      } else {

        stop("Attempted to select non-existing column or non-character column with line_color_ref")

      }
    }

    if (is.null(line_color_ref)) {

      line_color <- line_color
    }

    if (is.null(area_color)) {

      area_color <- line_color

    } else { area_color <- area_color }

    ### conditional area color
    if (!is.null(area_color_ref) && is.character(area_color_ref)) {

      if (all(area_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(area_color_ref)) { area_color_ref <- which(names(data) %in% area_color_ref) }

        area_color <- data[[area_color_ref]][index]

      } else {

        stop("Attempted to select non-existing column or non-character column with area_color_ref")

      }
    }

    if (is.null(area_color_ref)) {

      area_color <- area_color
    }

     ### tooltip options
     # tooltip_position <- htmlwidgets::JS(paste0("{(yVal, i) => ((i === 0 ) ? 'right'
     #       : (i === ",last_index[index],") ? 'left'
     #       : (yVal > ",value_mean[index],") ? 'bottom'
     #       : 'top')}"))

     tooltip_position <- htmlwidgets::JS(paste0("{(yVal, i) => ((yVal > ",value_mean[index],") ? 'bottom'
       : 'top')}"))

     tooltip_offset <- 5

     if (is.null(tooltip_color)) {

       tooltip_color <- line_color

     } else { tooltip_color <- tooltip_color }

     if (tooltip == TRUE) {

       if (tooltip_type == 1) {

       # tooltip == 1
       tooltip_1 <- dataui::dui_tooltip(components = list(
            dataui::dui_sparkpointseries(
              size = 0,
              renderLabel = htmlwidgets::JS(htmltools::HTML(paste0(
              "(d) => React.createElement('tspan', {fill: '",tooltip_color,"', fontSize: '",tooltip_size,"', fontWeight: 'bold', stroke: 'white'}, d.toFixed(",decimals,"))"))),
              labelPosition = tooltip_position,
              labelOffset = tooltip_offset
            )))

       tooltip_2 <- NULL

       } else {

       tooltip_1 <- dataui::dui_tooltip(components = list(
            dataui::dui_sparkpointseries(
              size = 0
            )))

       # tooltip == 2
       tooltip_2 <- htmlwidgets::JS(htmltools::HTML(paste0("
            function (_ref) {
              var datum = _ref.datum;
              return React.createElement(
                    'tspan',
                    {style: {fontSize: '",tooltip_size,"', color: '",tooltip_color,"', fontWeight: 'bold', stroke: 'transparent'}},
                    datum.y ? datum.y.toLocaleString(undefined, {maximumFractionDigits: ",decimals,"}) : \"--\"
                  )
            }
        ")))
       }

     } else {

       tooltip_1 <- dataui::dui_tooltip(components = list(
            dataui::dui_sparkpointseries(
              size = 0
            )))
       tooltip_2 <- NULL
     }


    dataui::dui_sparkline(
      data = value,
      height = height,
      max = max_value,
      min = min_value,
      margin = list(
        top = margin[[1]],
        right = margin[[2]],
        bottom = margin[[3]],
        left = margin[[4]]
      ),
      ### tooltip_type == 2
      renderTooltip = tooltip_2,
      components = list(
        dataui::dui_sparklineseries(
          curve = line_curve,
          showLine = show_line,
          stroke = line_color,
          strokeWidth = line_width,
          fill = area_color,
          fillOpacity = area_opacity,
          showArea = show_area
        ),
        ### assign all points to sparkline but only show those that have a color assigned to it
        dataui::dui_sparkpointseries(
          points = as.list("all"),
          stroke = highlight_points[[1]],
          fill = highlight_points[[1]],
          size = point_size
        ),
        dataui::dui_sparkpointseries(
          points = as.list("first"),
          stroke = highlight_points[[2]],
          fill = highlight_points[[2]],
          size = point_size
        ),
        dataui::dui_sparkpointseries(
          points = as.list("last"),
          stroke = highlight_points[[3]],
          fill = highlight_points[[3]],
          size = point_size
        ),
        dataui::dui_sparkpointseries(
          points = as.list("min"),
          stroke = highlight_points[[4]],
          fill = highlight_points[[4]],
          size = point_size
        ),
        dataui::dui_sparkpointseries(
          points = as.list("max"),
          stroke = highlight_points[[5]],
          fill = highlight_points[[5]],
          size = point_size
        ),
        dataui::dui_sparkpointseries(
          points = as.list(labels),
          fill = "transparent",
          stroke = "transparent",
          renderLabel = htmlwidgets::JS(htmltools::HTML(paste0(
            "(d) => React.createElement('tspan', {fill: '",line_color,"', fontSize: '",label_size,"', stroke: 'transparent'}, d.toFixed(",decimals,"))"))),
          labelPosition = label_position,
          labelOffset = label_offset
        ),
        statline,
        bandline_pattern,
        bandline,
        ### tooltip_type == 1
        tooltip_1
      )
    )
  }
}


#' Color of highlight used in `react_sparkbar`.
#'
#' @param first,last,min,max The colors of first, last, min, and max bars
#'
#' @return a function that provides colors for specific bars.
#'
#' @export

highlight_bars <- function(first = "transparent",
                           last = "transparent",
                           min = "transparent",
                           max = "transparent") {

  col <- c(first, last, min, max)
  col
}


#' Add a sparkline bar chart to a reactable table
#'
#' The `react_sparkbar()` function utilizes the {dataui} package <https://github.com/timelyportfolio/dataui> to create an interactive sparkline bar chart.
#'     The data provided must be in a list format.
#'     The vertical height of the sparkbar can be adjusted with `height`. By default, the height is matched to the height of a cell in a reactable table. However, the height can be increased to better see the patterns in the data.
#'     The four-sided margin around the sparkbar can be controlled with `margin()`. When labels are added to the sparkbars, the margin will auto-adjust (in most instances) to be able to display those labels.
#'     If the labels contain large values or values with many digits, the left and right margins may need to be increased slightly for the full numbers to be visible.
#'     The fill color and fill width can be controlled with `fill_color`, `fill_color_ref`, and `fill_opacity`.
#'     The outline color and width of the filled bars can be controlled with `outline_color`, `outline_color_ref`, and `outline_width`.
#'     `statline` can be used to show a horizontal dotted line that represents either the mean, median, min, or max (your choice).
#'     The appearance of the statline and statline labels can be controlled with `statline_color` and `statline_label_size`.
#'     A bandline can be added by using `bandline`. The options are innerquartiles which highlights the innerquartiles of the data or range which highlights the full range of the data.
#'     By default, `react_sparkbar()` is interactive and data points will be shown when hovering over the sparkbars. This can be turned off by setting `tooltip` to FALSE.
#'     There are two tooltip types available within `tooltip_type`. The size and color of the tooltip labels can be adjusted with `tooltip_size` and `tooltip_color`.
#'     Also by default, there are no labels on the bars themselves. However, one could add labels to the first, last, min, max, or all values within `labels`.
#'     The labels that are shown on the sparkbar and in the tooltip are automatically rounded to the nearest whole integer. But decimals can be shown by providing the number of decimal places in `decimals`.
#'     The minimum value of a data series is the minimum value shown for a sparkbar, but this can be adjusted with `min_value` and the max can be adjusted with `max_value`.
#'     `react_sparkline()` should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing a column with numeric values in a list format.
#'
#' @param height Height of the sparkbar.
#'     Default is 22.
#'
#' @param fill_color The color of the bar fill.
#'     Default is slategray.
#'
#' @param fill_color_ref Optionally assign fill colors from another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one color can be provided per row.
#'     Default is NULL.
#'
#' @param fill_opacity A value between 0 and 1 that adjusts the opacity.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 1.
#'
#' @param outline_color The color of the outline around the filled bars.
#'     Default is transparent.
#'
#' @param outline_color_ref Optionally assign outline colors from another column
#'     by providing the name of the column containing the colors in quotes.
#'     Only one color can be provided per row.
#'     Default is NULL.
#'
#' @param outline_width Width of the outline around the filled bars.
#'     Default is 1.
#'
#' @param highlight_bars Use `highlight_bars()` to assign colors to particular bars.
#'     Colors can be assigned to all, min, max, first, or last bars.
#'     By default, transparent colors are assigned to each bars.
#'
#' @param labels Show labels for points of interest.
#'     Options are 'min', 'max', 'first', 'last', 'all', or 'none'.
#'     Default is 'none'.
#'
#' @param label_size The size of the labels.
#'     Default is 0.8em.
#'
#' @param decimals Numeric: The number of decimals displayed in the labels and tooltip.
#'     Default is 0.
#'
#' @param max_value The maximum value of the sparkbar range.
#'     Default is NULL (automatically the maximum value of each sparkbar series).
#'     Takes either:
#'
#'     1. a numeric vector of length 1
#'     2. a numeric vector of length equal to the number of rows
#'     3. a column name (as string) which holds the max_values to use
#'     4. a function which is applied to the maximum value of each row
#'
#' @param min_value The minimum value of the sparkbar range.
#'     Default is NULL (automatically the minimum value of each sparkbar series).
#'     Takes either:
#'
#'     1. a numeric vector of length 1
#'     2. a numeric vector of length equal to the number of rows
#'     3. a column name (as string) which holds the min_values to use
#'     4. a function which is applied to the minimum value of each row
#'
#' @param statline Inserts a horizontal dotted line representing a statistic,
#'     and places the value of that statistic to the right of the line.
#'     Options are 'mean', 'median', 'min', or 'max'.
#'     Default is NULL.
#'
#' @param statline_color The color of the horizontal dotted statline.
#'     Default is red.
#'
#' @param statline_label_size The size of the label to the right of the statline.
#'     Default is 0.8em.
#'
#' @param bandline Inserts a horizontal bandline to render ranges of interest.
#'     Options are 'innerquartiles' or 'range' (min to max).
#'     Default is NULL.
#'
#' @param bandline_color The color of the bandline.
#'     Default is red.
#'
#' @param bandline_opacity A value between 0 and 1 that adjusts the opacity.
#'     A value of 0 is fully transparent, a value of 1 is fully opaque.
#'     Default is 0.2.
#'
#' @param tooltip Logical: turn the tooltip on or off.
#'     Default is TRUE.
#'
#' @param tooltip_type The tooltip type.
#'     Options are 1 or 2.
#'     Default is 1.
#'
#' @param tooltip_color The color of the tooltip labels.
#'     Default is NULL.
#'
#' @param tooltip_size The size of the tooltip labels.
#'     Default is '1.1em'.
#'
#' @param margin The four-sided margin around the sparkbar.
#'      Use margin() to assign the top, right, bottom, and left margins.
#'
#' @return a function that creates a sparkline bar chart
#'     from a column containing a list of values.
#'
#' @import reactable
#'
#' @examples
#' library(dplyr)
#' ## Default sparkline bar chart
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkbar(.))))
#'
#' ## Highlight particular bars
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkbar(.,
#'  decimals = 1,
#'  min_value = 0,
#'  highlight_bars = highlight_bars(min="red",max="blue")))))
#'
#' ## Conditionally assign fill colors to groups
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  mutate(flower_cols = case_when(
#'    Species == "setosa" ~ "purple",
#'    Species == "versicolor" ~ "darkgreen",
#'    Species == "virginica" ~ "orange",
#'    TRUE ~ "grey"
#'  )) %>%
#'  reactable(.,
#'  columns = list(flower_cols = colDef(show=FALSE),
#'  petal_width = colDef(cell = react_sparkbar(.,
#'  height = 80,
#'  fill_color_ref = "flower_cols"))))
#'
#' ## Add labels to particular bars
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkbar(.,
#'  height = 80,
#'  decimals = 1,
#'  highlight_bars = highlight_bars(first="blue",last="red"),
#'  labels = c("first","last")))))
#'
#' ## Add statline to show the mean for each sparkbar
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkbar(.,
#'  height = 80,
#'  decimals = 1,
#'  statline = "mean"))))
#'
#' ## Combine multiple elements together
#'iris %>%
#'  group_by(Species) %>%
#'  summarize(petal_width = list(Petal.Width)) %>%
#'  reactable(.,
#'  columns = list(petal_width = colDef(cell = react_sparkbar(.,
#'  height = 80,
#'  decimals = 1,
#'  statline = "mean",
#'  bandline = "innerquartiles"))))
#'
#' @export

react_sparkbar <- function(data,
                           height = 22,
                           fill_color = "slategray",
                           fill_color_ref = NULL,
                           fill_opacity = 1,
                           outline_color = "transparent",
                           outline_color_ref = NULL,
                           outline_width = 1,
                           highlight_bars = NULL,
                           labels = "none",
                           label_size = "0.8em",
                           decimals = 0,
                           max_value = NULL,
                           min_value = NULL,
                           statline = NULL,
                           statline_color = "red",
                           statline_label_size = "0.8em",
                           bandline = NULL,
                           bandline_color = "red",
                           bandline_opacity = 0.2,
                           tooltip = TRUE,
                           tooltip_type = 1,
                           tooltip_color = NULL,
                           tooltip_size = "1.1em",
                           margin = NULL) {

  cell <- function(value, index, name) {

    if (!is.null(margin) && length(margin)<4) {

      stop("please provide margin dimensions within `margin()`. Ex. margin = margin(t=10)")
    }

    if (!is.logical(tooltip)) {

      stop("`tooltip` must either be TRUE or FALSE.")
    }

    if (!is.null(tooltip_type) && !any(tooltip_type %in% c(1,2))) {

      stop("`tooltip_type` must be either 1 or 2")
    }

    if (!is.null(highlight_bars) && length(highlight_bars)<4) {

      stop("please provide highlight color assignments within `highlight_bars()`. Ex. highlight_bars = highlight_bars(max='red')")
    }

    if (!is.null(labels) && !any(labels %in% c("none","first","last","min","max","all"))) {

      stop("`labels` must be either first, last, min, max, all, or none")
    }

    if (!is.null(bandline) && !any(bandline %in% c("innerquartiles","range"))) {

      stop("`bandline` must be either innerquartiles or range")
    }

    if (!is.null(statline) && !any(statline %in% c("mean","median","min","max"))) {

      stop("`statline` must be either mean, median, min, or max")
    }

    ### find last index and min, max, and mean values
    last_index <- lapply(data[[name]], function(x) length(x)-1)
    value_max <- lapply(data[[name]], function(x) x[which.max(abs(x))])
    value_min <- lapply(data[[name]], function(x) x[which.min(abs(x))])
    value_mean <- lapply(data[[name]], mean)

    # Allow functions for max_ and min_value
    if(is.function(max_value)) {
      max_value <- lapply(value_max, max_value)
    }

    if(is.function(min_value)) {
      min_value <- lapply(value_min, min_value)
    }

    # Allow vectors of length equal to the number of rows (same as `length(value_max`)
    # for max_ and min_value
    if (length(max_value) > 1) {
      if (length(max_value) != length(value_max)) {
        stop(paste0("Error in `react_sparklines()`:\n",
                    "`max_value` must either be a numeric vector of length 1, ",
                    "a numeric vector of length equal to the number of rows, ",
                    "a function or a column name as string."))
      }
      max_value <- max_value[[index]]
    }

    if (length(min_value) > 1) {
      if (length(min_value) != length(value_min)) {
        stop(paste0("Error in `react_sparklines()`:\n",
                    "`min_value` must either be a numeric vector of length 1, ",
                    "a numeric vector of length equal to the number of rows, ",
                    "a function or a column name as string."))
      }
      min_value <- min_value[[index]]
    }

    # Allow strings to get max_ and min_values from other columns
    if (is.character(max_value)) {
      if (is.null(data[[max_value]][[index]])) {
        stop(paste0("Error in `react_sparklines()`: `max_value`.\nColumn `", max_value, "` doesn't exist."))
      }
      max_value <- data[[max_value]][[index]]
    }

    if (is.character(min_value)) {
      if (is.null(data[[min_value]][[index]])) {
        stop(paste0("Error in `react_sparklines()`: `min_value`.\nColumn `", min_value, "` doesn't exist."))
      }
      min_value <- data[[min_value]][[index]]
    }

    if (!is.null(statline) && statline %in% c("mean","median","min","max")) {

      statline <- dataui::dui_sparkhorizontalrefline(
        stroke = statline_color,
        strokeDasharray = "3,3",
        strokeWidth = 1.5,
        strokeOpacity = 0.75,
        reference = statline,
        renderLabel = htmlwidgets::JS(htmltools::HTML(paste0(
          "(d) => React.createElement('tspan', {fill: '",statline_color,"', fontWeight: 'bold', fontSize: '",statline_label_size,"', stroke: 'transparent'}, d.toFixed(",decimals,"))"))),
        labelPosition = "right",
        labelOffset = 10)

      if (any(labels %in% "none") && is.null(margin)) {

        margin <- margin(t=4,r=28,b=3,l=13)

      } else if (is.null(margin)) {

        margin <- margin(t=12,r=26,b=3,l=26)
      }

    } else {

      statline <- dataui::dui_sparkhorizontalrefline(
        stroke = "transparent")

    }

      if (!is.null(bandline) && bandline == "innerquartiles") {

      bandline_pattern <- dataui::dui_sparkpatternlines(
        id = "pattern",
        height = 4,
        width = 4,
        stroke = bandline_color,
        strokeWidth = 1,
        orientation= list("diagonal")
      )

      bandline <- dataui::dui_sparkbandline(
        band = "innerquartiles",
        fill = "url(#pattern)",
        fillOpacity = bandline_opacity
      )

    } else if (!is.null(bandline) && bandline == "range") {

      bandline_pattern <- dataui::dui_sparkpatternlines(
        id = "pattern",
        height = 4,
        width = 4,
        stroke = bandline_color,
        strokeWidth = 1,
        orientation= list("diagonal")
      )

      bandline <- dataui::dui_sparkbandline(
        band = list(from = list(y=min(value)), to = list(y=max(value))),
        fill = "url(#pattern)",
        fillOpacity = bandline_opacity
      )

    } else {

      bandline_pattern <- dataui::dui_sparkpatternlines(
        id = "NA",
        stroke = "transparent"
      )

      bandline <- dataui::dui_sparkbandline(
        fill = "transparent"
      )
    }

    if (any(labels %in% "none") && is.null(margin)) {

      margin <- margin(t=3,r=13,b=0,l=13)

    } else if (is.null(margin)) {

      margin <- margin(t=12,r=26,b=0,l=26)
    }

    ### conditional outline color
    if (!is.null(outline_color_ref) && is.character(outline_color_ref)) {

      if (all(outline_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(outline_color_ref)) { outline_color_ref <- which(names(data) %in% outline_color_ref) }

        outline_color <- data[[outline_color_ref]][index]

      } else {

        stop("Attempted to select non-existing column or non-character column with outline_color_ref")

      }
    }

    if (is.null(outline_color_ref)) {

      outline_color <- outline_color
    }

    ### conditional fill color
    if (!is.null(fill_color_ref) && is.character(fill_color_ref)) {

      if (all(fill_color_ref %in% names(which(sapply(data, is.character))))) {

        if (is.character(fill_color_ref)) { fill_color_ref <- which(names(data) %in% fill_color_ref) }

        fill_color <- data[[fill_color_ref]][index]

      } else {

        stop("Attempted to select non-existing column or non-character column with fill_color_ref")

      }

    } else if (is.null(fill_color_ref)) {

      fill_color <- fill_color
    }

    if (!is.null(highlight_bars)) {

      if (is.null(highlight_bars)) {

        highlight_bars <- highlight_bars(first = fill_color,
                                         last = fill_color,
                                         min = fill_color,
                                         max = fill_color)

      } else {highlight_bars <- highlight_bars}

      highlight_bars <- replace(highlight_bars, highlight_bars=="transparent", fill_color)

      ### logic for highlighting. if min/max is located in the first/last bars, they will superseed the first/last colors
      if ((highlight_bars[[1]] != fill_color ||
           highlight_bars[[2]] != fill_color) &&
          (highlight_bars[[3]] == fill_color &&
           highlight_bars[[4]] == fill_color)) {

        fill_condition <- htmlwidgets::JS(paste0("{(yVal, i) => ((i === 0) ? '",highlight_bars[[1]],
                                                 "' : (i === ",last_index[index],") ? '",highlight_bars[[2]],
                                                 "' : '",fill_color,"')}"))

      } else if ((highlight_bars[[1]] != fill_color ||
                  highlight_bars[[2]] != fill_color) &&
                 (highlight_bars[[3]] != fill_color &&
                  highlight_bars[[4]] == fill_color)) {

        fill_condition <- htmlwidgets::JS(paste0("{(yVal, i) => ((i === 0 && yVal != ",value_min[index],") ? '",highlight_bars[[1]],
                                                 "' : (i === ",last_index[index],") ? '",highlight_bars[[2]],
                                                 "' : (yVal === ",value_min[index],") ? '",highlight_bars[[3]],
                                                 "' : '",fill_color,"')}"))

      } else if ((highlight_bars[[1]] != fill_color ||
                  highlight_bars[[2]] != fill_color) &&
                 (highlight_bars[[3]] == fill_color &&
                  highlight_bars[[4]] != fill_color)) {

        fill_condition <- htmlwidgets::JS(paste0("{(yVal, i) => ((i === 0 && yVal != ",value_max[index],") ? '",highlight_bars[[1]],
                                                 "' : (i === ",last_index[index],") ? '",highlight_bars[[2]],
                                                 "' : (yVal === ",value_max[index],") ? '",highlight_bars[[4]],
                                                 "' : '",fill_color,"')}"))

      } else if ((highlight_bars[[1]] != fill_color ||
                  highlight_bars[[2]] != fill_color) &&
                 (highlight_bars[[3]] != fill_color &&
                  highlight_bars[[4]] != fill_color)) {

        fill_condition <- htmlwidgets::JS(paste0("{(yVal, i) => ((i === 0 && yVal != ",value_max[index],") ? '",highlight_bars[[1]],
                                                 "' : (i === ",last_index[index],") ? '",highlight_bars[[2]],
                                                 "' : (yVal === ",value_max[index],") ? '",highlight_bars[[4]],
                                                 "' : (yVal === ",value_min[index],") ? '",highlight_bars[[3]],
                                                 "' : '",fill_color,"')}"))


      } else if ((highlight_bars[[1]] == fill_color &&
                  highlight_bars[[2]] == fill_color) &&
                 (highlight_bars[[3]] != fill_color ||
                  highlight_bars[[4]] != fill_color)) {

        fill_condition <- htmlwidgets::JS(paste0("{(yVal, i) => ((yVal === ",value_max[index],") ? '",highlight_bars[[4]],
                                                 "' : (yVal === ",value_min[index],") ? '",highlight_bars[[3]],
                                                 "' : '",fill_color,"')}"))

      } else {

        fill_condition <- fill_color

      }

    } else { fill_condition <- fill_color }

     ### tooltip options
     tooltip_position <- htmlwidgets::JS(paste0("{(yVal, i) => ((yVal > ",value_mean[index],") ? 'bottom'
       : 'top')}"))

     tooltip_offset <- 6

     if (is.null(tooltip_color)) {

       tooltip_color <- fill_color

     } else { tooltip_color <- tooltip_color }

     if (tooltip == TRUE) {

       if (tooltip_type == 1) {

       # tooltip == 1
       tooltip_1 <- dataui::dui_tooltip(components = list(
            dataui::dui_sparkpointseries(
              size = 0,
              renderLabel = htmlwidgets::JS(htmltools::HTML(paste0(
              "(d) => React.createElement('tspan', {fill: '",tooltip_color,"', fontSize: '",tooltip_size,"', fontWeight: 'bold', stroke: 'white'}, d.toFixed(",decimals,"))"))),
              labelPosition = tooltip_position,
              labelOffset = tooltip_offset
            )))

       tooltip_2 <- NULL

       } else {

       tooltip_1 <- dataui::dui_tooltip(components = list(
            dataui::dui_sparkpointseries(
              size = 0
            )))

       # tooltip == 2
       tooltip_2 <- htmlwidgets::JS(htmltools::HTML(paste0("
            function (_ref) {
              var datum = _ref.datum;
              return React.createElement(
                    'tspan',
                    {style: {fontSize: '",tooltip_size,"', color: '",tooltip_color,"', fontWeight: 'bold', stroke: 'transparent'}},
                    datum.y ? datum.y.toLocaleString(undefined, {maximumFractionDigits: ",decimals,"}) : \"--\"
                  )
            }
        ")))
       }

     } else {

       tooltip_1 <- dataui::dui_tooltip(components = list(
            dataui::dui_sparkpointseries(
              size = 0
            )))
       tooltip_2 <- NULL
     }


    dataui::dui_sparkline(
      data = value,
      height = height,
      max = max_value,
      min = min_value,
      margin = list(
        top = margin[[1]],
        right = margin[[2]],
        bottom = margin[[3]],
        left = margin[[4]]
      ),
      ### tooltip == 2
      renderTooltip = tooltip_2,
      components = list(
        dataui::dui_sparkbarseries(
          stroke = outline_color,
          strokeWidth = outline_width,
          fill = fill_condition,
          fillOpacity = fill_opacity
        ),
        dataui::dui_sparkpointseries(
          points = as.list(labels),
          fill = "transparent",
          stroke = "transparent",
          renderLabel = htmlwidgets::JS(htmltools::HTML(paste0(
            "(d) => React.createElement('tspan', {fill: '",fill_color,"', fontSize: '",label_size,"', stroke: 'transparent'}, d.toFixed(",decimals,"))"))),
          labelPosition = "top",
          labelOffset = 6.5
        ),
        bandline_pattern,
        bandline,
        statline,
        ### tooltip_type == 1
        tooltip_1
      )
    )
  }
}
