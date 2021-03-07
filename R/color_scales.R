#' Add color scales to rows in a column
#'
#' The `color_scales()` function conditionally colors each cell of a column depending on their value in relation to other values in that particular column.
#'     It should be placed within the style argument in reactable::colDef.
#'
#' @param data Dataset containing at least one numeric column.
#'
#' @param colors A vector of colors to color the cells.
#'     Colors should be given in order from low values to high values.
#'     Default colors provided are red-white-blue: c("#ff3030", "#ffffff", "#1e90ff").
#'     Can use R's built-in colors or other color packages.
#'
#' @param bright_values Optionally display values as white.
#'     Values with a dark-colored background will be shown in white.
#'     Default is set to TRUE but can be turned off by setting to FALSE.
#'
#' @param span Optionally apply colors to values in relation to the entire dataset instead of by column.
#'     Default is set to NULL but can be turned on by setting to TRUE.
#'
#' @return a function that applies conditional colors
#'     to a column of numeric values.
#'
#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## By default, the colors_scales() function uses a red-white-blue three-color pattern
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(style = color_scales(data))))
#'
#' ## If only two colors are desired,
#' ## you can specify them with colors = 'c(color1, color2)';
#' reactable(data,
#'  columns = list(
#'  Petal.Length = colDef(style = color_scales(data,
#'  colors = c("red", "green")))))
#'
#' ## Apply color_scales() across all numeric columns using reactable::defaultColDef
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data)))
#'
#' ## Use span to apply colors to values in relation to the entire dataset
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data, span = TRUE)))
#'
#' ## Span can take column names
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data, span = c("Sepal.Length", "Sepal.Width"))))
#'
#' ## Or it can also take column positions instead
#' reactable(data,
#' defaultColDef = colDef(style = color_scales(data, span = 1:2)))
#'
#' @export

color_scales <- function(data, colors = c("#ff3030", "#ffffff", "#1e90ff"), bright_values = TRUE, span = FALSE) {
  
  color_pal <- function(x) {
    
    if (!is.na(x))
      rgb(colorRamp(c(colors))(x), maxColorValue = 255)
    else
      NULL
  }
  
  assign_color <- function(x) {
    
    if (!is.na(x)) {
      rgb_sum <- rowSums(colorRamp(c(colors))(x))
      color <- ifelse(rgb_sum >= 375, "black", "white")
      color
    } else
      NULL
  }
  
  style <- function(value, index, name) {
    
    if (!is.numeric(value)) return(value)
    
    if (is.logical(span)) {
      
      if (span) {
        
        normalized <- (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))
        
      } else {
        
        normalized <- (value - min(data[[name]], na.rm = TRUE))/(max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))
        
      }
      
      cell_color <- color_pal(normalized)
      font_color <- assign_color(normalized)
      
    } else if (is.numeric(span) | is.character(span)) {
      
      if (all(span %in% which(sapply(data, is.numeric))) | all(span %in% names(which(sapply(data, is.numeric))))) {
        
        if (is.character(span)) { span <- which(names(data) %in% span) }
        
        normalized <- (value - min(dplyr::select(data, !!span), na.rm = TRUE)) / (max(dplyr::select(data, !!span), na.rm = TRUE) - min(dplyr::select(data, !!span), na.rm = TRUE))
        cell_color <- if (name %in% colnames(data)[span]) { color_pal(normalized) }
        font_color <- if (name %in% colnames(data)[span]) { assign_color(normalized) }
        
      } else {
        
        stop("Attempted to select non-existing or non-numeric columns with span")
        
      }
      
    }
    
    
    if (bright_values == FALSE) {
      
      list(background = cell_color)
      
    } else {
      
      list(background = cell_color, color = font_color)
      
    }
    
  }
}
