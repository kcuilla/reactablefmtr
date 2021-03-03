#' Embed image from web to rows in a column
#'
#' The `embed_img()` function adds images obtained from the web to a column within reactable.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param data Dataset containing URL's to images
#'
#' @param height A value given for the height of the image in px.
#'     Default height is 24px.
#'
#' @param width A value given for the width of the image in px.
#'     Default width is 24px.
#'
#' @param label Optionally assign a label to the image from another column.
#'     Default is set to NULL or no label.
#'
#' @import reactable
#'
#' @return a function that renders an image
#'     to a column containing a valid web link.
#'
#' @examples
#' ## If no image links are in the original dataset, you need to assign them like so:
#' library(dplyr)
#' data <- iris %>%
#'  mutate(
#'    img = case_when(
#'      Species == "setosa" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/800px-Kosaciec_szczecinkowaty_Iris_setosa.jpg",
#'      Species == "versicolor" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/1920px-Iris_versicolor_3.jpg",
#'      Species == "virginica" ~ "https://www.fs.fed.us/wildflowers/beauty/iris/Blue_Flag/images/iris_virginica/iris_virginica_virginica.jpg",
#'      TRUE ~ "NA"))
#'
#' ## Then use embed_img() to display images
#' reactable(data,
#' columns = list(
#'  img = colDef(cell = embed_img())))
#'
#' ## By default, images are given a size of 24px by 24px,
#' ## but you can adjust the size using height and width:
#' reactable(data,
#' columns = list(
#'  img = colDef(cell = embed_img(height = "50", width = "45"))))
#'
#' ## Optionally assign a label to the image from another column
#' reactable(data,
#' columns = list(
#'  img = colDef(cell = embed_img(data, label = "Species"))))
#'
#' @export


embed_img <- function(data, height = "24", width = "24", label = NULL) {

  image <- function(value, index, name) {

    if (!is.character(value)) return(value)

    if (grepl("https|http", value) == FALSE) {

      stop("must provide valid link to image")
    }

    image <- htmltools::img(src = value, align = "center", height = height, width = width)

    if (!is.null(label)) {

      col_label <- sprintf("    %s", data[[index, label]])

      htmltools::tagList(image, col_label)

    } else htmltools::tagList(image)
  }
}
