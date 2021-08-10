#' Embed image from web to cells in a column
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
#' @param horizontal_align The horizontal alignment of the image within a cell.
#'      Options are "left", "right", or "center".
#'      Default is "center".
#'
#' @param label Optionally assign a label to the image from another column.
#'     Default is set to NULL or no label.
#'
#' @param label_position Position of label relative to image.
#'     Options are "right", "left", "below", or "above".
#'     Default is right.
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
#'  img = case_when(
#'  Species == "setosa" ~
#'  "https://upload.wikimedia.org/wikipedia/commons/d/d9/Wild_iris_flower_iris_setosa.jpg",
#'  Species == "versicolor" ~
#'  "https://upload.wikimedia.org/wikipedia/commons/7/7a/Iris_versicolor.jpg",
#'  Species == "virginica" ~
#'  "https://upload.wikimedia.org/wikipedia/commons/9/9f/Iris_virginica.jpg",
#'  TRUE ~ "NA"))
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
#'  img = colDef(cell = embed_img(height = 50, width = 45))))
#'
#' ## Optionally assign a label to the image from another column
#' reactable(data,
#' columns = list(
#'  img = colDef(cell = embed_img(data, label = "Species"))))
#'
#' @export


embed_img <- function(data,
                      height = 24,
                      width = 24,
                      horizontal_align = "center",
                      label = NULL,
                      label_position = "right") {

  '%notin%' <- Negate('%in%')

  if (label_position %notin% c("left", "right", "above", "below") == TRUE) {

    stop("label_position must be either 'left', 'right', 'above', 'below'")
  }

  if (!is.null(horizontal_align) && horizontal_align %notin% c("left", "right", "center") == TRUE) {

    stop("horizontal_align must be either 'left', 'right', or 'center'")
  }

  # assign horizontal align
  if (horizontal_align == "left") {

    horizontal_align_css <- "flex-start"

  } else if (horizontal_align == "right") {

    horizontal_align_css <- "flex-end"

  } else horizontal_align_css <- "center"

  image <- function(value, index, name) {

    if (!is.character(value)) return(value)

    if (is.null(value) || is.na(value) || value == "NA" || value == "na" || stringr::str_detect(value, " ")) return("")

    if (grepl("https|http", value) == FALSE) {

      stop("must provide valid link to image.")
    }

    image <- htmltools::img(src = value, align = "center", height = height, width = width)

    if (!is.null(label) & label_position == "right") {

      col_label <- sprintf("     %s", data[[index, label]])

      htmltools::tagList(htmltools::div(style = list(display = "flex", justifyContent = horizontal_align_css),
                                        image, col_label))

    } else if (!is.null(label) & label_position == "left") {

      col_label <- sprintf("%s     ", data[[index, label]])

      htmltools::tagList(htmltools::div(style = list(display = "flex", justifyContent = horizontal_align_css),
                                        col_label, image))

    } else if (!is.null(label) & label_position == "below") {

      col_label <- sprintf("%s", data[[index, label]])

      htmltools::tagList(
        htmltools::div(style = list(display = "flex", justifyContent = horizontal_align_css),
                      image),
        htmltools::div(style = list(textAlign = "center"),
                      col_label))

    } else if (!is.null(label) & label_position == "above") {

      col_label <- sprintf("%s", data[[index, label]])

      htmltools::tagList(
        htmltools::div(style = list(textAlign = "center"),
                       col_label),
        htmltools::div(style = list(display = "flex", justifyContent = horizontal_align_css),
                       image))

    } else htmltools::tagList(htmltools::div(style = list(display = "flex", justifyContent = horizontal_align_css),
                              image))
  }
}
