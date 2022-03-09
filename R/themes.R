#' Theme default
#'
#' Reactable-inspired default theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #333333.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #333333.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard default theme
#' reactable(data,
#'           theme = default())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = default(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

default <- function(font_size = 15,
                    font_color = "#333333",
                    header_font_size = 15,
                    header_font_color = "#333333",
                    cell_padding = 6,
                    centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      color = header_font_color,
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(color = font_color,
                            fontSize = header_font_size)
  )
}


#' Theme cerulean
#'
#' Bootstrap-inspired cerulean theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #141415.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #cfe9f7.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard cerulean theme
#' reactable(data,
#'           theme = cerulean())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = cerulean(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

cerulean <- function(font_size = 14,
                     font_color = "#141415",
                     header_font_size = 15,
                     header_font_color = "#cfe9f7",
                     cell_padding = 6,
                     centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#e9ecef",
    borderWidth = "1px",
    stripedColor = "#e9ecef",
    highlightColor = "#e9ecef",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#3ba9e8",
      borderBottomColor = "#eaedf0",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#1782c0"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#141415",
      "&:focus" = list(color = "#141415")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#141415"),
    rowSelectedStyle = list(backgroundColor = "rgba(52, 156, 244, 0.5)"),
    selectStyle = list(
      backgroundColor = "#2fa4e7",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#e9ecef", color = "#2fa4e7")
    ),
    pageButtonStyle = list(
      backgroundColor = "#ffffff",
      color = "#2fa4e7",
      "&:hover" = list(backgroundColor = "#e9ecef", color = "#2fa4e7")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#e9ecef", color = "#2fa4e7"),
    pageButtonActiveStyle = list(backgroundColor = "#2fa4e7", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#2fa4e7", color = "#ffffff")
  )
}


#' Theme cosmo
#'
#' Bootstrap-inspired cosmo theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #141415.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #ffffff.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard cosmo theme
#' reactable(data,
#'           theme = cosmo())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = cosmo(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

cosmo <- function(font_size = 14,
                  font_color = "#141415",
                  header_font_size = 15,
                  header_font_color = "#ffffff",
                  cell_padding = 6,
                  centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#f8f9fa",
    borderColor = "#f8f9fa",
    borderWidth = "1px",
    stripedColor = "white",
    highlightColor = "white",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#373a3c",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#111111"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#9a9a9a",
      borderColor = "#b3cecc",
      "&:focus" = list(color = "#888888")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#9a9a9a"),
    rowSelectedStyle = list(backgroundColor = "#78c2ad", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#2780e3",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      backgroundColor = "#ffffff",
      color = "#2780e3",
      "&:hover" = list(backgroundColor = "#2780e3", color = "#ffffff")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#2780e3", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#2780e3", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#2780e3", color = "#ffffff")
  )
}


#' Theme cyborg
#'
#' Bootstrap-inspired cyborg theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #888888.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #7b7b7b.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard cyborg theme
#' reactable(data,
#'           theme = cyborg())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = cyborg(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

cyborg <- function(font_size = 14,
                   font_color = "#888888",
                   header_font_size = 15,
                   header_font_color = "#7b7b7b",
                   cell_padding = 6,
                   centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#060606",
    borderColor = "#888888",
    borderWidth = "1px",
    stripedColor = "#282828",
    highlightColor = "#282828",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#060606",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(backgroundColor = "#2a9fd6", color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(backgroundColor = "#2a9fd6", color = "#ffffff"),
      fontSize = "15px"
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#349cf4"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#888888",
      "&:focus" = list(color = "#888888")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#888888"),
    rowSelectedStyle = list(backgroundColor = "#2a9fd6", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#282828",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#2a9fd6", color = "#ffffff")
    ),
    pageButtonStyle = list(
      backgroundColor = "#282828",
      color = "#ffffff",
      "&:hover" = list(backgroundColor = "#2a9fd6", color = "#ffffff")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#2a9fd6", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#2a9fd6", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#2a9fd6", color = "#ffffff")
  )
}


#' Theme darkly
#'
#' Bootstrap-inspired darkly theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #ffffff.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #afbdcc.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard darkly theme
#' reactable(data,
#'           theme = darkly())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = darkly(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

darkly <- function(font_size = 14,
                   font_color = "#ffffff",
                   header_font_size = 15,
                   header_font_color = "#afbdcc",
                   cell_padding = 6,
                   centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#222222",
    borderColor = "#222222",
    borderWidth = "1px",
    stripedColor = "#adb5bd",
    highlightColor = "#adb5bd",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#375a7f",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#afbdcc"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#212529",
      borderColor = "#222222",
      "&:focus" = list(color = "#212529")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#212529"),
    rowSelectedStyle = list(backgroundColor = "#375a7f", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#00bc8c",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#00efb2")
    ),
    pageButtonStyle = list(
      backgroundColor = "#00bc8c",
      color = "#ffffff",
      "&:hover" = list(backgroundColor = "#00efb2")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#00efb2", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#00efb2", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#00efb2", color = "#ffffff")
  )
}


#' Theme flatly
#'
#' Bootstrap-inspired flatly theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #212529.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #ffffff.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard flatly theme
#' reactable(data,
#'           theme = flatly())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = flatly(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

flatly <- function(font_size = 14,
                   font_color = "#212529",
                   header_font_size = 15,
                   header_font_color = "#ffffff",
                   cell_padding = 6,
                   centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#ffffff",
    borderWidth = "1px",
    stripedColor = "#ecf0f1",
    highlightColor = "#ecf0f1",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#2c3e50",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#1cbc9c"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#1cbc9c"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#1cbc9c"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#212529",
      "&:focus" = list(color = "#212529")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#212529"),
    rowSelectedStyle = list(backgroundColor = "#2c3e50", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#18bc9c",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#0f7864")
    ),
    pageButtonStyle = list(
      backgroundColor = "#18bc9c",
      color = "#ffffff",
      "&:hover" = list(backgroundColor = "#0f7864")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#0f7864", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#0f7864", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#0f7864", color = "#ffffff")
  )
}


#' Theme journal
#'
#' Bootstrap-inspired journal theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #fad9d8.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard journal theme
#' reactable(data,
#'           theme = journal())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = journal(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

journal <- function(font_size = 14,
                    font_color = "#222222",
                    header_font_size = 15,
                    header_font_color = "#fad9d8",
                    cell_padding = 6,
                    centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#aaaaaa",
    borderWidth = "1px",
    stripedColor = "#eeeeee",
    highlightColor = "#eeeeee",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#ef8683",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#000000"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#212529",
      borderColor = "#df9696",
      "&:focus" = list(color = "#212529")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#212529"),
    rowSelectedStyle = list(backgroundColor = "#f9d5d4", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#ffffff",
      color = "#eb6864",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#eb6864", color = "#ffffff")
    ),
    pageButtonStyle = list(
      backgroundColor = "#ffffff",
      color = "#eb6864",
      "&:hover" = list(backgroundColor = "#eb6864", color = "#ffffff")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#eb6864", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#eb6864", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#eb6864", color = "#ffffff")
  )
}


#' Theme lux
#'
#' Bootstrap-inspired lux theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #8c8c8c.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #7f7f7f.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard lux theme
#' reactable(data,
#'           theme = lux())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = lux(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

lux <- function(font_size = 14,
                font_color = "#8c8c8c",
                header_font_size = 15,
                header_font_color = "#7f7f7f",
                cell_padding = 6,
                centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#f7f7f9",
    borderWidth = "1px",
    stripedColor = "#dadada",
    highlightColor = "#f7f7f9",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#1a1a1a",
      color = header_font_color,
      textTransform = "uppercase",
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(textTransform = "uppercase",
                             color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#000000"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#f7f7f9",
      color = "#8c8c8c",
      borderColor = "#f7f7f9",
      "&:focus" = list(color = "#888888")
    ),
    inputStyle = list(backgroundColor = "#f7f7f9", color = "#8c8c8c"),
    rowSelectedStyle = list(backgroundColor = "#1a1a1a", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#282828",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#f7f7f9", color = "#1a1a1a")
    ),
    pageButtonStyle = list(
      backgroundColor = "#ffffff",
      color = "#1a1a1a",
      "&:hover" = list(backgroundColor = "#f7f7f9", color = "#1a1a1a")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#f7f7f9", color = "#1a1a1a"),
    pageButtonActiveStyle = list(backgroundColor = "#1a1a1a", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#1a1a1a", color = "#ffffff")
  )
}


#' Theme minty
#'
#' Bootstrap-inspired minty theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #9a9a9a.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 16.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #c9e7de.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard minty theme
#' reactable(data,
#'           theme = minty())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = minty(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

minty <- function(font_size = 15,
                  font_color = "#9a9a9a",
                  header_font_size = 16,
                  header_font_color = "#c9e7de",
                  cell_padding = 6,
                  centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#f7f7f9",
    borderWidth = "1px",
    stripedColor = "#ededed",
    highlightColor = "#f7f7f9",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#78c2ad",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#111111"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#9a9a9a",
      borderColor = "#b3cecc",
      "&:focus" = list(color = "#888888")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#9a9a9a"),
    rowSelectedStyle = list(backgroundColor = "#78c2ad", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#f3969a",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      backgroundColor = "#78c2ad",
      color = "#ffffff",
      "&:hover" = list(backgroundColor = "#f3969a", color = "#ffffff")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#f3969a", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#f3969a", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#f3969a", color = "#ffffff")
  )
}


#' Theme sandstone
#'
#' Bootstrap-inspired sandstone theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #3e3f3a.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 16.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #7c7a78.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard sandstone theme
#' reactable(data,
#'           theme = sandstone())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = sandstone(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

sandstone <- function(font_size = 15,
                      font_color = "#3e3f3a",
                      header_font_size = 16,
                      header_font_color = "#7c7a78",
                      cell_padding = 6,
                      centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#f8f5f0",
    borderWidth = "1px",
    stripedColor = "#ededed",
    highlightColor = "#f8f5f0",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#f8f5f0",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#000000"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#000000"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#000000"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#3e3f3a",
      borderColor = "#bcbfc1",
      "&:focus" = list(color = "#3e3f3a")
    ),
    inputStyle = list(
      backgroundColor = "#ffffff",
      borderColor = "#bcbfc1",
      color = "#3e3f3a"
    ),
    rowSelectedStyle = list(backgroundColor = "#dfd7ca", color = "#8e8c84"),
    selectStyle = list(
      backgroundColor = "#dfd7ca",
      color = "#8e8c84",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      backgroundColor = "#f8f5f0",
      color = "#8e8c84",
      "&:hover" = list(backgroundColor = "#f3969a", color = "#8e8c84")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#dfd7ca", color = "#8e8c84"),
    pageButtonActiveStyle = list(backgroundColor = "#dfd7ca", color = "#8e8c84"),
    pageButtonCurrentStyle = list(backgroundColor = "#dfd7ca", color = "#8e8c84")
  )
}


#' Theme slate
#'
#' Bootstrap-inspired slate theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #aaaaaa.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 16.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #97999b.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard slate theme
#' reactable(data,
#'           theme = slate())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = slate(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

slate <- function(font_size = 15,
                  font_color = "#aaaaaa",
                  header_font_size = 16,
                  header_font_color = "#97999b",
                  cell_padding = 6,
                  centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#272b30",
    borderColor = "#272b30",
    borderWidth = "1px",
    stripedColor = "#464a4d",
    highlightColor = "#464a4d",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#3a3f44",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(backgroundColor = "#141516", color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(backgroundColor = "#141516", color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#aaaaaa",
      borderColor = "#dfe3e7",
      "&:focus" = list(color = "#aaaaaa")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#aaaaaa"),
    rowSelectedStyle = list(backgroundColor = "#181a1c", color = "#ffffff"),
    selectStyle = list(
      backgroundColor = "#41464c",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff",
      "&:hover" = list(backgroundColor = "#17191b", color = "#ffffff")
    ),
    pageButtonStyle = list(
      backgroundColor = "#41464c",
      color = "#ffffff",
      "&:hover" = list(backgroundColor = "#17191b", color = "#ffffff")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#17191b", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#17191b", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#17191b", color = "#ffffff")
  )
}


#' Theme spacelab
#'
#' Bootstrap-inspired spacelab theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #8e8e8e.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #8e8e8e.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard spacelab theme
#' reactable(data,
#'           theme = spacelab())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = spacelab(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

spacelab <- function(font_size = 14,
                     font_color = "#8e8e8e",
                     header_font_size = 15,
                     header_font_color = "#8e8e8e",
                     cell_padding = 6,
                     centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#eeeeee",
    borderWidth = "1px",
    stripedColor = "#dadada",
    highlightColor = "#dadada",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#ededed",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort]" = list(color = "#349cf4"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#349cf4"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#349cf4"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#8e8e8e",
      "&:focus" = list(color = "#8e8e8e")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#8e8e8e"),
    rowSelectedStyle = list(backgroundColor = "rgba(52, 156, 244, 0.5)"),
    selectStyle = list(
      backgroundColor = "#436d99",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonHoverStyle = list(backgroundColor = "#ededed", color = "#8e8e8e"),
    pageButtonActiveStyle = list(backgroundColor = "#436d99", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#436d99", color = "#ffffff")
  )
}


#' Theme superhero
#'
#' Bootstrap-inspired superhero theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #ebebeb.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #ebebeb.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard superhero theme
#' reactable(data,
#'           theme = superhero())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = superhero(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

superhero <- function(font_size = 14,
                      font_color = "#ebebeb",
                      header_font_size = 15,
                      header_font_color = "#ebebeb",
                      cell_padding = 6,
                      centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#2b3e50",
    borderColor = "#2b3e50",
    borderWidth = "1px",
    stripedColor = "#4a5969",
    highlightColor = "#4a5969",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#4e5d6c",
      color = header_font_color,
      transitionDuration = "0.5s",
      "&:hover[aria-sort], &:focus" = list(color = "#ffffff"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(color = "#ffffff"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(color = font_color,
                             fontSize = header_font_size),
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      border = "none",
      "&:focus" = list(color = "#2b3e50", border = "none")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#2b3e50"),
    rowSelectedStyle = list(backgroundColor = "rgba(223, 105, 26, 1)"),
    selectStyle = list(backgroundColor = "rgba(223, 105, 26, 1)", color = "#ebebeb"),
    pageButtonHoverStyle = list(backgroundColor = "#abb6c2"),
    pageButtonActiveStyle = list(backgroundColor = "rgba(223, 105, 26, 1)"),
    pageButtonCurrentStyle = list(backgroundColor = "rgba(223, 105, 26, 1)")
  )
}


#' Theme espn
#'
#' ESPN-inspired table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 12.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #6C6D6F.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 11.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #48494a.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 7.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard espn theme
#' reactable(data,
#'           theme = espn())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = espn(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

espn <- function(font_size = 12,
                 font_color = "#6C6D6F",
                 header_font_size = 11,
                 header_font_color = "#48494a",
                 cell_padding = 7,
                 centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#ededed",
    stripedColor = "#fafafa",
    highlightColor = "#fafafa",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderTop = "1px solid #e1e2e4",
      borderBottom = "1px solid #e1e2e4",
      padding = "4px",
      background = "#ffffff",
      borderColor = "#ffffff",
      color = header_font_color,
      textTransform = "uppercase",
      "&:hover" = list(color = "#004D9A"),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      borderTop = "1px solid #e1e2e4",
      borderLeft = "1px solid #e1e2e4",
      borderRight = "1px solid #e1e2e4",
      backgroundColor = "#ffffff",
      textTransform = "uppercase",
      fontSize = "11px",
      color = font_color,
      fontSize = header_font_size
    ),
    searchInputStyle = list(color = "#6C6D6F",
                            fontSize = "13px"),
    inputStyle = list(backgroundColor = "#ffffff", color = "#6C6D6F"),
    rowSelectedStyle = list(backgroundColor = "#48494a"),
    selectStyle = list(color = "#48494a"),
    pageButtonStyle = list(color = "#48494a", fontSize = "13px"),
    paginationStyle = list(color = "#48494a", fontSize = "13px")
  )
}


#' Theme fivethirtyeight
#'
#' 538-inspired table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 12.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #000000.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 5.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard fivethirtyeight theme
#' reactable(data,
#'           theme = fivethirtyeight())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = fivethirtyeight(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

fivethirtyeight <- function(font_size = 14,
                            font_color = "#222222",
                            header_font_size = 12,
                            header_font_color = "#000000",
                            cell_padding = 5,
                            centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#dddddd",
    stripedColor = "#dddddd",
    highlightColor = "#f0f0f0",
    cellPadding = cell_padding,
    tableStyle = list(
      fontSize = font_size,
      borderBottom = "3px solid #222222"
    ),
    headerStyle = list(
      borderWidth = "3px",
      paddingTop = "12px",
      verticalAlign = "bottom",
      textAlign = "bottom",
      background = "#ffffff",
      textTransform = "uppercase",
      borderColor = "#222222",
      color = header_font_color,
      "&:hover" = list(background = "#dddddd"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#5b5e5f", color = "#ffffff"),
      borderColor = "#333",
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(
        paddingBottom = "3px",
        verticalAlign = "bottom",
        textAlign = "bottom",
        backgroundColor = "#ffffff",
        textTransform = "uppercase",
        fontSize = header_font_size,
        color = font_color
      )
    ),
    searchInputStyle = list(
      textTransform = "uppercase",
      color = "#222222",
      fontSize = "14px"
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#222222"),
    rowSelectedStyle = list(backgroundColor = "#dddddd"),
    pageButtonStyle = list(textTransform = "uppercase", fontSize = "14px"),
    paginationStyle = list(textTransform = "uppercase", fontSize = "14px")
  )
}


#' Theme nytimes
#'
#' The New York Times-inspired table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 13.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #333333.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 11.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #999999.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 5.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard nytimes theme
#' reactable(data,
#'           theme = nytimes())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = nytimes(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

nytimes <- function(font_size = 13,
                    font_color = "#333333",
                    header_font_size = 11,
                    header_font_color = "#999999",
                    cell_padding = 5,
                    centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#e7e7e7",
    stripedColor = "#e7e7e7",
    highlightColor = "#eeeeee",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "0px",
      padding = "5px",
      background = "#ffffff",
      borderColor = "#ffffff",
      color = header_font_color,
      fontWeight = "500",
      textTransform = "uppercase",
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(
        borderWidth = "0px",
        backgroundColor = "#ffffff",
        textTransform = "uppercase",
        fontSize = header_font_size,
        borderColor = "#ffffff",
        color = font_color
      )
    ),
    searchInputStyle = list(color = "#333333",
                            fontSize = "13px"),
    inputStyle = list(backgroundColor = "#ffffff", color = "#333333"),
    rowSelectedStyle = list(backgroundColor = "#e9edf0"),
    selectStyle = list(color = "#333333"),
    pageButtonStyle = list(color = "#333333", fontSize = "14px"),
    paginationStyle = list(color = "#333333", fontSize = "14px")
  )
}


#' Theme pff
#'
#' Pro Football Focus-inspired table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 16.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #878e94.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 12.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #ffffff.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 4.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard pff theme
#' reactable(data,
#'           theme = pff())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = pff(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

pff <- function(font_size = 16,
                font_color = "#878e94",
                header_font_size = 12,
                header_font_color = "#ffffff",
                cell_padding = 4,
                centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "0px",
    stripedColor = "#f9f9fb",
    highlightColor = "#f1f3f4",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "1px",
      padding = "7px",
      borderTop = "1px solid #595d63",
      backgroundColor = "#595d63",
      textTransform = "uppercase",
      borderColor = "#595d63",
      color = header_font_color,
      transitionDuration = "0.1s",
      "&:hover[aria-sort]" = list(backgroundColor = "#4d5056", color = "#ffffff"),
      "&[aria-sort='ascending']" = list(
        backgroundColor = "#393c40",
        color = "#ffffff",
        boxShadow = "inset 0 3px 0 0 #ffffff"
      ),
      "&[aria-sort='descending']" = list(
        backgroundColor = "#393c40",
        color = "#ffffff",
        boxShadow = "inset 0 -3px 0 0 #ffffff"
      ),
      fontSize = header_font_size,
      fontWeight = "bold"
    ),
    groupHeaderStyle = list(
      "&:not(:empty)" = list(
        backgroundColor = "#e4e8ed",
        textTransform = "uppercase",
        fontWeight = "bold",
        color = font_color,
        fontSize = header_font_size,
        boxShadow = "inset 3px 0 0 0 #ffffff"
      )
    ),
    searchInputStyle = list(
      fontSize = "14px",
      textTransform = "uppercase",
      backgroundColor = "#ffffff",
      color = "#878e94",
      "&:focus" = list(color = "#878e94")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#878e94"),
    rowSelectedStyle = list(backgroundColor = "#e4e8ed"),
    selectStyle = list(
      backgroundColor = "#073c57",
      color = "#ffffff",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      textTransform = "uppercase",
      fontSize = "15px",
      backgroundColor = "#ffffff",
      color = "#164861",
      "&:hover" = list(backgroundColor = "#052c3f", color = "#ffffff")
    ),
    pageButtonHoverStyle = list(backgroundColor = "#052c3f", color = "#ffffff"),
    pageButtonActiveStyle = list(backgroundColor = "#073c57", color = "#ffffff"),
    pageButtonCurrentStyle = list(backgroundColor = "#073c57", color = "#ffffff"),
    paginationStyle = list(textTransform = "uppercase", fontSize = "15px")
  )
}


#' Theme sanfran
#'
#' San Francisco Chronicles-inspired table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is transparent
#'
#' @param cell_color Color of the background of the cells.
#'      Default is #f5f5f5.
#'
#' @param cell_border_width Numeric value representing the border width of the cells.
#'      Default is 6.
#'
#' @param cell_border_color Numeric value representing the border color of the cells.
#'      Default is #ffffff.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param pagination_color Color of the pagination below the table.
#'      Default is #222222.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard void theme
#' reactable(data,
#'           theme = sanfran())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = sanfran(font_size = 12, font_color = "grey"))
#'
#' @export

sanfran <- function(font_size = 14,
                    font_color = "#222222",
                    header_font_size = 15,
                    header_font_color = "#212121",
                    cell_color = "#f5f5f5",
                    cell_border_width = 6,
                    cell_border_color = "#ffffff",
                    cell_padding = 6,
                    pagination_color = "#222222",
                    centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex",
                            flexDirection = "column",
                            justifyContent = "center",
                            background = cell_color,
                            borderWidth = cell_border_width,
                            borderColor = cell_border_color)

  } else {

    centered_content = list(background = cell_color,
                        borderWidth = cell_border_width,
                        borderColor = cell_border_color)
  }

  reactableTheme(
    color = font_color,
    backgroundColor = "transparent",
    borderColor = "#f5f5f5",
    stripedColor = "lightgrey",
    highlightColor = "lightgrey",
    cellPadding = cell_padding,
    cellStyle = centered_content,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(color = header_font_color,
                       fontSize = header_font_size),
    selectStyle = list(color = pagination_color),
    paginationStyle = list(color = pagination_color)
  )
}


#' Theme hoverdark
#'
#' Changes from light-themed to dark-themed on hover
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 4.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard hoverdark theme
#' reactable(data,
#'           theme = hoverdark())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = hoverdark(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

hoverdark <- function(font_size = 15,
                      font_color = "#222222",
                      header_font_size = 15,
                      cell_padding = 4,
                      centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "grey",
    borderWidth = "1px",
    stripedColor = "#D6D6D6",
    highlightColor = "#CACACA",
    cellPadding = cell_padding,
    tableStyle = list(
      transitionDuration = "2s",
      "&:hover" = list(backgroundColor = "black", color = "#ffffff"),
      fontSize = font_size
    ),
    headerStyle = list(
      borderWidth = "2px",
      "&:hover" = list(borderColor = "#ffffff"),
      fontSize = header_font_size
    )
  )
}


#' Theme hoverlight
#'
#' Changes from dark-themed to light-themed on hover
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table.
#'      Default is #ffffff.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 4.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard hoverlight theme
#' reactable(data,
#'           theme = hoverlight())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = hoverlight(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

hoverlight <- function(font_size = 15,
                       font_color = "#ffffff",
                       header_font_size = 15,
                       cell_padding = 4,
                       centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#000000",
    borderColor = "grey",
    borderWidth = "1px",
    stripedColor = "lightgrey",
    highlightColor = "lightgrey",
    cellPadding = cell_padding,
    tableStyle = list(
      transitionDuration = "2s",
      "&:hover" = list(backgroundColor = "white", color = "#000000"),
      fontSize = font_size
    ),
    headerStyle = list(
      borderWidth = "2px",
      "&:hover" = list(borderColor = "black"),
      fontSize = header_font_size
    ),
    paginationStyle = list(backgroundColor = "#ffffff", color = "black")
  )
}


#' Theme dark
#'
#' dark table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #FFFFFF.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 16.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #FFFFFF.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard dark theme
#' reactable(data,
#'           theme = dark())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = dark(font_size = 12, font_color = "red", cell_padding = 3))
#'
#' @export

dark <- function(font_size = 15,
                 font_color = "#FFFFFF",
                 header_font_size = 16,
                 header_font_color = "#FFFFFF",
                 cell_padding = 6,
                 centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

  centered_content <- list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else {

  centered_content <- list(display = "flex")
  }

  reactableTheme(
    color = font_color,
    backgroundColor = "#252525",
    borderWidth = "1px",
    borderColor = "#434343",
    stripedColor = "#303030",
    highlightColor = "#303030",
    cellPadding = cell_padding,
    style = list(backgroundColor = "#252525"),
    tableStyle = list(fontSize = font_size),
    cellStyle = centered_content,
    headerStyle = list(
      borderWidth = "2px",
      backgroundColor = "#252525",
      color = header_font_color,
      borderColor = "#ececec",
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      backgroundColor = "#252525",
      fontSize = header_font_size,
      color = font_color
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#252525"
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#252525"),
    rowSelectedStyle = list(backgroundImage = "linear-gradient(#191919, #252525)"),
    selectStyle = list(
      backgroundImage = "linear-gradient(#191919, #252525)",
      backgroundColor = "#999999",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      backgroundImage = "linear-gradient(#191919, #252525)"
    ),
    pageButtonHoverStyle = list(color = "#ffffff"),
    pageButtonActiveStyle = list(color = "#ffffff"),
    pageButtonCurrentStyle = list(color = "#ffffff"),
    paginationStyle = list(backgroundImage = "linear-gradient(#191919, #252525)")
  )
}


#' Theme midnight
#'
#' midnight table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #727272.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #666666.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard midnight theme
#' reactable(data,
#'           theme = midnight())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = midnight(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

midnight <- function(font_size = 15,
                     font_color = "#727272",
                     header_font_size = 15,
                     header_font_color = "#666666",
                     cell_padding = 6,
                     centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

  centered_content <- list(display = "flex", flexDirection = "column", justifyContent = "center",
      "&:hover" = list(
        transitionDuration = "0.25s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      ))

  } else {

  centered_content <- list(
      "&:hover" = list(
        transitionDuration = "0.25s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      ))
  }

  reactableTheme(
    color = font_color,
    backgroundColor = "#141518",
    borderWidth = "0px",
    stripedColor = "#00468c",
    highlightColor = "#00468c",
    cellPadding = cell_padding,
    style = list(backgroundColor = "#000000"),
    tableStyle = list(fontSize = font_size),
    cellStyle = centered_content,
    tableBodyStyle = list(backgroundImage = "linear-gradient(#000000, #0d0d0d, #191919)"),
    headerStyle = list(
      borderWidth = "0px",
      backgroundColor = "#000000",
      color = header_font_color,
      transitionDuration = "0.5s",
      transitionTimingFunction = "ease-out",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending']" = list(color = "#ffffff"),
      "&[aria-sort='descending']" = list(
        color = "#ffffff",
        borderBottomColor = "#ffffff",
        borderWidth = "1px"
      ),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      backgroundColor = "#000000",
      fontSize = header_font_size,
      color = font_color,
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#262626",
      "&:focus" = list(color = "#262626")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#262626"),
    rowSelectedStyle = list(backgroundImage = "linear-gradient(#191919, #262626)"),
    selectStyle = list(
      backgroundImage = "linear-gradient(#191919, #262626)",
      backgroundColor = "#00468c",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      backgroundImage = "linear-gradient(#191919, #262626)",
      "&:hover" = list(color = "#ffffff")
    ),
    pageButtonHoverStyle = list(color = "#ffffff"),
    pageButtonActiveStyle = list(color = "#ffffff"),
    pageButtonCurrentStyle = list(color = "#ffffff"),
    paginationStyle = list(backgroundImage = "linear-gradient(#191919, #262626)")
  )
}


#' Theme midnightblue
#'
#' midnightblue table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #bababa.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is lightgrey.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard midnightblue theme
#' reactable(data,
#'           theme = midnightblue())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = midnightblue(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export


midnightblue <- function(font_size = 15,
                         font_color = "#bababa",
                         header_font_size = 15,
                         header_font_color = "lightgrey",
                         cell_padding = 6,
                         centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

  centered_content <- list(display = "flex", flexDirection = "column", justifyContent = "center",
      "&:hover" = list(
        transitionDuration = "0.25s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      ))

  } else {

  centered_content <- list(
      "&:hover" = list(
        transitionDuration = "0.25s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      ))
  }

  reactableTheme(
    color = font_color,
    backgroundColor = "#002853",
    borderWidth = "0px",
    stripedColor = "#00468c",
    highlightColor = "#00468c",
    cellPadding = cell_padding,
    style = list(backgroundColor = "#001021"),
    tableStyle = list(fontSize = font_size),
    cellStyle = centered_content,
    tableBodyStyle = list(backgroundImage = "linear-gradient(#001021, #001c3a, #002853)"),
    headerStyle = list(
      borderWidth = "1px",
      backgroundColor = "#001021",
      borderColor = "#001021",
      color = header_font_color,
      transitionDuration = "1s",
      transitionTimingFunction = "ease-out",
      "&:hover[aria-sort]" = list(color = "#ffffff"),
      "&[aria-sort='ascending']" = list(color = "#ffffff"),
      "&[aria-sort='descending']" = list(
        color = "#ffffff",
        borderBottomColor = "#ffffff",
        borderWidth = "1px"
      ),
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      backgroundColor = "#001021",
      fontSize = header_font_size,
      color = font_color,
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = "#ffffff"
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#141415",
      "&:focus" = list(color = "#141415")
    ),
    inputStyle = list(backgroundColor = "#ffffff", color = "#141415"),
    rowSelectedStyle = list(backgroundImage = "linear-gradient(#002853, #003766)"),
    selectStyle = list(
      backgroundImage = "linear-gradient(#002853, #003766)",
      backgroundColor = "#00468c",
      borderColor = "#ffffff",
      outlineColor = "#ffffff"
    ),
    pageButtonStyle = list(
      backgroundImage = "linear-gradient(#002853, #003766)",
      "&:hover" = list(color = "#ffffff")
    ),
    pageButtonHoverStyle = list(color = "#ffffff"),
    pageButtonActiveStyle = list(color = "#ffffff"),
    pageButtonCurrentStyle = list(color = "#ffffff"),
    paginationStyle = list(backgroundImage = "linear-gradient(#002853, #003766)")
  )
}


#' Theme sunrise
#'
#' sunrise table theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param font_color Color of the font for the text within the table and the group headers.
#'      Default is #8069ff.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #8069ff.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard sunrise theme
#' reactable(data,
#'           theme = sunrise())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = sunrise(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

sunrise <- function(font_size = 15,
                    font_color = "#8069ff",
                    header_font_size = 15,
                    header_font_color = "#8069ff",
                    cell_padding = 6,
                    centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

  centered_content <- list(display = "flex", flexDirection = "column", justifyContent = "center",
      "&:hover" = list(
        transitionDuration = "0.25s",
        transitionTimingFunction = "ease-out",
        color = "#699dff"
      ))

  } else {

  centered_content <- list(
      "&:hover" = list(
        transitionDuration = "0.25s",
        transitionTimingFunction = "ease-out",
        color = "#699dff"
      ))
  }

  reactableTheme(
    color = font_color,
    backgroundColor = "#ffcb69",
    borderWidth = "0px",
    stripedColor = "lightblue",
    highlightColor = "#ffffff",
    cellPadding = cell_padding,
    style = list(backgroundColor = "#fffa85"),
    tableStyle = list(
      fontSize = font_size
    ),
    cellStyle = centered_content,
    tableBodyStyle = list(backgroundImage = "linear-gradient(#fffa85, #ffe269, #ffcb69, #f98d77)"),
    headerStyle = list(
      borderWidth = "1px",
      backgroundColor = "#fffa85",
      borderColor = "#fffa85",
      color = header_font_color,
      transitionDuration = "1s",
      transitionTimingFunction = "ease-out",
      "&:hover[aria-sort]" = list(color = "#699dff"),
      "&[aria-sort='ascending']" = list(color = "#699dff", boxShadow = "inset 0 1px 0 0 #699dff"),
      "&[aria-sort='descending']" = list(color = "#699dff", borderBottomColor = "#699dff", borderWidth = "1px"),
      fontSize = header_font_size),
    groupHeaderStyle = list(
      backgroundColor = "#fffa85",
      "&:hover" = list(
        fontWeight = "bold",
        transitionDuration = "1s",
        transitionTimingFunction = "ease-out",
        color = font_color,
        fontSize = header_font_size
      )
    ),
    searchInputStyle = list(
      backgroundColor = "#ffffff",
      color = "#8069ff",
      "&:focus" = list(color = "#8069ff")),
    inputStyle = list(backgroundColor = "#ffffff", color = "#8069ff"),
    rowSelectedStyle = list(backgroundImage = "linear-gradient(#f98d77, #e65c6f)"),
    selectStyle = list(backgroundImage = "linear-gradient(#f98d77, #e65c6f)",
                       backgroundColor = "#00468c",
                       borderColor = "#ffffff", outlineColor = "#ffffff"),
    pageButtonStyle = list(backgroundImage = "linear-gradient(#f98d77, #e65c6f)",
                           "&:hover" = list(color = "#ffffff")),
    pageButtonHoverStyle = list(color = "#ffffff"),
    pageButtonActiveStyle = list(color = "#ffffff"),
    pageButtonCurrentStyle = list(color = "#ffffff"),
    paginationStyle = list(backgroundImage = "linear-gradient(#f98d77, #e65c6f)")
  )
}


#' Theme clean
#'
#' Simple clean-look theme
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is #222222.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard clean theme
#' reactable(data,
#'           theme = clean())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = clean(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

clean <- function(font_size = 14,
                  font_color = "#222222",
                  header_font_size = 15,
                  header_font_color = "#222222",
                  cell_padding = 6,
                  centered = FALSE) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex", flexDirection = "column", justifyContent = "center")

  } else { centered_content = NULL }

  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderColor = "#ffffff",
    stripedColor = "#e0e0e0",
    highlightColor = "#e0e0e0",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(
      borderWidth = "2px",
      borderColor = "#3b3b3b",
      color = header_font_color,
      fontSize = header_font_size
    ),
    rowSelectedStyle = list(backgroundColor = "#e0e0e0")
  )
}


#' Theme no_lines
#'
#' A table style with no lines or borders
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is transparent
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard no_lines theme
#' reactable(data,
#'           theme = no_lines())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = no_lines(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

no_lines <- function(font_size = 14,
                 font_color = "#222222",
                 header_font_size = 15,
                 header_font_color = "#222222",
                 centered = FALSE,
                 cell_padding = 6) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex",
                            flexDirection = "column",
                            justifyContent = "center",
                            borderColor = "transparent")

  } else { centered_content = list(borderColor = "transparent") }

  reactableTheme(
    color = font_color,
    backgroundColor = "transparent",
    stripedColor = "lightgrey",
    highlightColor = "lightgrey",
    cellPadding = cell_padding,
    cellStyle = centered_content,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(color = header_font_color,
                       borderColor = "transparent",
                       fontSize = header_font_size),
    selectStyle = list(color = "transparent"),
    paginationStyle = list(color = "transparent",
                           borderColor = "transparent")
  )
}


#' Theme void
#'
#' A table style completely void of borders and headers
#'
#' @param font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 14.
#'
#' @param font_color Color of the font for the text within the table.
#'      Default is #222222.
#'
#' @param header_font_size Numeric value representing the size of the font within the table (in px).
#'      Default is 15.
#'
#' @param header_font_color Color of the font for the header text.
#'      Default is transparent
#'
#' @param border_color Color of the borders between cells.
#'      Default is transparent.
#'
#' @param border_width Numeric value representing the border width between cells (in px).
#'      Default is 0.
#'
#' @param header_border_color Color of the bottom border of the header.
#'      Default is transparent.
#'
#' @param header_border_width Numeric value representing the bottom border width of the header (in px).
#'      Default is 0.
#'
#' @param centered Logical: vertically center the contents of the table.
#'     Default is FALSE.
#'
#' @param cell_padding Numeric value representing the padding size between cells (in px).
#'      Default is 6.
#'
#' @return an object of class theme that is applied to a reactable table.
#'
#' @import reactable
#'
#' @examples
#' data <- iris[10:29, ]
#'
#' ## Standard void theme
#' reactable(data,
#'           theme = void())
#'
#' ## Additional options applied
#' reactable(data,
#'           theme = void(font_size = 12, font_color = "grey", cell_padding = 3))
#'
#' @export

void <- function(font_size = 14,
                 font_color = "#222222",
                 header_font_size = 15,
                 header_font_color = "transparent",
                 border_color = "transparent",
                 border_width = 0,
                 header_border_color = "transparent",
                 header_border_width = 0,
                 centered = FALSE,
                 cell_padding = 6) {

  if (!is.logical(centered)) {

    stop("`centered` must be TRUE or FALSE")
  }

  if (centered == TRUE) {

    centered_content = list(display = "flex",
                            flexDirection = "column",
                            justifyContent = "center",
                            borderColor = border_color,
                            borderWidth = border_width)

  } else { centered_content = list(borderColor = border_color,
                                   borderWidth = border_width) }

  reactableTheme(
    color = font_color,
    backgroundColor = "transparent",
    stripedColor = "lightgrey",
    highlightColor = "lightgrey",
    cellPadding = cell_padding,
    cellStyle = centered_content,
    tableStyle = list(fontSize = font_size),
    headerStyle = list(color = header_font_color,
                       borderBottom = paste0("", header_border_width, "px solid ", header_border_color, ""),
                       fontSize = header_font_size),
    selectStyle = list(color = "transparent"),
    paginationStyle = list(color = "transparent")
  )
}

