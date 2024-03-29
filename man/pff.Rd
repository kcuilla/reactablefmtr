% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{pff}
\alias{pff}
\title{Theme pff}
\usage{
pff(
  font_size = 16,
  font_color = "#878e94",
  header_font_size = 12,
  header_font_color = "#ffffff",
  background_color = NULL,
  cell_padding = 4,
  centered = FALSE
)
}
\arguments{
\item{font_size}{Numeric value representing the size of the font within the table (in px).
Default is 16.}

\item{font_color}{Color of the font for the text within the table and the group headers.
Default is #878e94.}

\item{header_font_size}{Numeric value representing the size of the font within the table (in px).
Default is 12.}

\item{header_font_color}{Color of the font for the header text.
Default is #ffffff.}

\item{background_color}{Background color of the table.
Default is NULL.}

\item{cell_padding}{Numeric value representing the padding size between cells (in px).
Default is 4.}

\item{centered}{Logical: vertically center the contents of the table.
Default is FALSE.}
}
\value{
an object of class theme that is applied to a reactable table.
}
\description{
Pro Football Focus-inspired table theme
}
\examples{
data <- iris[10:29, ]

## Standard pff theme
reactable(data,
          theme = pff())

## Additional options applied
reactable(data,
          theme = pff(font_size = 12, font_color = "grey", cell_padding = 3))

}
