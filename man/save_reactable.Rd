% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_reactable.R
\name{save_reactable}
\alias{save_reactable}
\title{Save a reactable table as an image or .html file}
\usage{
save_reactable(input, output, ...)
}
\arguments{
\item{input}{A reactable table, .html file, or .Rmd file}

\item{output}{A .png or .html file name for the saved image}

\item{...}{Optional additional parameters passed from webshot2::webshot}
}
\value{
a function that converts a reactable table, .html file, or .Rmd file to an .png file or .html file
    and saves it in the user's working directory.
}
\description{
The `save_reactable()` function converts either a reactable table, .html file, or .Rmd file to an image or .html file
    and saves it in the user's working directory.
    Table can be saved as either a .png file or .html file. Other file types are not currently supported.
    If the reactable table is located within an .Rmd file and has additional CSS styles provided,
    specify the name of the .Rmd file as the input.
    Alternatively, if the reactable table exists in an .html file, specify the name of the .html file as the input.
    Additional parameters available within webshot2::webshot such as vwidth, vheight, and cliprect can be passed through `save_reactable()`.
}
\examples{
\dontrun{
## Save reactable table as a png file:
iris_table <- reactable(iris)
save_reactable(iris_table, "iris_table.png")

## Also works with a pipe
iris_table \%>\%
save_reactable("iris_table.png")

## Or save as an html file:
save_reactable(iris_table, "iris_table.html")

## If the reactable table was built in R Markdown with CSS styles applied,
## specify .Rmd file as input and save_reactable will run the file
## and save the output as an image
save_reactable("iris_table.Rmd", "iris_table.png")

## Alternatively, can do the same with an .html file
save_reactable("iris_table.html", "iris_table.png")
}
}
