## reactablefmtr <a href='https://kcuilla.github.io/reactablefmtr/index.html'><img src="man/figures/reactablefmtr_hex_logo.png" align="right" width="225" height="275"/>

<!-- badges: start -->
[![CRAN Status](https://www.r-pkg.org/badges/version/reactablefmtr?color=blue)](https://cran.r-project.org/package=reactablefmtr?color=blue)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/last-month/reactablefmtr?color=brightgreen)](https://cranlogs.r-pkg.org/badges/last-month/reactablefmtr?color=brightgreen)
<!-- badges: end -->

The {reactablefmtr} package simplifies and enhances the styling and formatting of tables built with the {reactable} **R** package. The {reactablefmtr} package provides many conditional formatters that are highly customizable and easy to use.

* **Conditionally format tables** with [color scales](https://kcuilla.github.io/reactablefmtr/articles/color_scales.html), [color tiles](https://kcuilla.github.io/reactablefmtr/articles/color_tiles.html), [data bars](https://kcuilla.github.io/reactablefmtr/articles/data_bars_development.html). Assign icons to values from [Font Awesome](https://fontawesome.com/icons?d=gallery&p=2) with [icon assign](https://kcuilla.github.io/reactablefmtr/articles/icon_assign.html) and [icon sets](https://kcuilla.github.io/reactablefmtr/articles/icon_sets.html).
* **Customized table themes** with a growing list of [themes](https://kcuilla.github.io/reactablefmtr/articles/themes.html) that can easily be applied to any {reactablefmtr} or {reactable} table.
* **Embed images** directly from the web into your table with [`embed_img()`](https://kcuilla.github.io/reactablefmtr/articles/embed_img.html).
* **Save tables** as static PNG files or as interactive HTML files with `save_reactable()`.

The {reactablefmtr} package was built using a combination of **R**, **CSS**, and **HTML** in order to allow any level of **R** user to build highly customizable and stylish tables without having to learn additional programming languages.

## Installation

The {reactablefmtr} package is available from CRAN and can be installed with:

```{r}
install.packages("reactablefmtr")
```

Or install the development version of {reactablefmtr} with:

```{r}
remotes::install_github("kcuilla/reactablefmtr")
```

## Examples

## Data Bars

Use `data_bars()` to assign a horizontal bars to each row. See the [tutorial](https://kcuilla.github.io/reactablefmtr/articles/data_bars_development.html) for customization options. 

![](man/figures/data_bars_animated_demo.gif)


## Color Scales

By default, `color_scales()` assigns a three-color red-white-blue pattern based on the value of the cells in a column from low to high:

In {reactable}, using dark color palettes such as the "magma" color set from {viridis} is troublesome since you can't see the values in the cells with dark backgrounds. Now, with {reactablefmtr}, the colors of the values automatically are changed to white if the colors are dark:

```{r}
library(viridis)
reactable(data,
          defaultColDef = colDef(style = color_scales(data,
                                                      colors = viridis::magma(5))))
```
<img src="man/figures/README_color_scales_bright_values.PNG" align="center" />


## Color Tiles

Another option of conditionally coloring cells based on their values is with `color_tiles()`: 

```{r}
library(viridis)
reactable(data,
          defaultColDef = colDef(style = color_tiles(data,
                                                     colors = viridis::magma(5))))
```
<img src="man/figures/README_color_tiles_bright_values.PNG" align="center" />


## Save Static or Interactive Tables

{reactablefmtr} or {reactable} tables can be saved directly to a file as a static PNG image or interactive HTML file with `save_reactable()`.

Save as a PNG file:

```{r}
save_reactable(table_name, "table.png")
```

Save as an HTML file:

```{r}
save_reactable(table_name, "table.html")
```

If custom CSS styling is applied to the table within an R Markdown document:

```{r}
save_reactable("table_name.Rmd", "table.png")
```


## Acknowledgments & Contributions

A huge thank you to Greg Lin for creating the amazing {reactable} [package]((https://glin.github.io/reactable/index.html))! Without Greg, {reactablefmtr} simply would not exist! 

Also thank you to June Chao for contributing the option to choose columns within the span option in `color_scales()` and `color_tiles()`!


