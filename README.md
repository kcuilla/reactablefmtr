# reactablefmtr <img src="man/figures/reactablefmtr_hex_logo.png" align="right" />

<!-- badges: start -->
<!-- badges: end -->

Simplify the styling, formatting, and customization of tables made with `{reactable}`.


## Installation

You can install the `{reactablefmtr}` package from GitHub with:

```{r}
remotes::install_github("kcuilla/reactablefmtr")
library(reactablefmtr)
```

## Background

`{reactablefmtr}` is an extension of the `{reactable}` package created by Greg Lin. Before you use `{reactablefmtr}`, you need to first have an understanding of how to use `{reactable}`. Greg Lin put together a great overview [here](https://glin.github.io/reactable/index.html) that I recommend checking out. It has many examples and tutorials that are necessary to learn `{reactable}`.

A challenge of creating tables with `{reactable}` is that some of the code required to style and format the tables is a bit lengthy and can be difficult to understand for someone who is a novice R user. The `{reactablefmtr}` aims to firstly simplify the customization and formatting process that can be used by any R user regardless of their experience, and secondly, reduce the amount of time and effort required to build a table with {reactable}. 

## Showcase

### color_scales()

By default, `color_scales()` assigns a three-color red-white-blue pattern based on the value of the cells in a column from low to high:

```{r}
data <- iris[10:29, ]

reactable(data,
          columns = list(Petal.Length = colDef(style = color_scales(data))))
```
<img src="man/figures/README_color_scales_default.PNG" align="center" />

You can change the color scheme to any number of colors you'd like by specifying the colors in a vector and `color_scales` will assign the colors from low to high in the order you provide:

```{r}
data <- iris[10:29, ]

reactable(data,
          columns = list(Petal.Length = colDef(style = color_scales(data, c("purple", "pink", "white", "green")))))
```
<img src="man/figures/README_color_scales_example.PNG" align="center" />

You can also apply `color_scales` across all columns and use custom color palettes such as the "Pastel1" color set from {RColorBrewer}:

```{r}
data <- iris[10:29, ]

reactable(data,
          defaultColDef = colDef(style = color_scales(data, brewer.pal(3, "Pastel1"))))
```
<img src="man/figures/README_color_scales_custom.PNG" align="center" />
