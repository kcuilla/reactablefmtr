# reactablefmtr <img src="man/figures/reactable_hex.png" align="right" />

<!-- badges: start -->
<!-- badges: end -->

An R package to simplify formatting tables in {reactable}.


## Installation

You can install the {reactablefmtr} package from GitHub with:

```{r}
remotes::install_github("kcuilla/reactablefmtr")
library(reactablefmtr)
```

## Background

{reactablefmtr} is an extension of the {reactable} package created by Greg Lin. Before you use {reactablefmtr}, you need to first have an understanding of how to use {reactable}. Greg Lin put together a great overview [here](https://glin.github.io/reactable/index.html) that I recommend checking out. It has many examples and tutorials that are necessary to learn {reactable}.

A challenge of creating tables with {reactable} is that some of the code required to style and format the tables is a bit lengthy and can be difficult to understand for someone who is a novice R user. The {reactablefmtr} aims to firstly simplify the customization and formatting process that can be used by any R user regardless of their experience, and secondly, reduce the amount of time and effort required to build a table with {reactable}. 

## Showcase

### color_scales()

Retrieve the number of visits for a given site. The default granularity is monthly, but can set to daily or weekly with the `gr` argument.

```{r}
data <- iris[10:29, ]

reactable(data,
          columns = list(Petal.Length = colDef(style = color_scales(data))))
```
<img src="man/figures/README_color_scales_default.png" />
