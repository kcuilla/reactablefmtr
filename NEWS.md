# reactablefmtr 1.1.0 - development version

### New Features

#### animate

* The animation of `data_bars()` can now be controlled with `animate`. The `animate` parameter has also been added to `color_scales()`, `color_tiles()`, `icon_sets()`, and `icon_assign()`. The duration and timing function of the animation can be changed within `animate` or the animation can be turned off by setting it to 'none'. For available timing functions, see [CSS Transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)

#### cell_style()

* `cell_style()` can be used to customize the appearance of certain cells within a table. For example, if you wanted to display a particular value or set of values in a column as bold and in red text, you can do so by specifying either the row number or values themselves
* Custom styling options include: the color, size, style, and weight of the font within the cell, the color, width, and style of the border around the cell, the vertical and horizontal alignment within the cell, and the background color of the cell. Animation is also applied to `cell_style()` by default, but can be turned off by setting to 'none'

#### color_scales()

* The vertical and horizontal alignment within a cell can now be controlled with the `vertical_alignment` and `horizontal_alignment` options

#### data_bars()

* Optionally force a range of values to display their text labels on the outside-end of the filled bars when the text_position is set to either "inside-end", "inside-base", or "center". This can be useful when the length of the filled data bars is too short to the text labels on the inside of the filled bars
* Control the size of the text labels with `text_size`
* Borders around the filled data bars can now be adjusted with the `border_style`, `border_width`, and `border_color` options

#### embed_img()

* The horizontal alignment within a cell can now be controlled with the `horizontal_alignment` option 

#### icon_assign()

* The alignment of the icons within a column can now be changed to either left, right, or center with `align_icons`. Previously, icons could only be aligned to the left

### Other Modifications

* The default color of `fill_color` within `data_bars()` and `icon_assign()` has been changed to #67a9cf
* The default position of `text_position` within  `data_bars()` has been changed from "outside-end" to "inside-end"
* Text is now centered vertically within `data_bars()` by default
* The size of the text within `data_bars()` can now be changed with `text_size`


# reactablefmtr 1.0.0

### New Features

#### data_bars()

* `data_bars()` can now handle columns with positive and negative values. `data_bars_pos_neg()` is being depreciated
* `data_bars()` can create color gradients when more than two colors are provided with `fill_gradient = TRUE`. `data_bars_gradient()` is being depreciated
* The `colors` argument in `data_bars()` has been renamed to `fill_color`
* The height of the bars can now be adjusted with `bar_height`
* The default color of `background` has been changed from white to transparent
* `data_bars()` can now be either right-aligned or left-aligned with the `align_bars` argument by specifying either "left" or "right". By default, `data_bars()` are aligned left
* The placement of the labels can now be adjusted within `data_bars()` with the `text_position` argument. Labels can be placed either outside the filled bars with "outside-end" or "outside-base", inside the filled bars with "inside-end", "inside-base", or "center", or labels can be hidden with "none". By default, labels are placed on the "outside-end" of the filled bars
* The color of the labels can be changed by using the `text_color` argument. The default color is black
* Labels can be shown in bold text with the `bold_text` logical argument
* If labels are placed inside the filled bars and the bars are filled with a dark color, the color of the labels will automatically change to white rather than the default black with the `brighten_text` logical argument. The color of `brighten_text_color` can be changed to any color
* The colors for the fill of the `data_bars()` can be provided from another column by referencing the name of the column containing the colors within `fill_color_ref`. This is useful for assigning conditions or assigning colors to the bars for different groups within the dataset
* The opacity of the fill color can be adjusted with `fill_opacity` by providing a value between 0 and 1 (from transparent to opaque)
* The maximum width of the fill of `data_bars()` can be adjusted with the `max_value` argument. By default, the maximum width is the maximum value contained in the column. Ex. if showing percentages and the maximum value is 80%, by default, 80% will look 100% filled (since it's 80% out of 80%). By changing `max_value = 1` (or `max_value = 100` depending on how your percentages are displayed), it will shrink the 80% to be relative to 100%. If a maximum value is provided, a minimum value can also be set with `min_value`
* Icons can now be added to the data bars with either `icon` or `icon_ref`
* By default, the color of the icon is inherited from the fill color of the data bars, but can be changed with `icon_color` or `icon_color_ref`
* Images can now be added to the data bars with either `img` or `img_ref`

#### color_scales() & color_tiles()

* Colors can be provided from another column by referencing the name of the column containing the colors within `color_ref`. This is useful for assigning conditions or assigning colors to the bars for different groups within the dataset. If `color_ref` is used within `reactable::defaultColDef`, every column in the dataset, regardless of whether they are numeric or not, will receive the conditional colors. 
* Default colors have been changed from red-white-blue to blue-white-orange
* The opacity of the background colors can be adjusted with `opacity` by providing a value between 0 and 1
* The color of the values can be changed by using the `text_color` argument. The default color is black
* Values can be shown in bold text with the `bold_text` logical argument
* `brighten_values` has been renamed to `brighten_text`
* The color of the values in cells that have `brighten_values` applied can be changed with `brighten_text_color`. The default color is white
* Values can now be hidden by setting `show_text` to FALSE

#### embed_img()

* Labels from another column can now be positioned above, below, or to the left of the embedded image. Previously, the labels could only positioned to the right of the image

#### icon_assign()

* The size of the icons can now be adjusted using `icon_size`
* The opacity of `fill_color` and `empty_color` can be adjusted with `fill_opacity` and `empty_opacity` respectively, by providing a value between 0 and 1 (from transparent to opaque)
* In addition to optionally showing values to the left or right of the icons, values can now also be placed above or below the icons in `show_values`

#### icon_sets()

* Can now use any number of icons and colors to assign to data. Previously only three icons and colors could be used.
* Icons can be positioned to the right, left, above, below, or over the values with `icon_position`. Previously icons could only be positioned to the right of the values
* The size of the icons can now be adjusted using `icon_size`
* Default colors have been changed from red-white-blue to blue-grey-orange
* The opacity of the icon colors can be adjusted with `opacity` by providing a value between 0 and 1 (from transparent to opaque)
* Icons can be provided from another column by referencing the name of the column containing the icons within `icon_ref`
* Icon colors can be provided from another column by referencing the name of the column containing the colors within `icon_color_ref`

#### save_reactable()

* Reactable tables can now be saved as static .png files or dynamic .html files using `save_reactable()`
* If the reactable table was created in a .Rmd file and additional CSS styling are applied, reference the name of either the .Rmd file containing the reactable table or the .html file of the reactable table to save an image of the table
* Note: this feature requires the {webshot2} package which can be downloaded from https://github.com/rstudio/webshot2.

#### add_title(), add_subtitle(), add_source()

* You can now easily add a title and/or a subtitle above any reactable table with `add_title()` and `add_subtitle()`
* You can also add a source below a reactable table with `add_source()`
* Additional customization options such as adjusting the font size, font color, and font family are also available  

#### themes

Added 24 new table themes, including:

* Bootstrap-inspired themes: cerulean, cosmo, cyborg, darkly, flatly, journal, lux, minty, sandstone, slate, spacelab, and superhero
* News/sports-inspired themes: espn, fivethirtyeight, nytimes, and pff
* Other custom themes: clean, default, hoverdark, hoverlight, midnight, midnightblue, sunrise, and void

#### Breaking Changes

* The `colors` argument in `data_bars()` has been renamed to `fill_color`
* The `brighten_values` argument in `color_scales()` and `color_tiles()` has been renamed to `brighten_text`


# reactablefmtr 0.2.0

### New Features

* Data bars can now accept gradient colors by using `data_bars_gradient()`
* Columns can now be conditionally formatted with `color_tiles()`
* Images from the web can now be embedded into a column using `embed_img()`
* Columns can now be formatted using formatters from the `scales` package using the `number_fmt()` option
* By default, values displayed with the `color_scales()` and `color_tiles()` packages will automatically be shown in black or white color depending on the saturation of the background color of the cell. This option can be turned off by setting `bright_values = FALSE`
* Data bars are now animated on sort when using any of the three data bars formatters (`data_bars()`, `data_bars_pos_neg()`, `data_bars_gradient()`)
* Conditional colors can now be applied to values in relation to the entire dataset or to a group of columns by providing column names or column positions with the `span` option within `color_scales()` and `color_tiles()`. Special thanks to June Chao for the contribution for this feature.
* Icons can be assigned to each value in a column by using `icon_assign()`

### Bug Fixes

* An issue where aligning values while using `data_bars()` did not always align properly has been fixed

### Other

* Due to the addition of `number_fmt()` which allows for the use of formatters from the `scales` package, the `add_plus_sign()` formatter will be retired in the next CRAN release as well as the `commas` option in the `data_bars()` formatter, and the `percent` option in the `data_bars_pos_neg()` and `icon_sets()` formatters

# reactablefmtr 0.1.0

* Initial release to CRAN
