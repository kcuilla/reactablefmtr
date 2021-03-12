# reactablefmtr (development version)

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
