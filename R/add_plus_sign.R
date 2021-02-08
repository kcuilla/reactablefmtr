#' Add plus (+) sign to positive values
#'
#' The `add_plus_sign()` function adds a "+" sign to each row of a column that contains a positive value.
#'     Negative ("-") values remain the same.
#'     It should be placed within the cell argument in reactable::colDef.
#'
#' @param value numeric values only.
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#' Symbol = c("GOOG", "FB", "AMZN", "NFLX", "TSLA"),
#' Price = c(1265.13, 187.89, 1761.33, 276.82, 328.13),
#' Change = c(4.14, 1.51, -19.45, 5.32, -12.45))
#'
#' ## Values with a positive value receive a preceding "+" sign
#' reactable(data, columns = list(Change = colDef(cell = add_plus_sign)))


add_plus_sign <- function(value) {

  if (!is.na(value) & value >= 0)
    paste0("+", value)

  else
    value
}
