#' function name: histogram
#' Returns the histogram of a column from the WDBC data.
#' @param x a dataframe. It is by default the WDBC data, which
#' has a diagnosis column and multiple columns
#' with breast tissue characteristics.
#' for example, radius_mean records the mean of breast tissue radius
#' @param col a column name input as character.
#' It is by default 'radius_mean', which records the mean of
#' breast tissue radius
#' @return The histogram of the column from the WDBC data
#' with corresponding sizes
#' @importFrom dplyr select
#' @examples
#' data(data)
#' histogram(data, 'radius_mean')
#' @export
histogram <- function(x = data, col = "radius_mean") {
  if (col %in% colnames(x)) {
  column <- x |>
    select(col) |>
    unlist()

  hist(column, labels = TRUE, main = paste("Histogram of", col))}else {
    stop("Please enter a column from the data!")
  }
}
