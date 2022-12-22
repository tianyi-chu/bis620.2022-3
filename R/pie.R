#' function 1: pie chart
#' Plot Pie chart for the Breast Cancer Wisconsin (Diagnostic) Data Set (WDBC)
#' This function plots WDBC data.
#' @param x a dataframe. It is by default the WDBC data, which
#' has a diagnosis column and multiple columns with breast
#' tissue characteristics.
#' @param colname a column name input as character.
#' @param eq a builtin condition such as less than (<) or more than (>)
#' @param rhs the right-hand-side of the condition; used to subset the data
#' @return a pie plot showing the number of benign/malignant diagnosis
#' in the subsetted data
#' @importFrom dplyr group_by summarise arrange mutate n
#' @importFrom ggplot2 aes ggplot geom_text facet_grid geom_bar
#' coord_polar theme_void ggtitle
#' @examples
#' data(data)
#' pie(data, 'diagnosis',`!=`,2)
#' @export
pie <- function(x = data, colname = "diagnosis", eq = `!=`, rhs = 2) {
  if (typeof(eq) != "builtin") {
    stop("Please input eq as built-in conditions, e.g., eq = `==`")
  }
  if (colname %in% colnames(x)) {
    rows <- which(`eq`(as.vector(unlist(data[, colname])), rhs))
    if (length(rows) == 0) {
      stop("Subset contains no data: please input another condition!")
    }
    pie_data <- x[rows, ] |>
      group_by(diagnosis) |>
      summarise(n = n())
    pie_pos <- pie_data |>
      arrange(desc(n)) |>
      mutate(prop = n / sum(pie_data$n) * 100) |>
      mutate(ypos = cumsum(prop)- 0.5 * prop)
  } else {
    stop("Please input a column name!")
  }

  ggplot(pie_pos, aes(x = "", y = prop, fill = diagnosis)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(aes(y = ypos, label = n), color = "white", size = 6) +
    ggtitle("Number of benign/malignant diagnosis in the requested subset")
}
