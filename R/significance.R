#' function name: accuracy
#' Returns the accuracy of the glm model which is fitted using a
#' subset of the WDBC data.
#' @param x a dataframe. It is by default the WDBC data, which
#' has a diagnosis column and multiple columns with breast
#' tissue characteristics.
#' for example, radius_mean records the mean of breast tissue radius
#' @param subset a column name suffix input as character.
#' It is by default 'mean', which subsets all columns recording the mean
#' of breast tissue characteristics such as radius_mean.
#' @return The summary table of the glm model, which is fitted using
#' the subset of the WDBC data containing all columns with suffix 'subset'.
#' @importFrom dplyr group_by summarise select mutate if_else
#' @examples
#' data(data)
#' significance(data, "mean")
#' @export
significance <- function(x = data, subset = "mean") {
  options(warn = -1)
  # separate data
  data <- x |> mutate(diagnosis = if_else(diagnosis == "B", 1, 0)) |>
    mutate(diagnosis = factor(diagnosis))|>
    select(!id)
  # if diagnosis = 'B' then it gets a value of 1; gets 0 otherwise

  num_train <- round(nrow(data) * 0.8)
  num_test <- nrow(data) - num_train
  split <- sample(c(rep(1, num_train), rep(0, num_test)))

  subset_cols <- colnames(data)[grepl(subset, colnames(data))]
  if (length(subset_cols) == 0) {
    stop("Please enter a column from data!")
  }
  subset_data <- cbind(data[1], data[subset_cols])
  subset_train <- subset(subset_data, split == 1)

  lm_subset <- glm(diagnosis ~ .,
                   data = subset_train,
                   family = binomial(link = "logit"))
  summary(lm_subset)
}
