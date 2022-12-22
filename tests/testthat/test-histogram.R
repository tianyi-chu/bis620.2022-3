test_that(
  "The histogram() returns a histogram",
  {
    data(data)
    expect_true(inherits(histogram(), "histogram"))
  }
)

test_that(
  "The histogram() errors when the input column is not from the dataset",
  {
    data(data)
    expect_error(histogram(data, "wrong"))}
)
