test_that(
  "The significance() errors when input column is not in the dataset.",
  {
    data(data)
    expect_error(significance(data, "wrong column"))
  }
)

test_that(
  "The significance() runs normally for WDBC data.",
  {
    data(data)
    expect_true(inherits(significance(), "summary.glm"))
  }
)
