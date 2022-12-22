test_that(
  "The accuracy() errors when input column suffix is not contained 
in any column of the dataset.",
  {
    data(data)
    expect_error(accuracy(data, "wrong column"))
  }
)

test_that(
  "The accuracy() errors when diagnosis is not in the dataset.",
  {
    data(iris)
    expect_error(accuracy(iris, diagnosis))
  }
)

test_that(
  "The accuracy() runs correctly for WDBC data.",
  {
    data(data)
    p <- accuracy()
    expect_true(grepl("Accuracy =", p))
  }
)
