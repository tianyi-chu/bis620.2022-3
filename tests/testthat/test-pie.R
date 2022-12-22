test_that(
  "The pie() returns a ggplot object.",
  {
    data(data)
    p <- pie(data)
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The pie() errors when input column is not in the dataset.",
  {
    data(iris)
    expect_error(pie(iris, diagnosis))
  }
)

test_that(
  "The pie() errors when the input condition is not correct.",
  {
    data(data)
    expect_error(pie(data, eq = 2))
  }
)

test_that(
  "The pie() errors when the input condition returns no data.",
  {
    data(data)
    p <- pie()
    expect_error(pie(data, eq = `<`, rhs = -1))
  }
)
