test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data(ukb_accel)
    p <- accel_plot(ukb_accel[1:100, ])
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The accel_plot() errors when no time or freq column.",
  {
    data(iris)
    expect_error(accel_plot(iris))
  }
)

test_that(
  "The accel_plot() is correct for time-series data.",
  {
    data(ukb_accel)
    p <- accel_plot(ukb_accel[1:100, ])
    vdiffr::expect_doppelganger("first-100-samples", p)
  }
)

test_that(
  "Testing spectral_signature with take_log = FALSE.",
  {
    data(ukb_accel)
    p <- spectral_signature(ukb_accel) |> accel_plot()
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "Testing spectral_signature with take_log = TRUE.",
  {
    data(ukb_accel)
    p <- spectral_signature(ukb_accel, take_log = TRUE) |> accel_plot()
    expect_true(inherits(p, "gg"))
  }
)
