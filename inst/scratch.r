library(devtools)
library(lubridate)
library(dplyr)
library(ggplot2)

document()
document()

data(ukb_accel)

dt <- first(ukb_accel$time)

ukb_accel |>
  filter(time >= dt  & time < dt + minutes(5)) |>
#  (\(x) x[seq(1, nrow(x), by =100),])() |>
# e.g. 5 |> (\(x) x + 1)()  gives 6

  accel_plot() +
    theme_minimal() +
    xlab("Time") +
    ylab("Acceleration (in gravities)")

ukb_accel |>
  filter(time >= dt & time < dt + minutes(5)) |>
  spectral_signature(take_log = TRUE) |>
 # filter(freq <= 10) |>
  accel_plot()
