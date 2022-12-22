## code to prepare `DATASET` dataset goes here

data <- readRDS("data.rds")
usethis::use_data(data, overwrite = TRUE)
