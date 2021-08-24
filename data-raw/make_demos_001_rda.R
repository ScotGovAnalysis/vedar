library(vedar)
library(dplyr)
library(purrr)
filename_base <- "DemoS_001"
demos_001 <- prep_data(filename_base = filename_base, vignette = T)
usethis::use_data(demos_001)
