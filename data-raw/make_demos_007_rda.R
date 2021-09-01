library(vedar)
library(dplyr)
library(purrr)
filename_base <- "DemoS_007"
demos_007 <- prep_data(filename_base = filename_base, vignette = T)
usethis::use_data(demos_007, overwrite = T)
