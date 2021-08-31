library(tidyverse)

demos_files <- c("All costs.csv",
                 "Demands.csv",
                 "Process Marginals.csv",
                 "_SysCost.csv")
path <- "tests/data/demos_007/"

demos_007_results <- map(.x = paste(path, demos_files, sep = ""), ~read.csv(.x))
names(demos_001_results) <- demos_files
usethis::use_data(demos_007_results, overwrite = T)
