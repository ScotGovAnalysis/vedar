library(tidyverse)

demos_files <- c("All costs.csv",
                 "Annual technology costs.csv",
                 "Demands.csv",
                 "Fuel Supply.csv",
                 "Prices_All.csv",
                 "Prices_Energy.csv",
                 "Process Marginals.csv",
                 "_SysCost.csv")
path <- "tests/data/"

demos_001_results <- map(.x = paste(path, demos_files, sep = ""), ~read.csv(.x))
names(demos_001_results) <- demos_files
usethis::use_data(demos_001_results, overwrite = T)
