library(vedar)
library(tidyverse)

demos_001_vdt <- prep_vdt_dat("demos_001", refer_to_package_data = T)

usethis::use_data(demos_001_vdt, overwrite = T)

demos_007_vdt <- prep_vdt_dat("demos_007", refer_to_package_data = T)

usethis::use_data(demos_007_vdt, overwrite = T)
