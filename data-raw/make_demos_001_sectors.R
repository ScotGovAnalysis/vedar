library(vedar)
library(dplyr)
library(purrr)
data("demos_001")
sectors_commodity_file <- "sectors_commodity.csv"

sectors_process_file <- "sectors_process.csv"
sectors_commodity <- system.file("extdata",
                                 sectors_commodity_file,
                                 package = "vedar") %>%
  read.csv()
sectors_process <- system.file("extdata",
                               sectors_process_file,
                               package = "vedar") %>%
  read.csv()

demos_001_sector <- demos_001 %>%
  #assign the sector to the commodity
  define_sector_from_list(join_variable_name = "commodity",
                          sector_dat = sectors_commodity,
                          sector_info_column = sector,
                          sector_dat_join_variable_col = commodity) %>%
  #assign the sector to the process
  define_sector_from_list(join_variable_name = "process",
                          sector_dat = sectors_process,
                          sector_info_column = sector,
                          sector_dat_join_variable_col = process) %>%
  # here, assign a sector based on the process sector if available,
  # else, use the commodity information
  mutate(sector = if_else(is.na(process_sector),
                          commodity_sector,
                          process_sector),
         #create a variable to check that commodity sector and process
         # sector information match
         sector_match = if_else((is.na(process_sector) |
                                   is.na(commodity_sector) |
                                   process_sector == commodity_sector),
                                T, F))

usethis::use_data(demos_001_sector, overwrite = T)
