testthat::test_that("prep_sector_dat() returns strings in lower case",
                    {
                      sector_dat <- tibble::tribble(~sector, ~val,
                                                    "Reg1", 1,
                                                    "REG2", 2,
                                                    "reg3", 3)
                      testthat::expect_equal(
                        prep_sector_dat(sector_dat)$sector, c("reg1",
                                                              "reg2",
                                                              "reg3"))

                    })
