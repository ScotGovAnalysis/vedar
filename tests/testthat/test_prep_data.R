library(vedar)

testthat::test_that("file structure tests work", {

  filename_base <- "../data/DemoS_001"
  testthat::expect_true(vd_structure_match_expected(
    paste(filename_base,".vd", sep = ""), "vd_file"))
  testthat::expect_false(vd_structure_match_expected(
    paste(filename_base,"_mod.vd", sep = ""), "vd_file"))

  testthat::expect_true(vd_structure_match_expected(
    paste(filename_base,".vde",  sep = ""), "vde_file"))
  testthat::expect_false(vd_structure_match_expected(
    paste(filename_base,"_mod.vde",  sep = ""), "vde_file"))

  testthat::expect_true(vd_structure_match_expected(
    paste(filename_base,".vds",  sep = ""), "vds_file"))
  testthat::expect_false(vd_structure_match_expected(
    paste(filename_base,"_mod.vds",  sep = ""), "vds_file"))
  })

testthat::test_that("append_sets returns list of sets", {
  dat <- tibble::tibble(commodity = c("A", "B"),
                     attribute = c("var1", "var1"),
                     pv = rep(1,2))
  sets_dat <- tibble::tibble(object = rep("commodity", 2),
                             variable = c("A", "B"),
                             set = rep(
                               list(c("set1", "set2")), 2)
  )
  sets_in_sets_list <- function(sets_list_dat, sets_list_ref){
    sum(sets_list_dat %in% sets_list_ref) == length(sets_list_dat)
  }


  append_sets_out <- append_sets(dat, sets_dat, "commodity")
  testthat::expect_type(append_sets_out[["commodity_set"]], "list")
  testthat::expect_true(sets_in_sets_list(sets_list_dat =
                                  unique(unlist(
                                    append_sets_out[["commodity_set"]])),
                                sets_list_ref =
                                  unique(unlist(sets_dat$set))
  ))

})
