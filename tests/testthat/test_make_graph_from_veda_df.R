library(igraph)
library(tidyverse)
#demos_001 has many to many edges so used for tests
load("../../data/demos_001_sector.Rda")

g_001 <- demos_001_sector %>%
  dplyr::filter(period == 2006) %>%
  make_graph_from_veda_df(node_labels = process,
                          edge_labels = commodity
  )

match_weights_001 <- c(5953.5635,
                       5595.2974,
                       1865.0991,
                       509.1075,
                       478.4711,
                       159.4904,
                       13413.9600)

load("../../data/demos_007.Rda")
load("../../data/demos_001_vdt.Rda")

testthat::test_that("Computed edge values agree with data", {
 testthat::expect_equivalent(E(g_001)$weight, match_weights_001 )
})

testthat::test_that("Correct number of edges computed", {
  testthat::expect_equal(length(E(g_001)), 7)
})

testthat::test_that("make_graph_from_veda_df returns a list", {
  # would return string if stop conditions activated
  testthat::expect_type(g_001, "list")
})

testthat::test_that("edge attributes are c('weight',
                    'commodity',  'commodity_description')", {
  testthat::expect_equal(edge_attr_names(g_001),
                         c('weight',
                           'commodity',
                           'commodity_description'))
})

testthat::test_that("multiple region input data returns error", {
  #demos_007 is multiregion model
  testthat::expect_error(make_graph_from_veda_df(demos_007,
                                                node_labels =
                                                  process,
                                                edge_labels =
                                                  commodity))
                    })

testthat::test_that("correct graph structure returned for vdt data", {
   # The expected_edges for demos_001 were manually checked
   # Copied below
   expected_edges <- tribble(
    ~from, ~to, ~weight, ~commodity, ~commodity_description,
    "mincoa2",	"dtpscoa",	1,	"coa", "solid fuels",
    "mincoa3",	"dtpscoa",	1,	"coa", "solid fuels",
    "impnrgz",	"dtpscoa",	1,	"coa", "solid fuels",
    "mincoa1",	"dtpscoa",	1,	"coa", "solid fuels",
    "impcoa1",	"dtpscoa",	1,	"coa", "solid fuels",
    "mincoa2",	"expcoa1",	1,	"coa", "solid fuels",
    "mincoa3",	"expcoa1",	1,	"coa", "solid fuels",
    "impnrgz",	"expcoa1",	1,	"coa", "solid fuels",
    "mincoa1",	"expcoa1",	1,	"coa", "solid fuels",
    "impcoa1",	"expcoa1",	1,	"coa", "solid fuels",
    "dtpscoa",	"tpscoa_end_process",	1,	"tpscoa",	"demand total primary supply - coa",
    "impdemz",	"tpscoa_end_process",	1, "tpscoa", "demand total primary supply - coa"
    ) %>% as_tibble()
  g_001 <- make_graph_from_veda_df(demos_001_vdt %>%
                                 filter(region == "reg1"),
                                 input_data_type = "vdt",
                                 node_labels = "process")
  computed_edges <- igraph::as_data_frame(g_001, what = "edges")
  testthat::expect_true(all(computed_edges == expected_edges))
})




