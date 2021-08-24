#' Exported results tables of DemoS_001 runs in Veda2
#'
#' A list object in which each list corresponds to one of the csvs
#' created by the batch export function in Veda2
#'
#' @format A list of 8 elements:
#' \describe{
#'     \item{All costs.csv}{Tibble (13 x 12) cost_* attributes for coal processes}
#'     \item{Annual technology costs.csv}{Tibble (4 x 11) Cost_FOM, Cost_Inv
#'     for Process == "DTPSCOA"}
#'     \item{Demands.csv}{Tibble (2 x 13) VAR_FOut demand commodities TPSCOA}
#'     \item{Fuel Supply.csv}{Tibble (6 x 13) VAR_FOut for IMPCOA and MINCOA
#'     processes}
#'     \item{Prices_All.csv}{Tibble (4 x 10) EQ_CombalM for DTPCOA and COA}
#'     \item{Prices_Energy.csv}{Tibble (2 x 11) EQ_CombalM for COA}
#'     \item{Process Marginals.csv}{Tibble (11 x 11) VAR_ActM for processes}
#'     \item{_SysCost.csv}{Tibble (1 x 7) Reg_obj}
#'
#' }
"demos_001_results"

#' Results from demos_001 run created by prep_data()
#'
#' Results data from the DemoS_001 model in veda 2 using demos_001 run.
#' Dataset containing all attributes, commodities and processes#'
#'
#' @format Tibble with 117 rows and 15 columns:
#' \describe{
#'     \item{attribute}{attribute name as lower case string}
#'     \item{commodity}{commodity name as lower case string}
#'     \item{process}{process name as lower case string}
#'     \item{period}{model period as numeric}
#'     \item{region}{region name as lower case string}
#'     \item{vintage}{model vintage as numeric}
#'     \item{timeslice}{timeslice as string. Missing data converted to
#'     annual}
#'     \item{usercontraint}{userconstraint name as lower case string}
#'     \item{pv}{numeric value for the datum}
#'     \item{commodity_description}{Imported commodity description as
#'     lower case string}
#'      \item{process_description}{Imported process description as
#'     lower case string}
#'      \item{userconstraint_description}{Imported userconstraint description as
#'     lower case string}
#'      \item{commodity_set}{Imported commodity sets as
#'     lower list object}
#'     \item{process_set}{Imported process sets as
#'     lower list object}
#'     \item{userconstriant_set}{Imported userconstraint sets as
#'     lower list object}
#' }
"demos_001"
