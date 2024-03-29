#' Extract the undiscounted system cost
#'
#' Extract undiscounted system cost as the reg_obj variable for each region
#'
#' @param dat Tibble of veda output data from prep_data with or without
#' additional variables
#' @return Tibble (n(regions) x 2): region, syscost
#' @export
syscost <- function(dat){
  if("reg_obj" %in% dat$attribute == F){
    stop("Check data. Missing attribute `reg_obj'")
  }
  if("region" %in% names(dat) == F){
    stop("Check data. Missing column: `region`")
  }

  out <- dat %>%
    dplyr::filter(attribute == "reg_obj")

  if(all(unique(out$region) !=
     unique((demos_007 %>% filter(is.na(region) == F))$region))){
    stop("syscost error: The number of regions does not equal
         the reg_obj attributes.")
  }
  out <- out %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(syscost = pv) %>%
    ungroup()

}
