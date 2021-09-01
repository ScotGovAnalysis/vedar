
#' @export
syscost <- function(dat){
  if("reg_obj" %in% dat$attribute == F){
    stop("Check data. Missing attribute `reg_obj'")
  }
  if("region" %in% names(dat) == F){
    stop("Check data. Missing column: `region`")
  }

  out <- dat %>%
    filter(attribute == "reg_obj")

  if(nrow(out) !=
     unique((demos_007 %>% filter(is.na(region) == F))$region)){
    stop("Data issue: The number of regions does not equal
         the reg_obj attributes.")
  }
  out <- out
    group_by(region) %>%
    summarise(syscost = pv)

}
