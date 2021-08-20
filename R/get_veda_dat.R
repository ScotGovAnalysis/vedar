
# Import and prep vd files
#
# ccp <- prep_data(ccpu_filename_base) %>%
#     mutate(case = "ccpu")
#
####################################################
#' export
prep_data <- function(filename_base){
  vd_file <- paste(filename_base, ".VD", sep = "")
  vde_file <- paste(filename_base, ".VDE", sep = "")
  vds_file <- paste(filename_base, ".VDS", sep = "")


  dat <- import_vd(vd_file) %>%
    standardise_vd_dat() %>%
    mutate(timeslice = fix_timeslice(timeslice))

  descriptions <- import_vde(vde_file) %>%
    standardise_vd_dat()  %>%
    select(-region) %>%
    unique() %>%
    group_by(variable,  object) %>%
    # in case a variable entry has more than one description
    summarise(description = paste(description))

  sets <- import_vds(vds_file) %>%
    standardise_vd_dat() %>%
    # a single variable may be a member of more than one set.
    # Reduce dimension of sets by creating a set of sets
    # for each variable.
    #  This is needed to ensure that rows are not repeated
    #   when sets are joined to dat
    select(-region) %>%
    group_by(variable,  object) %>%
    summarise(set = list(set))


  #append descriptions
  # to be converted to a function
  dat <- dat %>%
    left_join(descriptions %>%
                filter(object == "commodity") %>%
                select(variable, description) %>%
                rename(commodity = variable),
              by = "commodity") %>%
    rename(commodity_description = description) %>%
    left_join(descriptions %>%
                filter(object == "process") %>%
                select(variable, description) %>%
                rename(process = variable),
              by = "process") %>%
    rename(process_description = description) %>%
    left_join(descriptions %>%
                filter(object == "userconstraint") %>%
                select(variable, description) %>%
                rename(userconstraint = variable),
              by = "userconstraint") %>%
    rename(userconstraint_description = description ) %>%
    # append sector information
    mutate(sector = define_sector(process, "code"),
           sector = if_else(sector == "" | is.null(sector),
                            define_sector(commodity,
                                          "code"),
                            sector))


  for(o in c("commodity", "process", "userconstraint")){
    dat <- append_sets(dat, sets, o)

  }


  dat %>%
    # add indicator for infrastructure/non-infrastructure processes
    mutate(
      infrastructure =
        if_else(
          grepl(
            "(infrastructure)|(distribution)|(distn)",
            process_description),
          "infrastructure",
          "non_infrastructure"))
}


###################################
#' export
import_vd <- function(file, dat_row_skip = 13, dim_row = 4){
  dat <- read.table(file,
                    sep = ",",
                    skip = dat_row_skip,
                    header = F)



  col_names <- read.table(file,
                          skip = dim_row-1,
                          nrow = 1,
                          sep = ";",
                          header = F) %>%
    separate(as.character("V1"),
             into = c("drop", "V1"),
             sep = "- ") %>%
    select(-drop) %>%
    mutate_all(str_to_lower)

  dat <- add_col_names(dat, col_names)

  dat


}


###################################
add_col_names <- function(dat, col_names){

  col_names <- unlist(as.list(col_names)) %>%
    unname

  names(dat) <- col_names

  dat

}

###################################
#' export
import_vde <- function(file){
  desc <- read.csv(file, header = F, sep = ",")
  names(desc) <- c("object", "region", "variable", "description")
  desc
}
###################################
#' export
import_vds <- function(file){
  set <- read.csv(file, header = F, sep = ",")
  names(set) <- c("object", "region", "set", "variable")


  set
}


###########################################
standardise_vd_dat <- function(dat){
  if("vintage" %in% names(dat)){
    # dat <- dat %>%
    #   select(-vintage)
  }
  if("period" %in% names(dat)){
    dat <- dat %>%
      mutate(period_date = as.Date(
        paste(as.character(period), "/01/01", sep = "")),
        period = as.numeric(as.character(period))
      )
  }

  dat  %>%
    # convert all strings to lower numeric
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, str_to_lower)
  # convert null or missing timeslice specification to "annual"
  #  mutate(timeslice = fix_timeslice(timeslice))

}

##################################
fix_timeslice <- function(timeslice){
  # a separate function was needed as grepl in the mutate resulted in
  #  'object not found' error when running in function but not out of function
  #  https://stackoverflow.com/questions/27773881/r-object-not-found-if-defined-within-a-function-when-using-data-table-dplyr
  #  Suggests scoping issue so fix with subfunction working on vector
  #
  timeslice_fixed = ifelse((grepl("(-)|(none)", timeslice)|is.na(timeslice)),
                           "annual",
                           timeslice)
  timeslice_fixed
}
###################################
define_sector <- function(variable, sector_def_var){
  if(sector_def_var == "code"){
    sector <- case_when(
      grepl("^([Aa](?!(ct)))[A-Za-z0-9]|(ghg-agr)",
            variable,
            perl = T) ~ "agriculture",
      grepl("^([Ee](?!x))[A-Za-z0-9]|(ghg-elc)",
            variable,
            perl = T) ~  "electricity",
      grepl("^([Ii](?!(mp)))[A-Za-z0-9]|(ghg-ind)",
            variable,
            perl = T) ~ "industry",
      grepl("^([u](?!(rn)))[a-z0-9]",
            variable,
            perl = T) ~ "industry",
      grepl("(ghg-fsup-ets)",
            variable,
            perl = T) ~ "industry",
      grepl("(ghg-other)",
            variable,
            perl = T) ~ "industry",
      grepl("(ghg-fsup-non-ets)",
            variable,
            perl = T) ~ "waste",
      grepl("^(pw)[A-Za-z0-9]",
            variable,
            perl = T) ~ "waste",
      grepl("^(land)|(luco2)|(dumlu)",
            variable,
            perl = T) ~ "land",
      grepl("^[p](?!w)[A-Za-z0-9]|(ghg-prc)",
            variable,
            perl = T) ~ "industry",
      grepl("^(urn)",
            variable,
            perl = T) ~ "resources",
      grepl("^(ccs)|(co2seq)",
            variable,
            perl = T) ~ "industry",
      grepl("^([Rr](?!(nw)))[A-Za-z0-9]|(ghg-res)",
            variable,
            perl = T) ~ "residential",
      grepl("^(exp)|(imp)|(min)|(rnw)", #starts with any of these strings
            variable,
            perl = T) ~ "resources",
      grepl("^[Ss][A-Za-z0-9]|(ghg-ser)",
            variable,
            perl = T) ~ "services",
      grepl("^[Tt][A-Za-z0-9]|(ghg-tra)",
            variable,
            perl = T) ~ "transport",
      grepl("(magic)|(target-missed)",
            variable,
            perl = T) ~ "target missed",
      grepl("(ghg-ggr)",
            variable,
            perl = T) ~ "industry",
      grepl("(ghg-ias)",
            variable,
            perl = T) ~ "transport",
      TRUE ~ ""
    )
  }else if(sector_def_var == "description"){
    #split the description on the first "." and use the returned first column
    sector = str_split_fixed(variable, "[.]", n = 2)[,1] # "[.]" is needed as regex
  }

  sector

}

###############################
append_sets <- function(dat, sets_dat, obj){
  #sets_dat is a long df which includes data for commodity, process, userconstraint.
  #The sets for each will be added as a column to dat

  obj_col <- enquo(obj)
  # obj_nm <- as_name(obj)

  sets_dat <- sets_dat %>%
    filter(object == obj) %>%
    rename(!!sym(obj) := variable) %>% #sym converts obj string to symbol and unquote with !!
    select(-object)

  dat %>%
    left_join(sets_dat, by = obj) %>%
    rename(!!paste(obj, "_set", sep = "") := set)
}
