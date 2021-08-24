
# Import and prep vd files
#
# ccp <- prep_data(ccpu_filename_base) %>%
#     mutate(case = "ccpu")
#
####################################################
#' export
prep_data <- function(filename_base,
                      use_sector_def_strings = F,
                      vignette = F
                      ){
  vd_file <- paste(filename_base, ".VD", sep = "")
  vde_file <- paste(filename_base, ".VDE", sep = "")
  vds_file <- paste(filename_base, ".VDS", sep = "")

  if(vignette == F){
    dat <- import_vd(vd_file) %>%
      standardise_vd_dat() %>%
      dplyr::mutate(timeslice = fix_timeslice(timeslice))


  descriptions <- import_vde(vde_file) %>%
    standardise_vd_dat()  %>%
    dplyr::select(-region) %>%
    unique() %>%
    dplyr::group_by(variable,  object) %>%
    # in case a variable entry has more than one description
    dplyr::summarise(description = paste(description)) %>%
    dplyr::ungroup()

  sets <- import_vds(vds_file) %>%
    standardise_vd_dat() %>%
    # a single variable may be a member of more than one set.
    # Reduce dimension of sets by creating a set of sets
    # for each variable.
    #  This is needed to ensure that rows are not repeated
    #   when sets are joined to dat
    dplyr::select(-region) %>%
    dplyr::group_by(variable,  object) %>%
    dplyr::summarise(set = list(set))  %>%
    dplyr::ungroup()
  }
  else{
    vd_file <- paste(filename_base, ".VD", sep = "")
    vde_file <- paste(filename_base, ".VDE", sep = "")
    vds_file <- paste(filename_base, ".VDS", sep = "")


    dat <- import_vd(system.file("extdata",
                                 vd_filename,
                                 package = "vedar")) %>%
      standardise_vd_dat() %>%
      dplyr::mutate(timeslice = fix_timeslice(timeslice))

    descriptions <- import_vde(system.file("extdata",
                                           vde_filename,
                                           package = "vedar")) %>%
      standardise_vd_dat()  %>%
      dplyr::select(-region) %>%
      unique() %>%
      dplyr::group_by(variable,  object) %>%
      # in case a variable entry has more than one description
      dplyr::summarise(description = paste(description)) %>%
      dplyr::ungroup()

    sets <- import_vds(system.file("extdata",
                                   vds_filename,
                                   package = "vedar")) %>%
      standardise_vd_dat() %>%
      # a single variable may be a member of more than one set.
      # Reduce dimension of sets by creating a set of sets
      # for each variable.
      #  This is needed to ensure that rows are not repeated
      #   when sets are joined to dat
      dplyr::select(-region) %>%
      dplyr::group_by(variable,  object) %>%
      dplyr::summarise(set = list(set))  %>%
      dplyr::ungroup()



  }


  #append descriptions
  # to be converted to a function
  dat <- dat %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "commodity") %>%
                       dplyr::select(variable, description) %>%
                       dplyr::rename(commodity = variable),
              by = "commodity") %>%
    dplyr::rename(commodity_description = description) %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "process") %>%
                       dplyr::select(variable, description) %>%
                       dplyr::rename(process = variable),
              by = "process") %>%
    dplyr::rename(process_description = description) %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "userconstraint") %>%
                       dplyr::select(variable, description) %>%
                       dplyr::rename(userconstraint = variable),
              by = "userconstraint") %>%
    dplyr::rename(userconstraint_description = description )




    for(o in c("commodity", "process", "userconstraint")){
    dat <- append_sets(dat, sets, o)
  }

  dat
  }


###################################
#' export
import_vd <- function(file, dat_row_skip = 13, dim_row = 4){
  dat <- utils::read.table(file,
                    sep = ",",
                    skip = dat_row_skip,
                    header = F)



  col_names <- utils::read.table(file,
                          skip = dim_row-1,
                          nrow = 1,
                          sep = ";",
                          header = F) %>%
    tidyr::separate(as.character("V1"),
             into = c("drop", "V1"),
             sep = "- ") %>%
    dplyr::select(-drop) %>%
    dplyr::mutate_all(stringr::str_to_lower)

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
  desc <- utils::read.csv(file, header = F, sep = ",")
  names(desc) <- c("object", "region", "variable", "description")
  desc
}
###################################
#' export
import_vds <- function(file){
  set <- utils::read.csv(file, header = F, sep = ",")
  names(set) <- c("object", "region", "set", "variable")


  set
}


###########################################
standardise_vd_dat <- function(dat){
  if("vintage" %in% names(dat)){
    # dat <- dat %>%
    #   dplyr::select(-vintage)
  }
  if("period" %in% names(dat)){
    dat <- dat %>%
      dplyr::mutate(period = ifelse(grepl("^[0-9]", period), period, NA),
                    period = as.numeric(as.character(period)),
                    vintage = ifelse(grepl("^[0-9]", vintage), vintage, NA),
                    vintage = as.numeric(as.character(vintage))
      )
  }

  dat  %>%
    # convert all strings to lower numeric
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.character, stringr::str_to_lower)


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
define_sector_from_string_subfunctio <- function(variable, sector_def_var){
  if(sector_def_var == "code"){
    sector <- dplyr::case_when(
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

  obj_col <- rlang::enquo(obj)
  # obj_nm <- as_name(obj)

  sets_dat <- sets_dat %>%
    dplyr::filter(object == obj) %>%
    dplyr::rename(!!rlang::sym(obj) := variable) %>% #sym converts obj string to symbol and unquote with !!
    dplyr::select(-object)

  dat %>%
    dplyr::left_join(sets_dat, by = obj) %>%
    dplyr::rename(!!paste(obj, "_set", sep = "") := set)
}

##########################################
#' export
prep_sector_dat <- function(sector_dat){
  # replace spaces in col names with _
  names(sector_dat) <- sub(" ", "_", names(sector_dat))


  sector_dat %>%
    #strings to lower
    dplyr::mutate_if(is.character, stringr::str_to_lower) %>%
    #column names to lower
    dplyr::rename_with(tolower)




}
##################################
#' Define sectors in imported veda data from list
#'
#' Append sector information from a tibble to tibble output from
#' \code{import_vd()} or \code{prep_data()}. The function joins the sector
#'  information based on a joining variable
#'
#' @param dat A tibble of veda data from import_vd or prep_data
#' @param join_variable_name A string column name in dat for joining to sector_dat
#' export
#' @param sector_dat A tibble containing the sector information for variables
#' @param sector_info_col Column in sector_dat containing sector information
#' @param sector_dat_variable_col Column in sector_dat for joining to dat
#' @examples
#' test_dat <- tibble::tibble(attribute = "var_fout",
#'              commodity = "adistelc00",
#'              process = "adistelc00" ,
#'              period = NA,
#'              vintage = NA,
#'              timeslice = "annual",
#'              region = "reg1",
#'              userconstraint = "-",
#'              pv = 10,
#'              commodity_description = NA,
#'              process_description = NA,
#'              usercostraint_description = NA,
#'              commodity_set = NA,
#'              process_set = NA,
#'              usercostraint_set = NA)
#'
#'sector_dat <- tibble::tibble(major_sector = "agr",
#'                           process = "adistlec00")
#'
#' t <- define_sector_from_list(test_dat, "process", sector_dat,
#'                             major_sector, process)
#'
#' export
define_sector_from_list <- function(dat,
                                    join_variable_name,
                                    sector_dat,
                                    sector_info_column,
                                    sector_dat_join_variable_col){

  # join_variable_name is a string column name

  sector_info_column <- rlang::enquo(sector_info_column)
  sector_dat_join_variable_col <- rlang::enquo(sector_dat_join_variable_col)
  #convert enquoed col to string. enquo introduces ~. remove with sub
  sector_info_column_name <- sub("~", replacement = "",
                            deparse(substitute(sector_info_column)))

  # select the sector column data to append
  sector_dat <- sector_dat %>%
    dplyr::select(!!sector_info_column, !!sector_dat_join_variable_col) %>%
    dplyr::rename(!!(join_variable_name) := !!sector_dat_join_variable_col)

  dat <- dat %>%
    dplyr::left_join(sector_dat, by = join_variable_name) %>%
    dplyr::rename(!!(paste(join_variable_name,
                           sector_info_column_name,
                           sep = "_")) := !!sector_info_column)

  dat
}


#################
#' Define sectors based on string matches
#'
#' Use specified regular expressions for sector definitions based on naming
#' conventions used for Scottish Times Model
#'
#' @param dat A tibble of veda data from import_vd or prep_data
#'
#' @example
#' data(demos_001)
#' demos_001 %>%
#'     define_sector_from_string
#'
#'export
define_sector_from_string <- function(dat){

    dat %>%
      dplyr::mutate(sector = define_sector_from_string_subfunction(process,
                                                                   "code"),
                    sector = dplyr::if_else(
                      sector == "" | is.null(sector),
                      define_sector(commodity,"code"), sector))
}
