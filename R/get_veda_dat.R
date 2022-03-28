
#' Import output from veda runs into R.
#'
#' Imports and combines vd, vde and vds files to create a
#'  single tibble with standard formatting
#
#' @param filename_base a character string of the veda files without the .vd* suffix
#' @param vignette logical. Set to TRUE if running from within the vignette or development environmnet.
#'
#' @examples
#'   path_to_data <- paste0(find.package("vedar"),
#'                          "/extdata/")
#'   filename_base <- paste0(path_to_data, "DemoS_001")
#'
#'   dat <- prep_data(filename_base)
#
####################################################
#' @export
prep_data <- function(filename_base,
                      vignette = F
                      ){
  vd_file <- paste(filename_base, ".VD", sep = "")
  vde_file <- paste(filename_base, ".VDE", sep = "")
  vds_file <- paste(filename_base, ".VDS", sep = "")

  if(vignette == T){
    vd_file <- system.file("extdata",
                           vd_file,
                           package = "vedar")
    vde_file <- system.file("extdata",
                           vde_file,
                           package = "vedar")
    vds_file <- system.file("extdata",
                           vds_file,
                           package = "vedar")
  }

  if(vd_structure_match_expected(vd_file, "vd_file")){
    dat <- import_vd(vd_file) %>%
      standardise_vd_dat() %>%
      dplyr::mutate(timeslice = fix_timeslice(timeslice))
  }else{
    stop("vd file structure does not match expected")
  }

  length_data <- nrow(dat)

  if(vd_structure_match_expected(vde_file, "vde_file")){
    descriptions <- import_vde(vde_file) %>%
      standardise_vd_dat()  %>%
      unique() %>%
      dplyr::group_by(variable,  object, region) %>%
      # in case a variable entry has more than one description
      dplyr::summarise(description = paste(description)) %>%
      dplyr::ungroup()
  }else{
    stop("vde file structure does not match expected")
}

  if(vd_structure_match_expected(vds_file, "vds_file")){
  sets <- import_vds(vds_file) %>%
    standardise_vd_dat() %>%
    # a single variable may be a member of more than one set.
    # Reduce dimension of sets by creating a set of sets
    # for each variable.
    #  This is needed to ensure that rows are not repeated
    #   when sets are joined to dat
    dplyr::group_by(variable,  object, region) %>%
    dplyr::summarise(set = list(set))  %>%
    dplyr::ungroup()
  }else{
    stop("vds file structure does not match expected")
  }

  #append descriptions
  # to be converted to a function
  dat <- dat %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "commodity") %>%
                       dplyr::select(variable, description, region) %>%
                       dplyr::rename(commodity = variable) %>%
                       dplyr::filter(is.na(commodity) == F),
              by = c("commodity", "region")) %>%
    dplyr::rename(commodity_description = description) %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "process") %>%
                       dplyr::select(variable, description, region) %>%
                       dplyr::rename(process = variable) %>%
                       dplyr::filter(is.na(process) == F),
              by = c("process", "region")) %>%
    dplyr::rename(process_description = description) %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "userconstraint") %>%
                       dplyr::select(variable, description, region) %>%
                       dplyr::rename(userconstraint = variable)%>%
                       dplyr::filter(is.na(userconstraint) == F),
              by = c("userconstraint", "region")) %>%
    dplyr::rename(userconstraint_description = description )




    for(o in c("commodity", "process", "userconstraint")){
      dat <- append_sets(dat, sets, o)
    }
  if(nrow(dat) != length_data){
    stop("data length has changed in prep_data! Check prep_data code!")
  }
  dat
  }


###################################
#' @export
import_vd <- function(file, dat_row_skip = 13, dim_row = 4){
  dat <- utils::read.table(file,
                    sep = ",",
                    skip = dat_row_skip,
                    header = F,
                    colClasses = c(rep("character", 8),
                                   "numeric"))



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
#' @export
import_vde <- function(file){
  desc <- utils::read.csv(file, header = F, sep = ",")
  names(desc) <- c("object", "region", "variable", "description")
  desc
}
###################################
#' @export
import_vds <- function(file){
  set <- utils::read.csv(file, header = F, sep = ",")
  names(set) <- c("object", "region", "set", "variable")


  set
}

#############################
#' @export
import_vdt <- function(file){
  dat <- utils::read.csv(file,
                         header = F,
                         sep = ",",
                         skip =3)
  if(ncol(dat)== 4){
    names(dat) <- c("region",
                    "process",
                    "commodity",
                    "direction")
  }else{
    stop("expecting vdt file to contain 4 columns, but contains different number")
  }
  dat
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
    dplyr::mutate_if(is.character, stringr::str_to_lower) %>%
    # replace all "none" or "-" with NA. ^-$ is regex for full string match to "-"
    dplyr::mutate_if(is.character, stringr::str_replace, "(none)|^-$", NA_character_)




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

append_sets <- function(dat, sets_dat, obj){
  #sets_dat is a long df which includes data for commodity, process, userconstraint.
  #The sets for each will be added as a column to dat

  obj_col <- rlang::enquo(obj)
  # obj_nm <- as_name(obj)

  sets_dat <- sets_dat %>%
    dplyr::filter(object == obj) %>%
    dplyr::rename(!!rlang::sym(obj) := variable) %>% #sym converts obj string to symbol and unquote with !!
    dplyr::select(-object) %>%
    dplyr::filter(is.na(!!obj) == F)

  dat %>%
    dplyr::left_join(sets_dat#, by = vars(obj, region)
                     ) %>%
    dplyr::rename(!!paste(obj, "_set", sep = "") := set)
}

##########################################
#' @export
prep_sector_dat <- function(sector_dat){
  # replace spaces in col names with _
  names(sector_dat) <- sub(" ", "_", names(sector_dat))


  sector_dat <- sector_dat %>%
    #strings to lower
    dplyr::mutate_if(is.character, stringr::str_to_lower)

  #column names to lower
  names(sector_dat) <- stringr::str_to_lower(names(sector_dat))

  sector_dat




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
#' @export
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


###############################
#' Check structure of vd data
#'
#' Check the input data structure is as expected
#'
#' @param filename String. vd filename.
#' @param filetype String "vd_file", "vde_file", "vds_file"
#' @return TRUE if structure matches expected  structure
vd_structure_match_expected <- function(filename, filetype){
  if(filetype == "vd_file"){
    vd_reference_structure <- .vd_reference_structure[[1]]
    vd_header <- scan(filename, skip = 2, what = character(),  nmax = 35 )
    #check vd file structure matches reference structure
    structure_match <- purrr::map2(vd_header, vd_reference_structure, ~identical(.x, .y))
    #line 20 specifies the field size. This might differ depending on different versions of GAMS. So check if all other lines in structure match == TRUE
    sum(unlist(structure_match)[-20]) ==
      length(vd_reference_structure)-1
  }else{

    vd_reference_file <- .vd_reference_structure[[filetype]]
    file <- read.csv(filename)
    identical(ncol(file), ncol(vd_reference_file))
  }
}


####################
# Import data from vdt file and standardise
#
# Import data from vdt data, change to lower case, append the set information.
# The vdt file also includes rows of commodities that are not part of the RES.
# These rows are omitted
#' @param filename_base String. The basename of the veda data excluding the
#' .vd* suffix
#' @param refer_to_package_data Logical. Flag whether the function
#' is calling function from the internal package repository.
#' This should be set to F unless calling the function with the example data
#' @return tibble. columns c("region", "process", "commodity", "direction",
#'                         "process_description", "commodity_description")
#' @example
#'      vdt_07 <-
#'       prep_vdt_data("demos_007",
#'        refer_to_package_data = T)
#' @export

prep_vdt_data <- function(filename_base, refer_to_package_data = F){

  vde_file <- paste(filename_base, ".VDE", sep = "")
  vdt_file <- paste(filename_base, ".VDT", sep = "")

  if(refer_to_package_data == T){
    vdt_file <- system.file("extdata",
                           vdt_file,
                           package = "vedar")
    vde_file <- system.file("extdata",
                            vde_file,
                            package = "vedar")

  }

  vdt <- import_vdt(vdt_file) %>%
    dplyr::mutate_all(stringr::str_to_lower)

  if(vd_structure_match_expected(vde_file, "vde_file")){
    descriptions <- import_vde(vde_file) %>%
      standardise_vd_dat()  %>%
      unique() %>%
      dplyr::group_by(variable,  object, region) %>%
      # in case a variable entry has more than one description
      dplyr::summarise(description = paste(description)) %>%
      dplyr::ungroup()
  }else{
    stop("vde file structure does not match expected")
  }

  #append descriptions
  # to be converted to a function
  vdt <- vdt %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "process") %>%
                       dplyr::select(variable, description) %>%
                       dplyr::rename(process = variable) %>%
                       dplyr::filter(is.na(process) == F) %>%
                       dplyr::distinct(),
                     by = c("process"
                            )) %>%
    dplyr::rename(process_description = description) %>%
    dplyr::left_join(descriptions %>%
                       dplyr::filter(object == "commodity") %>%
                       dplyr::select(variable, description) %>%
                       dplyr::rename(commodity = variable) %>%
                       dplyr::filter(is.na(commodity) == F) %>%
                       dplyr::distinct(),
                     by = c("commodity"
                     )) %>%
    dplyr::rename(commodity_description = description)


  # the vdt file includes commodities that are not present in the
  # user-defined model. Exclude these from the data

  commodities_to_exclude <- c("envi", "nrgi", "demi", "mati",
                              "envo", "nrgo", "demo", "mato")

  vdt <- vdt %>%
    dplyr::filter(commodity %in% commodities_to_exclude ==F)

  vdt



}
