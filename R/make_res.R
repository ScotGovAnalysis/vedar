#' Create a RES sankey diagram with data for a specified sector and period
#'
#' Use the full dataset from prep_data %>% define_sector_*() to create a sankey
#' diagram linking processes with commodities shown as flows. Flow magnitude
#' information is not included. The RES is shown for an individual
#' specified period and sector. The nodes are labelled with process_description.
#' This will be changed to an option
#'
#' @param dat Tibble output from prep_data() \%>\% define_sector_from_*().
#' @param period_select Numeric. Period for plotting RES.
#' @param sector_select String. Sector for plotting RES.
#' @param region_select String. Region for plotting RES
#' @param node_labels Column in dat for labelling nodes.
#' @param edge_labels Column in dat for labelling edges.
#' @param font_size Numeric. Font size for RES labels.
#' @examples
#'  data(demos_001_sector)
#'  demos_001_sector %>%
#'     make_res(period_select = 2005, sector_select = "coal",
#'              node_labels = process_description,
#'              edge_labels = commodity_description,
#'              font_size = 11)
#' @return NetworkD3 Sankey object
#' @export
make_res <- function(dat, period_select = NULL,
                     sector_select = NULL,
                     region_select = NULL,
                     node_labels = process_description,
                     edge_labels = commodity_description,
                     font_size){
 node_labels <- rlang::enquo(node_labels)
 edge_labels <- rlang::enquo(edge_labels)
  if(length(period_select) > 1){
    stop("RES plotted for single period. Specify single period")
  }
  if(period_select %in% dat$period == F){
    stop("period_select not in period of data")
  }
  if(sector_select %in% dat$sector == F){
    stop("sector_select not in sector of data")
  }
   if(region_select %in% dat$region == F){
     stop("region_select not in regions of data")
   }
  if("sector" %in% names(dat) == F){
    stop("Data missing sector information. Define sectors")
  }
  # RES data are rows with attributes var_fin|var_fout
  dat <- dat %>%
    dplyr::filter(attribute == "var_fin" | attribute == "var_fout",
           period == period_select,
           sector == sector_select,
           region == region_select) %>%
    #sum over timeslice and vintage
    dplyr::group_by(attribute, commodity, process,
                    commodity_description, process_description) %>%
    dplyr::summarise(pv = sum(pv)) %>%
    ungroup() %>%
    dplyr::select(attribute, commodity, process,
           commodity_description, process_description,
           pv) %>%
    unique()

  #  commodities may lack start or end process.
  # To show on RES, an extra node must be added.
  # Named by commodity
  dat <- dat %>%
    add_missing_nodes("var_fout") %>%
    add_missing_nodes("var_fin")


  nodes <- make_nodes(dat, process) %>%
    # append node description
    left_join(dat %>%
                select(process, process_description) %>%
                unique()
                )
  # networkD3 in make_sankey uses zero indexed node numbers. Assign node_num
  # to dat
  dat <- assign_node_num(dat, nodes)


  # convert the long var_fin,var_fout data to wide (source-target) edge data
  edges <- make_edges(dat %>%
                        dplyr::select(node_num, commodity, attribute),
                     node_col = node_num,
                     flow_col = commodity) %>%
    #assign the commodity description of the var_fout commodity to each edge
    dplyr::left_join(dat %>%
                dplyr::filter(attribute == "var_fout") %>%
                dplyr::select(commodity, commodity_description) %>%
                unique()
    ) %>%
    # at present, the RES only plots connecitions. All values = 1
    dplyr::mutate(value = 1)

  sn <- make_sankey(nodes, edges,
                    source = source,
                    target = target,
                    value = value,
                    node_label = node_labels,
                    edge_label = edge_labels,
                    font_size = font_size)

  sn
}

################################

#' Add nodes for flows without a start or end node
#'
#' Search flows labelled by a flow direction for missing counternode and
#' add row data as required. The magnitude of the flow will be the sum of
#' all incoming flows
#'
#' @param flow_direction String. "var_fout" to look for missing end nodes,
#' "var_fin" to look for missing start nodes.
#' @param dat Tibble.
#' @return Input tibble with missing nodes appended.
#' @keywords internal
add_missing_nodes <- function(dat, flow_direction = "var_fout"){
  dat %>%
    # for each commodity, check whether var_fin and var_fout are specified
    dplyr::group_by(commodity) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = map(.x = data,
                             #return tibble after bind_rows for creation
                             # of missing row with node information
                             ~add_missing_nodes_subfunction(dat = .x,
                                            direction = flow_direction,
                                            commodity = commodity))) %>%
    tidyr::unnest(cols = c(data)) %>%
    ungroup()

}

################################

#' Return tibble with missing nodes appended as required
#'
#' Return tibble with missing nodes appended as required
#'
#' @param dat Tibble from nested data by commodity.
#' @param direction String "var_fout" or "var_fin".
#' @param commodity String. Grouping variable in add_missing_nodes.
#' @return Input tibble with missing nodes inserted.
#' @keywords internal
add_missing_nodes_subfunction <- function(dat, direction, commodity){

  directions <- c("var_fin", "var_fout")
  # create string variables for labelling nodes and flows
  if(direction == "var_fout"){
    process_suffix <- "_end_process"
  }else{
    process_suffix <- "_start_process"
  }

  # extract the direction (i.e. attribute) string to check for
  direction_to_check <- directions[which(directions != direction)]

  # if the direction specified is in data and
  #     the direction to check is not in data
  if(direction %in% (pull(dat, attribute)) &
     (direction_to_check %in% pull(dat, attribute)) == F){
      # create the row to add
     row_to_add <- tibble::tibble(attribute = direction_to_check,
                         process = paste(commodity, process_suffix, sep = ""),
                         process_description = paste(commodity,
                                                     process_suffix, sep = ""),
                         #pv is the sum of flows in the opposite direction
                         pv = sum(dat$pv),
                         commodity_description = paste(commodity,
                                                       "_demand", sep = "")
    )
     # return the dat with extra row
      dat %>%
        dplyr::bind_rows(row_to_add)

    }else{
      #return dat
      dat
  }
}

################################

#' Extract all nodes from dat
#'
#' Extract nodes from dat in long format and assign
#' zero indexed node_num
#'
#' @param dat Tibble - long.
#' @param node_column The column in dat for getting the node identities.
#' @return Tibble with node_column and node_num.
#' @keywords internal
make_nodes <- function(dat, node_column){
  node_column <- rlang::enquo(node_column)
  nodes <- tibble::tibble({{node_column}} :=
                            unique(pull(dat, !!node_column))) %>%
    dplyr::mutate(node_num = row_number() - 1)

  nodes
}


###############################
#' Append node numbers to dat
#'
#' @param dat Tibble - long.
#' @param nodes Tibble output of make_nodes.
#' @return Input tibble with node_num appended
#' @keywords internal
assign_node_num <- function(dat, nodes){
  dat %>%
    dplyr::left_join(nodes %>% select(process, node_num))
}


###################################
#' Create tibble of edge data from long tibble
#'
#' Create tibble of edge data from long tibble
#'
#' @param dat Tibble - long.
#' @param node_col The column to use for node information.
#' networkD3 requires node_num.
#' @param flow_col Column that specifies ID of unique flows (commodity).
#' @return Tibble with source-target pairs for each flow.
#' @keywords internal
make_edges <- function(dat, node_col, flow_col){
  node_col <- rlang::enquo(node_col)
  flow_col <- rlang::enquo(flow_col)

  node_col_numeric <- is.numeric(pull(dat, !!node_col))

out <- dat %>%
    dplyr::select(!!flow_col, !!node_col, attribute) %>%
    tidyr::pivot_wider(values_from = !!node_col,
                       names_from = attribute,
                       #specify that non-unique values are collapsed to a list
                       #for all variables
                       values_fn = list(list)) %>%
    dplyr::group_by(!!flow_col) %>%
    summarise(edges = map(.x = var_fout,
                         .y = var_fin,
                         ~expand.grid(source =unlist(.x),
                                      target = unlist(.y)))
    ) %>%
    tidyr::unnest(cols = edges)
}

###########################
#' Make NetworkD3 sankey object
#'
#' Make NetworkD3 Sankey object from node and edge data with tooltips
#'
#' @param nodes Nodes tibble zero indexed.
#' @param edges Edge data with source and target zero-indexed.
#' @param source, target, value Column names in edges tibble.
#' @param edge_label, node_label Enquoted columns in edges tibble. Used for flow
#' tooltip
#' @param font_size Numeric.
#' @return NetworkD3
#' @keywords internal
make_sankey <- function(nodes, edges, source, target, value,
                        node_label = process_description,
                        edge_label = NULL, font_size = 12){
  # node_label and edge_label are quosures from make_res. So no need to
  # enquo()
  source <- rlang::enquo(source)
  target <- rlang::enquo(target)
  value <- rlang::enquo(value)
  # THE CALL TO PRINT IS NEEDED TO EVALUATE node_label. NEED ANOTHER SOLUTION
  t <- print(node_label)


  if(min(c(pull(edges, !!source), pull(edges, !!target)) != 0)){
    stop("node numbers must be zero indexed")
  }
  if((is.numeric(pull(edges, !!source)) &
      is.numeric(pull(edges, !!target))) == F){
    stop("edge source and target must be numeric")
  }

  sn <- networkD3::sankeyNetwork(Links = edges,
                                 Nodes = nodes,
                                 # arguments to sankeyNetwork strings
                                 Source = rlang::as_string(
                                   rlang::ensym(source)),
                                 Target = rlang::as_string(
                                   rlang::ensym(target)),
                                 Value = rlang::as_string(
                                   rlang::ensym(value)),
                                 NodeID = rlang::as_string(
                                   rlang::ensym(node_label)),
                                 fontSize = font_size
  )

  # Add Custom tooltips
  # https://stackoverflow.com/questions/45635970/displaying-edge-information-in-sankey-tooltip/45918897#45918897
  if(is.null(edge_label) == F){
  # add the names back into the links data because sankeyNetwork strips it out
      sn$x$links$name <-  pull(edges, !!edge_label)

      # add onRender JavaScript to set the title to the value of 'name' for each link
      sn <- htmlwidgets::onRender(
        sn,
        '
      function(el, x) {
      d3.selectAll(".link").select("title foreignObject body pre")
      .text(function(d) { return d.name; });
      }
      '
      )
  }

  # display the result
  sn
}

#######

