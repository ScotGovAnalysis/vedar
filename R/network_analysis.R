#'
#' Use the full dataset from prep_data %>% define_sector_*() to create a sankey
#' diagram linking processes with commodities shown as flows. Flow magnitude
#' information is not included. The RES is shown for an individual
#' specified period and sector. The nodes are labelled with process_description.
#' This will be changed to an option
#'
#' @param dat Tibble output from prep_data() \%>\% define_sector_from_*().
#' @param node_labels Column in dat for labelling nodes.
#' @param edge_labels Column in dat for labelling edges.
#' @param sankey_width Width (in pixels) of sankey.
#' @param sankey_height Height (in pixels) of sankey.
#' @param font_size Numeric. Font size for RES labels.
#' @param use_weights Logical. Whether to represent edge weights in Sankey that contains data for a single period. For data containing more than 1 period, magnitudes are unitary.
#' @param input_data_type Character. "vd" or "vdt"
#' @examples
#'  data(demos_001_sector)
#'  demos_001_sector %>%
#'     make_res(period_select = 2005, sector_select = "coal",
#'              node_labels = process_description,
#'              edge_labels = commodity_description,
#'              font_size = 11)
#'
#'  data(demos_001_vdt)
#'  make_res(demos_001_vdt %>%
#'              filter(region == "reg1"),
#'          input_data_type = "vdt")
#'
#' @return NetworkD3 Sankey object
#' @export
make_res <- function(dat,
                       node_labels = process_description,
                       edge_labels = commodity_description,
                       sankey_width = NULL,
                       sankey_height = NULL,
                       font_size = 10,
                       use_weights = T,
                       input_data_type = "vd"){
  node_labels <- rlang::enquo(node_labels)
  edge_labels <- rlang::enquo(edge_labels)

  g <- make_graph_from_veda_df(dat = dat,
                               node_labels = !!node_labels,
                               edge_labels = !!edge_labels,
                               input_data_type = input_data_type)
  # extract the vertex data from graph
  vertices <- igraph::as_data_frame(g, what = "vertices") %>%
    #assign_node_num() which is called below requires
    #    a column "process"
    dplyr::rename(process = name) %>%
    #assign zero-indexed node number
    dplyr::mutate(node_num = dplyr::row_number() - 1)

  # sankey requires edge data to be zero-indexed node numbers
  edges <- igraph::as_data_frame(g, what = "edges") %>%
    # assign node number to "from" nodes
    assign_node_num(vertices, col_to_assign_num = from) %>%
    #replace the character "from" node by the node_num
    dplyr::select(-from) %>%
    dplyr::rename(from = node_num) %>%
    #repeat for the "to" node
    assign_node_num(vertices, col_to_assign_num = to) %>%
    dplyr::select(-to) %>%
    dplyr::rename(to = node_num)
  if(use_weights == F){
    edges$weight = 1
  }

  sn <- make_sankey(vertices, edges,
                    source = from,
                    target = to,
                    value = weight,
                    node_label = names(vertices %>%
                                         dplyr::select(-node_num)),
                    edge_label = edge_labels,
                    sankey_width = sankey_width,
                    sankey_height = sankey_height,
                    font_size = font_size)

  sn
}

#################################################################
#' Create a igraph from a veda dataframe
#'
#' Use the full dataset from prep_data to create an igraph graph. Processes are
#' nodes and commodity flows are represented by edges. When
#' the data is for a single year, the edge weights are the
#' values are determined by the var_fin and var_fout variables
#' of the given process/commodity. Note that since TIMES does
#' not give information of how a given var_fout is split over
#' downstream commodities, an assumption has to be made. Here,
#' the weight is assigned in proportion to the ratio of var_fin
#' variables of downstream commodities linked to the originating
#' process.
#'
#' @param dat Tibble output from prep_data() \%>\% define_sector_from_*().
#' @param node_labels Column in dat for labelling nodes.
#' @param edge_labels Column in dat for labelling edges.
#' @param input_data_type Character. "vd" or "vdt"
#' @examples
#'  data(demos_001_sector)
#'  g <- demos_001_sector %>%
#'     make_graph_from_veda_df(node_labels = process_description,
#'                             edge_labels = commodity_description
#'              )
#'  E(g)
#'  E(g)$weight
#'  E(g)$commodity
#'
#'  # If a singe period selected, the weight is set to the var_fout pv
#'  g_w <- demos_001_sector %>%
#'    filter(period == 2005) %>%
#'     make_graph_from_veda_df(node_labels = process_description,
#'                             edge_labels = commodity_description
#'              )
#'
#'  E(g_w)
#'  E(g_w)$weight
#'  E(g_w)$commodity
#'
#' @return igraph graph object. Edge attributes: commodity, commodity description
#' @export
make_graph_from_veda_df <- function(dat,
                                    node_labels = process_description,
                                    edge_labels = commodity_description,
                                    input_data_type = "vd"
                                    ){

  node_labels <- rlang::enquo(node_labels)
  edge_labels <- rlang::enquo(edge_labels)

  #handle multiple regions
  regions <- unique(dat$region)[is.na(unique(dat$region)) == F]
  if(length(regions) > 1){
    stop("make_graph_from_veda_df requires data from a single region.
         Please filter data before passing to function")
  }
if(input_data_type == "vd"){
  # RES data are rows with attributes var_fin|var_fout
  dat <- dat %>%
    dplyr::filter(attribute == "var_fin" | attribute == "var_fout",
                  ) %>%
    #sum over timeslice and vintage
    dplyr::group_by(attribute, commodity, process, period,
                    commodity_description, process_description) %>%
    dplyr::summarise(pv = sum(pv)) %>%
    dplyr::ungroup() %>%
    dplyr::select(attribute, commodity, process, period,
                  commodity_description, process_description,
                  pv) %>%
    unique()
}else if(input_data_type == "vdt"){
  # RES data are rows with direction "in" or "out"
  dat <- dat %>%
    dplyr::filter(direction == "in" | direction == "out",
    ) %>%
    dplyr::rename(attribute = direction) %>%
    dplyr::mutate(
      attribute = dplyr::case_when(attribute == "in" ~ "var_fin",
                                   attribute == "out" ~ "var_fout"),
      pv = 1 # dummy column
      )
}else{
  stop("specify input_data_type as either 'vd' or 'vdt'")
}


  #  commodities may lack start or end process.
  # To show on RES, an extra node must be added.
  # Named by commodity
  dat <- dat %>%
    add_missing_nodes("var_fout") %>%
    add_missing_nodes("var_fin")


  nodes <- make_nodes(dat, process) %>%
    # append node description
           dplyr::left_join(dat %>%
                    dplyr::select(process, process_description) %>%
                    unique()
    )
  # networkD3 in make_sankey uses zero indexed node numbers. Assign node_num
  # to dat
  dat <- assign_node_num(dat, nodes, col_to_assign_num = process)


  # convert the long var_fin,var_fout data to wide (source-target) edge data
  edges <- make_edges(dat %>%
                        dplyr::select(!!node_labels, commodity, attribute),
                      node_col = !!node_labels,
                      flow_col = commodity)


  #check if only a single period is selected
  # sum the unique numeric values to exclude NAs
  # if only single period included, use edge_weight = pv
  if(input_data_type == "vd"){
  periods <- unique(dat$period)
    if(length(periods[is.na(periods) == F])==1){
      #The assignment of weight is taken from the var_fin or var_fout, dependent
      # on the number of sources and targets.

      edges <- edges %>%
        #append the var_fin of each target by source
        dplyr::group_by(source) %>%
        dplyr::group_nest() %>%
        dplyr::mutate(data = purrr::map(data, ~join_weights_to_edge(.x,
                                                             dat,
                                                             !!node_labels,
                                                             !!edge_labels,
                                                             direction = "var_fin") %>%
                                   dplyr::rename(var_fin = pv))) %>%
        tidyr::unnest(cols = c(data)) %>%
        dplyr::ungroup() %>%
        # count the number of targets for each source and commodity
        dplyr::left_join(edges %>%
                           dplyr::group_by(source, commodity) %>%
                           dplyr::summarise(n_target = length(unique(target))) %>%
                           dplyr::ungroup()) %>%

        #append the var_fout of each source by target
        dplyr::group_by(target) %>%
        dplyr::group_nest() %>%
        dplyr::mutate(data = purrr::map(data,
                                        ~join_weights_to_edge(.x,
                                                             dat,
                                                             !!node_labels,
                                                             !!edge_labels,
                                                             direction =
                                                               "var_fout") %>%
                                   dplyr::rename(var_fout = pv))) %>%
        tidyr::unnest(cols = c(data)) %>%
        dplyr::ungroup() %>%
        # count the number of sources for each target and commodity
        dplyr::left_join(edges %>%
                           dplyr::group_by(target, commodity) %>%
                           dplyr::summarise(n_source = length(unique(source))) %>%
                           dplyr::ungroup()
        )

      # assign weight as var_fout (of source) if there is 1 target
      # and var_fin (of target) if there is 1 source
      # else weight is var_fout of source divided proportionally
      # by var_fins of target
      edges <- edges %>%
        dplyr::mutate(
          total_target_var_fin_by_source =
            unlist(purrr::map2(source, commodity,
                                      ~total_target_var_fin_by_source_function(
                                        edges, .x, .y))),
          weight = dplyr::if_else(n_target == 1,
                                              var_fout,
                                              var_fin
                                              ),
                      # if neither n_target or n_source = 1, weight = NA

          weight = dplyr::if_else(n_source != 1 & n_target != 1,
                                    unlist(purrr::pmap(list(var_fout,
                                              var_fin,
                                              total_target_var_fin_by_source),
                                         ~..1 * ..2/..3)),
                                    weight),
          weight = dplyr::if_else(is.na(var_fout), var_fin, weight),
          weight = dplyr::if_else(is.na(var_fin), var_fout, weight)) %>%
        dplyr::left_join(dat %>%
                           dplyr::select(commodity, commodity_description) %>%
                           dplyr::filter(grepl("(_demand)", commodity_description) == F) %>%
                           unique())

        # test for approximate inequality and return error if derived edge weights
        # don't sum to var_fin or var_fout
        if(all.equal(sum(edges$weight),
                     sum((dat %>% dplyr::filter(attribute == "var_fin"))$pv)) == F){
          stop("Weight of Edges != var_fin")
        }
        if(all.equal(sum(edges$weight),
                     sum((dat %>% dplyr::filter(attribute == "var_fout"))$pv)) == F){
          stop("Weight of Edges != var_fout")
        }
    }else{ #if more than 1 period
      #as above, but set weight = 1
      #assign the commodity description of the var_fout commodity to each edge
      edges <- edges %>%
        dplyr::left_join(dat %>%
                           dplyr::filter(attribute == "var_fout") %>%
                           dplyr::select(commodity, commodity_description) %>%
                           unique()
        ) %>%
        dplyr::mutate(weight = 1)
    }
}else{  #if  data is "vdt"
        # set weight = 1
        #assign the commodity description of the var_fout commodity to each edge
        edges <- edges %>%
          dplyr::left_join(dat %>%
                             dplyr::filter(attribute == "var_fout") %>%
                             dplyr::select(commodity, commodity_description) %>%
                             unique()
          ) %>%
          dplyr::mutate(weight = 1)

    }



  igraph::graph_from_data_frame(edges %>%
                                  dplyr::select(source,
                                                target,
                                                weight,
                                                commodity,
                                                commodity_description
                                                ) %>%
                                  #if several periods have been
                                  # included, edges will be
                                  # duplicated
                                  dplyr::distinct(),
                                directed = T)
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
  #browser()
  dat %>%
    # for each commodity, check whether var_fin and var_fout are specified
    dplyr::group_by(commodity) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(.x = data,
                             #return tibble after bind_rows for creation
                             # of missing row with node information
                             ~add_missing_nodes_subfunction(dat = .x,
                                            direction = flow_direction,
                                            commodity = commodity))) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::ungroup()

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
  if(direction %in% (dplyr::pull(dat, attribute)) &
     (direction_to_check %in% dplyr::pull(dat, attribute)) == F){
      # create the row to add
     row_to_add <- tibble::tibble(attribute = direction_to_check,
                         process = paste(commodity, process_suffix, sep = ""),
                         process_description = paste(commodity,
                                                     process_suffix, sep = ""),
                         #pv is the sum of flows in the opposite direction
                         pv = sum(dat$pv),
                         commodity_description =
                           paste(commodity,"_demand", sep = "")
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
                            unique(dplyr::pull(dat, !!node_column))) %>%
    dplyr::mutate(node_num = dplyr::row_number() - 1)

  nodes
}


###############################
#' Append node numbers to dat
#'
#' @param dat Tibble - long.
#' @param nodes Tibble output of make_nodes.
#' @param col_to_assign_num Column in dat to use for
#'          assigning node number
#' @return Input tibble with node_num appended
#' @keywords internal

assign_node_num <- function(dat, nodes, col_to_assign_num){
  col_to_assign_num <- rlang::enquo(col_to_assign_num)
  dat %>%
    dplyr::left_join(nodes %>%
                       dplyr::select(process, node_num) %>%
                       dplyr::rename(!!col_to_assign_num := process))
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
  #create fns to pass to values_fn pivot_wider
  #https://stackoverflow.com/questions/71423206/using-pivot-wider-in-a-function-with-named-variable-from-function-argument

  fns <- stats::setNames(list(list), deparse(substitute(node_col)))

  node_col <- rlang::enquo(node_col)
  flow_col <- rlang::enquo(flow_col)

  node_col_numeric <- is.numeric(dplyr::pull(dat, !!node_col))

out <- dat %>%
    dplyr::select(!!flow_col, !!node_col, attribute) %>%
    tidyr::pivot_wider(values_from = !!node_col,
                       names_from = attribute,
                       #specify that non-unique values are collapsed to a list
                       #for all variables
                       values_fn = fns) %>%
    dplyr::group_by(!!flow_col) %>%
    dplyr::summarise(edges = purrr::map(.x = var_fout,
                         .y = var_fin,
                         ~expand.grid(source =unlist(.x),
                                      target = unlist(.y)))
    ) %>%
    dplyr::filter(purrr::map(edges, ~nrow(.x))>=1) %>%
    tidyr::unnest(cols = edges)
}

###################################
#' Assign value of edge as either var_fin/var_fout
#'
#'  Assign value of edge as either var_fin/var_fout
#'
#' @param edge_data Tibble of source-target edges.
#' @param dat Vedar data tibble in long format with var_fin, var_fout data
#' @param node_col The column in dat to use for node information.
#' @param edge_col The column in dat to use for edge information.
#' @param direction String value of attribute either "var_fin" or "var_fout"
#' @return Tibble with source-target pairs for each flow and the pv value
#'  of the specified direction
#' @keywords internal
join_weights_to_edge <- function(edge_data,
                                 dat,
                                 node_col,
                                 edge_col,
                                 direction = "var_fin"){

  if(direction == "var_fin"){
    col_label = "target"
    }else if(direction == "var_fout"){
      col_label = "source"
    }else{
    stop("direction must be specified as 'var_fin' or 'var_fout'")
  }
  dplyr::left_join(edge_data, dat %>%
                     dplyr::filter(attribute == direction) %>%
                     dplyr::select({{node_col}}, {{edge_col}}, pv ) %>%
                     dplyr::rename(!!col_label := {{node_col}}))
}

##########################
#' Compute the sum of the var_fin over targets for each source by commodity
#'
#' Compute the sum of the var_fin over targets for each source by commodity
#'
#' @param dat Tibble of source-target edge data
#' @param source_val String value of source
#' @param commodity_val String value of commodity
#' @return numeric total of var_fin over target by source, commodity
#' @keywords internal
total_target_var_fin_by_source_function <- function(dat,  source_val, commodity_val){

   sum((dat %>%
    dplyr::filter(source == source_val,
           commodity == commodity_val))$var_fin)

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
#' @param sankey_width Width (in pixels) of sankey.
#' @param sankey_height Height (in pixels) of sankey.
#' @param font_size Numeric.
#' @return NetworkD3
#' @keywords internal
make_sankey <- function(nodes, edges, source, target, value,
                        node_label = process_description,
                        edge_label = NULL,
                        sankey_width = NULL,
                        sankey_height = NULL,
                        font_size = 12){
  # node_label and edge_label are quosures from make_res. So no need to
  # enquo()
  source <- rlang::enquo(source)
  target <- rlang::enquo(target)
  value <- rlang::enquo(value)



  if(min(c(dplyr::pull(edges, !!source), dplyr::pull(edges, !!target)) != 0)){
    stop("node numbers must be zero indexed")
  }
  if((is.numeric(dplyr::pull(edges, !!source)) &
      is.numeric(dplyr::pull(edges, !!target))) == F){
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
                                   node_label),
                                 fontSize = font_size,
                                 width = sankey_width,
                                 height = sankey_height
  )

  # Add Custom tooltips
  # https://stackoverflow.com/questions/45635970/displaying-edge-information-in-sankey-tooltip/45918897#45918897
  if(is.null(edge_label) == F){
  # add the names back into the links data because sankeyNetwork strips it out
      sn$x$links$name <-  dplyr::pull(edges, !!edge_label)

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
#' Check whether a regular expression is present in nodes (processes) in a list of paths

#' @param node_regex A string regular expression to search for in process nodes.
#' @param path A list of paths
#' @examples
#'  dg <- demos_001_sector %>%
#'            filter(period == 2006) %>%
#'            make_graph_from_veda_df(node_labels = process,
#'                          edge_labels = commodity
#'                            )
#'  all_mincoa1_paths <- all_simple_paths(g, from = "mincoa1")
#'  check_in_paths("(exp)", all_min_coa1_paths)
#' @return logical list
#' @export
check_in_path <- function(node_regex, path){
  purrr::map(path, ~grepl(node_regex, names(.x)))
}


################################
#' Create a RES sankey diagram from an igraph object created by make_graph_from_veda_df()
#'
#' Use a graph made from make_graph_from_veda_df() to create a sankey
#' diagram linking processes with commodities shown as flows.
#'
#' @param g igraph representation of network with processes as nodes and commodities as edges
#' @param edge_labels Edge attribute used for labelling edges.
#' @param sankey_width Width (in pixels) of sankey.
#' @param sankey_height Height (in pixels) of sankey.
#' @param font_size Numeric. Font size for RES labels.
#' @examples
#'  data(demos_001_sector)
#'  demos_001_sector %>%
#'  filter(period == 2006) %>%
#'  make_graph_from_veda_df() %>%
#'     make_res_from_graph(
#'              edge_labels = commodity_description,
#'              font_size = 11)
#'
#'  data(demos_007_vdt)
#'  demos_007_vdt %>%
#'      filter(region == "reg1") %>%
#'      make_graph_from_veda_df(input_data_type = "vdt")
#'
#' @return NetworkD3 Sankey object
#' @export
make_res_from_graph <- function(g,
                                edge_labels =
                                  commodity_description,
                                sankey_width = NULL,
                                sankey_height = NULL,
                                font_size = 10){
  #node_labels <- rlang::enquo(node_labels)
  edge_labels <- rlang::enquo(edge_labels)

# extract the vertex data from graph
  vertices <-
    igraph::as_data_frame(g, what = "vertices") %>%
    #assign_node_num() which is called below requires
    #    a column "process"
    dplyr::rename(process = name) %>%
    #assign zero-indexed node number
    dplyr::mutate(node_num = dplyr::row_number() - 1)

  # sankey requires edge data to be zero-indexed node numbers
  edges <- igraph::as_data_frame(g, what = "edges") %>%
    # assign node number to "from" nodes
    assign_node_num(vertices, col_to_assign_num = from) %>%
    #replace the character "from" node by the node_num
    dplyr::select(-from) %>%
    dplyr::rename(from = node_num) %>%
    #repeat for the "to" node
    assign_node_num(vertices, col_to_assign_num = to) %>%
    dplyr::select(-to) %>%
    dplyr::rename(to = node_num)


  sn <- make_sankey(vertices, edges,
                    source = from,
                    target = to,
                    value = weight,
                    node_label =
                      names(vertices %>%
                               dplyr::select(-node_num)),
                    edge_label = edge_labels,
                    sankey_width = sankey_width,
                    sankey_height = sankey_height,
                    font_size = font_size)

  sn
}
