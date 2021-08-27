#' export
make_res <- function(dat, period_select = NULL, sector_select = NULL,
                     font_size){

  if(length(period_select) > 1){
    stop("RES plotted for single period. Specify single period")
  }
  if(period_select %in% dat$period == F){
    stop("period_select not in period of data")
  }
  if(sector_select %in% dat$sector == F){
    stop("sector_select not in sector of data")
  }
  # RES data are rows with attributes var_fin|var_fout
  dat <- dat %>%
    filter(attribute == "var_fin" | attribute == "var_fout",
           period == period_select,
           sector == sector_select) %>%
    select(attribute, commodity, process,
           commodity_description, process_description,
           pv) %>%
    unique()

  # demand commodities do not have end process.
  # To show on RES, an extra node must be added.
  # Named by commodity
  dat <- dat %>%
    add_missing_nodes("var_fout")


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
    left_join(dat %>%
                filter(attribute == "var_fout") %>%
                select(commodity, commodity_description) %>%
                unique()
    ) %>%
    # at present, the RES only plots connecitions. All values = 1
    mutate(value = 1)

  sn <- make_sankey(nodes, edges,
                    source = "source",
                    target = "target",
                    value = "value",
                    node_label = "process_description",
                    edge_label = "commodity",
                    font_size = font_size)

  sn
}

################################
add_missing_nodes <- function(dat, flow_direction = "var_fout"){
  dat %>%
    dplyr::group_by(commodity) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = map(.x = data,
                             ~add_missing_nodes_subfunction(dat = .x,
                                            direction = flow_direction,
                                            commodity = commodity))) %>%
    tidyr::unnest(cols= c(data)) %>%
    ungroup()

}

################################
add_missing_nodes_subfunction <- function(dat, direction, commodity){
  directions <- c("var_fin", "var_fout")
  if(direction == "var_fout"){
    process_suffix <- "_end_process"
  }else{
    process_suffix <- "_start_process"
  }
  direction_to_check <- directions[which(directions != direction)]
  if(direction %in% (pull(dat, attribute)) &
     (direction_to_check %in% pull(dat, attribute)) == F){
    row_to_add <- tibble(attribute = direction_to_check,
                         process = paste(commodity, process_suffix, sep = ""),
                         process_description = paste(commodity,
                                                     process_suffix, sep = ""),
                         pv = sum(dat$pv),
                         commodity_description = paste(commodity,
                                                       "_demand", sep = "")
    )
    row_to_add
    dat %>%
      bind_rows(row_to_add)
  }else{
    dat
  }
}

################################
make_nodes <- function(dat, node_column){
  node_column <- rlang::enquo(node_column)
  nodes <- tibble::tibble({{node_column}} :=
                            unique(pull(dat, !!node_column))) %>%
    mutate(node_num = row_number() - 1)

  nodes
}


###############################
assign_node_num <- function(dat, nodes){
  dat %>%
    left_join(nodes %>% select(process, node_num))
}


###################################
make_edges <- function(dat, node_col, flow_col){
  node_col <- enquo(node_col)
  flow_col <- enquo(flow_col)

  node_col_numeric <- is.numeric(pull(dat, !!node_col))

out <- dat %>%
    dplyr::select(!!flow_col, !!node_col, attribute) %>%
    tidyr::pivot_wider(values_from = !!node_col,
                       names_from = attribute,
                       values_fn = list) %>%
    dplyr::group_by(!!flow_col) %>%
    summarise(edges = map(.x = var_fout,
                         .y = var_fin,
                         ~expand.grid(source =unlist(.x),
                                      target = unlist(.y)))
    ) %>%
    tidyr::unnest(cols = edges)
}

###########################
make_sankey <- function(nodes, edges, source, target, value,
                        node_label = process_description,
                        edge_label = NULL, font_size = 12){
  source <- enquo(source)
  target <- enquo(target)
  value <- enquo(value)
  node_label <- enquo(node_label)
  edge_label <- enquo(edge_label)

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
                                 Source = rlang::as_string(rlang::ensym(source)),
                                 Target = rlang::as_string(rlang::ensym(target)),
                                 Value = rlang::as_string(rlang::ensym(value)),
                                 NodeID = rlang::as_string(rlang::ensym(node_label)),
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

