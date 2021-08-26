#' export
make_res <- function(dat, period_select = NULL, sector_select = NULL){
  dat <- dat %>%
    filter(attribute == "var_fin" | attribute == "var_fout",
           period == period_select,
           sector == sector_select) %>%
    select(attribute, commodity, process,
           commodity_description, process_description,
           pv) %>%
    unique()

  nodes <- make_nodes(dat, process) %>%
    # append node description
    left_join(dat %>%
                select(process, process_description) %>%
                unique()
                )

  dat <- assign_node_num(dat, nodes)

  edges <- make_edges(dat %>%
                        dplyr::select(node_num, commodity, attribute),
                     node_col = node_num,
                     flow_col = commodity) %>%
    left_join(dat %>%
                filter(attribute == "var_fout") %>%
                select(commodity, commodity_description) %>%
                unique()
    )
}

################################
make_nodes <- function(dat, node_column){
  node_column <- rlang::enquo(node_column)
  nodes <- tibble::tibble(!!node_column := unique(dat %>%
                                           select({{node_column}}))[, 1]
                          ) %>%
    mutate(node_num = row_number())

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

  node_col_numeric <- is.numeric((dat %>% select(!!node_col))[,1])

out <- dat %>%
    dplyr::select(!!flow_col, !!node_col, attribute) %>%
    tidyr::pivot_wider(values_from = !!node_col,
                       names_from = attribute,
                       values_fn = list)
  if(node_col_numeric == TRUE){
      out <- out %>%
        tidyr::replace_na(list(var_fin = list(c(0)),
                           var_fout = list(c(0)))
        )}else{
          out <- out %>%
            tidyr::replace_na(list(var_fin = list(c("0")),
                                   var_fout = list(c("0")))
            )
        }
  out <- out %>%
    dplyr::group_by(!!flow_col) %>%
    summarise(edges = map(.x = var_fout,
                         .y = var_fin,
                         ~expand.grid(source =unlist(.x),
                                      target = unlist(.y)))
    ) %>%
    tidyr::unnest(cols = edges) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(source = dplyr::if_else(source == 0,
                                          paste(!!flow_col, "_source", sep = ""),
                                          source),
                  target = dplyr::if_else(target == 0,
                                          paste(!!flow_col, "_target", sep = ""),
                                          target))
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

