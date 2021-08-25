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

  dat %>%
    dplyr::select(!!flow_col, !!node_col, attribute) %>%
    tidyr::pivot_wider(values_from = !!node_col,
                       names_from = attribute,
                       values_fn = list) %>%
    tidyr::replace_na(list(var_fin = list(c(0)),
                           var_fout = list(c(0)))
    )%>%
    dplyr::group_by(!!flow_col) %>%
    summarise(edges =map(.x = var_fout,
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
make_plotly_sankey <- function(nodes, edges){
  fig <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",


    node = list(
      label = nodes$process,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),

    link = list(
      source = as.numeric(edges$source),
      target = as.numeric(edges$target),
      value = rep(2, nrow(edges))
    )
  )

  fig <- fig %>%
    plotly::layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )
  fig
}

#######

