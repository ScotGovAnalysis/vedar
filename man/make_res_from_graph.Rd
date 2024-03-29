% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_analysis.R
\name{make_res_from_graph}
\alias{make_res_from_graph}
\title{Create a RES sankey diagram from an igraph object created by make_graph_from_veda_df()}
\usage{
make_res_from_graph(
  g,
  edge_labels = commodity_description,
  sankey_width = NULL,
  sankey_height = NULL,
  font_size = 10
)
}
\arguments{
\item{g}{igraph representation of network with processes as nodes and commodities as edges}

\item{edge_labels}{Edge attribute used for labelling edges.}

\item{sankey_width}{Width (in pixels) of sankey.}

\item{sankey_height}{Height (in pixels) of sankey.}

\item{font_size}{Numeric. Font size for RES labels.}
}
\value{
NetworkD3 Sankey object
}
\description{
Use a graph made from make_graph_from_veda_df() to create a sankey
diagram linking processes with commodities shown as flows.
}
\examples{
 data(demos_001_sector)
 demos_001_sector \%>\%
 filter(period == 2006) \%>\%
 make_graph_from_veda_df() \%>\%
    make_res_from_graph(
             edge_labels = commodity_description,
             font_size = 11)

 data(demos_007_vdt)
 demos_007_vdt \%>\%
     filter(region == "reg1") \%>\%
     make_graph_from_veda_df(input_data_type = "vdt")

}
