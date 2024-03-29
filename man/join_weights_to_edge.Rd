% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_analysis.R
\name{join_weights_to_edge}
\alias{join_weights_to_edge}
\title{Assign value of edge as either var_fin/var_fout}
\usage{
join_weights_to_edge(edge_data, dat, node_col, edge_col, direction = "var_fin")
}
\arguments{
\item{edge_data}{Tibble of source-target edges.}

\item{dat}{Vedar data tibble in long format with var_fin, var_fout data}

\item{node_col}{The column in dat to use for node information.}

\item{edge_col}{The column in dat to use for edge information.}

\item{direction}{String value of attribute either "var_fin" or "var_fout"}
}
\value{
Tibble with source-target pairs for each flow and the pv value
 of the specified direction
}
\description{
Assign value of edge as either var_fin/var_fout
}
\keyword{internal}
