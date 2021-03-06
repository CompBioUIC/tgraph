% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{tgraph_plot}
\alias{tgraph_plot}
\title{Displays a dynamic network at each time step}
\usage{
tgraph_plot(tgraph, layout_coords)
}
\arguments{
\item{tgraph}{a dynamic network}

\item{layout_coords}{igraph layout object compatible with tgraph. This defines
where each vertex is displayed and is used two make two plots easily visually
comparable.}
}
\value{
a single plot consisting of an array of igraph plots, one for each
timestep
}
\description{
Displays a dynamic network at each time step
}
