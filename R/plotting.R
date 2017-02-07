require(igraph)
require(gridExtra)


#' Plots a given metric at each time step in a dynamic network
#'
#' @param tgraph a dynamic network
#' @param graph_metric a function that takes an igraph graph and returns a
#' numeric value
#' @return a plot
tgraph_plot_metric <- function(tgraph, graph_metric = avg_degree) {
  vals <- sapply(tgraph, graph_metric)
  plot(vals, type="o")
  title("tgraph Plot Metric")
}


#' Displays a dynamic network at each time step
#'
#' @param tgraph a dynamic network
#' @param layout_coords igraph layout object compatible with tgraph. This defines
#' where each vertex is displayed and is used two make two plots easily visually
#' comparable.
#' @return a single plot consisting of an array of igraph plots, one for each
#' timestep
tgraph_plot <- function(tgraph, layout_coords) {

  if (missing(layout_coords)) layout_coords <- layout.fruchterman.reingold(tgraph[[1]])
  total_t <- length(tgraph)
  plot_grid_dimensions <- rectangularize(total_t)

  # Sets gridExtra dimensions, enabling multi-plot
  par(mfrow=plot_grid_dimensions)

  graph_plotter <- function(t) {
    title_str <- paste('t = ', t)
    plot(tgraph[[t]], layout=layout_coords, main=title_str)
  }

  lapply(1:total_t, graph_plotter)
}
