# Functions here all operate on a graph and return a number
library(igraph)

#' the average degree of the graph
#'
#' @param G an igraph graph object
#' @return the average degree of G
#'
avg_degree <- function(G) {
  mean(degree(G))
}
